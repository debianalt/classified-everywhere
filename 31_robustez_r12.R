# 31 — Round-12 robustness set (external adversarial review, 29 Aug 2026).
#
# Six blocks, one output table each:
#   (a) equivalence margins for the regional hazard ratios (V22) — the body
#       says region carries no effect; this states what the design can exclude;
#   (b) leading-supplier concentration against the number of suppliers (V23) —
#       the mechanical driver of a leading share is few suppliers, not little
#       purchasing, so the claim must hold against n;
#   (c) the scale ratio within sectors (V24) — if the north-east sells food and
#       the metropolitan core sells equipment, a pooled ratio measures the
#       product mix, not the periphery;
#   (d) leave-one-out for the access gradient — drop every jurisdiction's
#       leading supplier at once and recompute the gradient's correlation;
#   (e) the shift-share decomposition of the fall in direct contracting,
#       canonical — the 22/83 quoted in §7 had no script behind it;
#   (f) negative political control (author's decision) — the gradient against
#       provincial alignment with the national government and against budget
#       transparency (ITPP).
#
# ANONYMISATION (project rule, 27 Aug): leading suppliers are dropped by
# identifier internally; no CUIT and no name reaches any output.
#
# Run: Rscript 31_robustez_r12.R

source("theme_house.R", chdir = TRUE)
suppressPackageStartupMessages({library(survival); library(broom)})

todo <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia))
adj <- todo |> filter(between(ejercicio, WIN0, WIN1))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))

# ── (a) equivalence for the regional hazard ratios ───────────────────────────
# Spell construction copied from 17_survival_km_cox.R (model 4, the fully
# stratified specification the body quotes).
primera_vez <- todo |> group_by(cuit) |>
  summarise(primera = min(ejercicio), .groups = "drop")
spell <- adj |>
  group_by(cuit, provincia) |>
  summarise(entrada = min(ejercicio), fin = max(ejercicio), .groups = "drop") |>
  inner_join(primera_vez, by = "cuit") |>
  filter(primera >= WIN0, entrada <= WIN1 - 1) |>
  mutate(dur = fin - entrada + 1,
         evento = as.integer(fin < WIN1),
         region = factor(region_of(provincia), levels = REGION_LEVELS))
entry_vars <- adj |>
  semi_join(spell, by = "cuit") |>
  inner_join(spell |> select(cuit, entrada), by = "cuit") |>
  filter(ejercicio == entrada) |>
  group_by(cuit) |>
  summarise(territorial_entrada = mean(anclaje_territorial) > 0.5,
            n_adj_entrada = n(),
            rubro_entrada = names(sort(table(rubro_principal),
                                       decreasing = TRUE))[1],
            .groups = "drop")
top_rubros <- entry_vars |> count(rubro_entrada, sort = TRUE) |>
  slice_head(n = 8) |> pull(rubro_entrada)
d <- spell |>
  inner_join(entry_vars, by = "cuit") |>
  left_join(master |> select(cuit, personeria), by = "cuit") |>
  mutate(
    territorial = factor(ifelse(territorial_entrada, "anclado", "otro"),
                         levels = c("otro", "anclado")),
    forma = factor(dplyr::case_match(
      personeria, "Persona Fisica" ~ "PersFisica",
      "Sociedad Anonima" ~ "SA",
      "Sociedad Responsabilidad Limitada" ~ "SRL",
      .default = "OtraForma"), levels = c("SA", "SRL", "PersFisica",
                                          "OtraForma")),
    rubro = factor(ifelse(rubro_entrada %in% top_rubros,
                          substr(rubro_entrada, 1, 18), "OTROS")),
    intensidad_banda = cut(n_adj_entrada, c(0, 1, 5, 20, Inf),
                           labels = c("1", "2-5", "6-20", "20+"))) |>
  filter(!is.na(forma))

cox4 <- coxph(Surv(dur, evento) ~ region + territorial + forma +
                strata(rubro) + strata(entrada) + strata(intensidad_banda),
              data = d)
reg_terms <- grep("^region", names(coef(cox4)), value = TRUE)
ci90 <- confint(cox4, parm = reg_terms, level = 0.90)
ci95 <- confint(cox4, parm = reg_terms, level = 0.95)
# TOST logic: equivalence at margin m is declared when the 90% CI for the
# log-HR lies inside [-log(m), +log(m)]; the tightest margin that both
# regional terms satisfy is the bound the design supports.
margen <- exp(max(abs(ci90)))
tost <- tibble(
  termino = reg_terms,
  HR = round(exp(coef(cox4)[reg_terms]), 3),
  ic90_lo = round(exp(ci90[, 1]), 3), ic90_hi = round(exp(ci90[, 2]), 3),
  ic95_lo = round(exp(ci95[, 1]), 3), ic95_hi = round(exp(ci95[, 2]), 3)) |>
  mutate(margen_equivalencia_conjunto = round(margen, 3))
write_tab(tost, "tab_tost_region.csv")
cat(sprintf("\n(a) margen de equivalencia conjunto (TOST 90%%): HR dentro de [%.3f, %.3f]\n",
            1 / margen, margen))

# ── (b) concentration against the number of suppliers ────────────────────────
conc <- read.csv("tables/tab_concentracion.csv")
conc <- conc |>
  mutate(share_norm = (share_prov_lider - 1 / proveedores) /
           (1 - 1 / proveedores))
rho_n <- cor.test(conc$share_prov_lider, conc$proveedores,
                  method = "spearman", exact = FALSE)
rho_adj <- cor.test(conc$share_prov_lider, conc$n_adj,
                    method = "spearman", exact = FALSE)
rho_norm_n <- cor.test(conc$share_norm, conc$proveedores,
                       method = "spearman", exact = FALSE)
conc_out <- tibble(
  prueba = c("share lider vs numero de proveedores",
             "share lider vs numero de adjudicaciones",
             "share normalizado vs numero de proveedores"),
  rho = round(c(rho_n$estimate, rho_adj$estimate, rho_norm_n$estimate), 3),
  p = signif(c(rho_n$p.value, rho_adj$p.value, rho_norm_n$p.value), 3))
write_tab(conc_out, "tab_concentracion_vs_n.csv")
write_tab(conc |> select(provincia, proveedores, n_adj, share_prov_lider,
                         share_norm) |>
            mutate(share_norm = round(share_norm, 3)) |>
            arrange(desc(share_norm)),
          "tab_concentracion_normalizada.csv")
cat("\n(b) concentracion:\n"); print(as.data.frame(conc_out))

# ── (c) scale within sector ──────────────────────────────────────────────────
ipc <- read.csv(file.path(DIR_PROC, "ipc_anual.csv"))
tc <- read.csv(file.path(DIR_PROC, "tipo_cambio_anual.csv"))
w <- adj |> filter(moneda == "ARS") |>
  left_join(ipc |> select(ejercicio = anio, deflactor_2024), by = "ejercicio") |>
  left_join(tc |> select(ejercicio = anio, tc_oficial_prom), by = "ejercicio") |>
  mutate(real = monto * deflactor_2024,
         usd = monto / tc_oficial_prom,
         forma = ifelse(personeria == "Persona Fisica", "Persona fisica",
                        ifelse(is.na(personeria), NA, "Sociedad")),
         region = region_of(provincia)) |>
  filter(!is.na(forma), region %in% c("NEA", "Metro"))
MIN_CELDA <- 30
sec <- w |> group_by(region, forma, rubro_principal) |>
  summarise(n_adj = n(),
            mediana_real = round(median(real)),
            mediana_usd = round(median(usd)), .groups = "drop") |>
  filter(n_adj >= MIN_CELDA)
sec_wide <- sec |>
  tidyr::pivot_wider(names_from = region,
                     values_from = c(n_adj, mediana_real, mediana_usd)) |>
  filter(!is.na(mediana_usd_NEA), !is.na(mediana_usd_Metro)) |>
  mutate(ratio_usd = round(mediana_usd_NEA / mediana_usd_Metro, 3))
write_tab(sec_wide, "tab_escala_por_sector.csv")
# composition-adjusted ratio: NEA medians weighted by the METROPOLITAN sector
# mix, so the product mix is the metropolitan one on both sides
aj <- sec_wide |>
  group_by(forma) |>
  summarise(
    sectores = n(),
    ratio_bruto = round(sum(mediana_usd_NEA * n_adj_NEA) /
                          sum(mediana_usd_Metro * n_adj_NEA), 3),
    ratio_ajustado = round(sum(mediana_usd_NEA * n_adj_Metro) /
                             sum(mediana_usd_Metro * n_adj_Metro), 3),
    mediana_ratios = round(median(ratio_usd), 3), .groups = "drop")
write_tab(aj, "tab_escala_por_sector_resumen.csv")
cat("\n(c) escala dentro de sector:\n"); print(as.data.frame(aj))
cat("   ratios por sector (Sociedad):\n")
print(as.data.frame(sec_wide |> filter(forma == "Sociedad") |>
                      select(rubro_principal, n_adj_NEA, n_adj_Metro,
                             ratio_usd)))

# ── (d) leave-one-out for the gradient ───────────────────────────────────────
lider <- adj |> count(provincia, cuit, name = "n_l") |>
  group_by(provincia) |> slice_max(n_l, n = 1, with_ties = FALSE) |> ungroup()
sin_lider <- adj |> anti_join(lider |> select(provincia, cuit),
                              by = c("provincia", "cuit"))
g_full <- adj |> group_by(provincia) |>
  summarise(anclado = mean(anclaje_territorial), .groups = "drop") |>
  left_join(adj |> distinct(cuit, provincia) |>
              left_join(master |> select(cuit, personeria), by = "cuit") |>
              filter(!is.na(personeria)) |> group_by(provincia) |>
              summarise(pf = mean(personeria == "Persona Fisica"),
                        .groups = "drop"), by = "provincia")
g_loo <- sin_lider |> group_by(provincia) |>
  summarise(anclado_loo = mean(anclaje_territorial), .groups = "drop") |>
  left_join(sin_lider |> distinct(cuit, provincia) |>
              left_join(master |> select(cuit, personeria), by = "cuit") |>
              filter(!is.na(personeria)) |> group_by(provincia) |>
              summarise(pf_loo = mean(personeria == "Persona Fisica"),
                        .groups = "drop"), by = "provincia")
g <- g_full |> left_join(g_loo, by = "provincia")
rho_obs <- cor.test(g$anclado, g$pf, method = "spearman", exact = FALSE)
rho_loo <- cor.test(g$anclado_loo, g$pf_loo, method = "spearman", exact = FALSE)
loo_out <- tibble(
  medida = c("rho observado", "rho sin el proveedor lider de cada jurisdiccion"),
  rho = round(c(rho_obs$estimate, rho_loo$estimate), 3),
  p = signif(c(rho_obs$p.value, rho_loo$p.value), 3))
write_tab(loo_out, "tab_gradiente_loo.csv")
write_tab(g |> mutate(across(where(is.numeric), ~round(.x, 3))),
          "tab_gradiente_loo_detalle.csv")
cat("\n(d) leave-one-out:\n"); print(as.data.frame(loo_out))

# ── (e) shift-share of the fall in direct contracting ────────────────────────
# Two-fold decomposition with period-mean weights: exactly additive, no
# interaction remainder. Types: anchored vs non-anchored organs.
ss <- adj |>
  filter(ejercicio %in% c(WIN0, WIN1)) |>
  mutate(directa = grepl("Directa", procedimiento, ignore.case = TRUE),
         tipo = ifelse(anclaje_territorial, "anclado", "no_anclado")) |>
  group_by(ejercicio, tipo) |>
  summarise(n = n(), s = mean(directa), .groups = "drop") |>
  group_by(ejercicio) |> mutate(w = n / sum(n)) |> ungroup()
tot <- ss |> group_by(ejercicio) |>
  summarise(directa = sum(w * s), .groups = "drop")
s0 <- ss |> filter(ejercicio == WIN0); s1 <- ss |> filter(ejercicio == WIN1)
stopifnot(identical(s0$tipo, s1$tipo))
delta <- sum(s1$w * s1$s) - sum(s0$w * s0$s)
between <- sum((s1$w - s0$w) * (s0$s + s1$s) / 2)
within  <- sum(((s0$w + s1$w) / 2) * (s1$s - s0$s))
stopifnot(abs(between + within - delta) < 1e-12)
ss_out <- tibble(
  componente = c("caida total (pp)", "composicion hacia anclados (pp)",
                 "estrechamiento dentro de cada tipo (pp)",
                 "share composicion (%)", "share estrechamiento (%)"),
  valor = round(c(delta, between, within,
                  100 * between / delta, 100 * within / delta), 4) * c(100, 100, 100, 1, 1))
write_tab(ss_out, "tab_shiftshare_directa.csv")
write_tab(ss |> mutate(across(where(is.numeric), ~round(.x, 4))),
          "tab_shiftshare_directa_base.csv")
cat("\n(e) shift-share directa", WIN0, "->", WIN1, ":\n")
print(as.data.frame(ss_out))
cat(sprintf("   nivel: %.1f%% -> %.1f%%\n", 100 * tot$directa[1],
            100 * tot$directa[2]))

# ── (f) negative political control ───────────────────────────────────────────
cov <- read_parquet(file.path(DIR_PROC, "covariates_provincia_anio.parquet"))
pol <- cov |> filter(between(ejercicio, WIN0, WIN1)) |>
  group_by(provincia) |>
  summarise(alineado_medio = mean(alineado, na.rm = TRUE),
            itpp_medio = mean(itpp, na.rm = TRUE), .groups = "drop")
gp <- g_full |> left_join(pol, by = "provincia")
tests <- list(
  c("anclado vs alineado", "anclado", "alineado_medio"),
  c("persona fisica vs alineado", "pf", "alineado_medio"),
  c("anclado vs ITPP", "anclado", "itpp_medio"),
  c("persona fisica vs ITPP", "pf", "itpp_medio"))
neg <- bind_rows(lapply(tests, function(v) {
  ct <- cor.test(gp[[v[2]]], gp[[v[3]]], method = "spearman", exact = FALSE)
  tibble(prueba = v[1], n = sum(complete.cases(gp[, c(v[2], v[3])])),
         rho = round(unname(ct$estimate), 3), p = signif(ct$p.value, 3))
}))
write_tab(neg, "tab_control_politico.csv")
cat("\n(f) control negativo politico:\n"); print(as.data.frame(neg))

# sensitivity: the five debatable Milei-era codings, recoded as a block in the
# direction that would STRENGTHEN an alignment story (checked against the
# public record 29-08; see covariates/SOURCES.md)
recode <- cov |> filter(era == "milei") |>
  distinct(provincia, alineado) |>
  mutate(alineado_alt = dplyr::case_match(
    provincia,
    "Entre Ríos" ~ 0.5, "Mendoza" ~ 0.5,          # 1 -> 0.5
    "Misiones" ~ 1, "Salta" ~ 1, "Tucumán" ~ 1,   # 0.5 -> 1
    .default = NA) |> coalesce(alineado))
pol_alt <- cov |> filter(between(ejercicio, WIN0, WIN1)) |>
  left_join(recode |> select(provincia, alineado_alt), by = "provincia") |>
  mutate(alineado2 = ifelse(era == "milei", alineado_alt, alineado)) |>
  group_by(provincia) |>
  summarise(alineado_alt = mean(alineado2, na.rm = TRUE), .groups = "drop")
gp2 <- g_full |> left_join(pol_alt, by = "provincia")
sens <- bind_rows(lapply(list(c("anclado vs alineado (recodificado)", "anclado"),
                              c("persona fisica vs alineado (recodificado)", "pf")),
                         function(v) {
  ct <- cor.test(gp2[[v[2]]], gp2$alineado_alt, method = "spearman",
                 exact = FALSE)
  tibble(prueba = v[1], rho = round(unname(ct$estimate), 3),
         p = signif(ct$p.value, 3))
}))
write_tab(sens, "tab_control_politico_sens.csv")
cat("\n(f bis) sensibilidad del coding:\n"); print(as.data.frame(sens))
