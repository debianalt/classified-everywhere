# 17 — Supplier survival with right censoring: Kaplan-Meier curves and a Cox
# model (external review 4.4). The cohort tables of 05/12 compare presidential
# windows and cannot separate exit from the closing of the observation window;
# this script makes the censoring explicit.
#
# Spell definition: entry = year of first award, end = year of last award,
# duration = end - entry + 1 (years from entry to the last observed award).
# Observation closes in 2025 (2026 is a partial year and is dropped), so a
# supplier whose last award falls in 2025 is right-censored.
# Every covariate is measured at cohort entry (first-year awards only).
#
# Run: Rscript 17_survival_km_cox.R

source("theme_house.R", chdir = TRUE)
suppressPackageStartupMessages({library(survival); library(broom)})

Y_END <- WIN1

todo <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia))
adj <- todo |> filter(between(ejercicio, WIN0, Y_END))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))

# true entrants only: a supplier already winning before the window opens would
# be left-truncated, its spell counted from a date that is not its entry
primera_vez <- todo |> group_by(cuit) |>
  summarise(primera = min(ejercicio), .groups = "drop")

spell <- adj |>
  group_by(cuit, provincia) |>
  summarise(entrada = min(ejercicio), fin = max(ejercicio), .groups = "drop") |>
  inner_join(primera_vez, by = "cuit") |>
  filter(primera >= WIN0, entrada <= Y_END - 1) |>
  mutate(dur = fin - entrada + 1,
         evento = as.integer(fin < Y_END),
         region = factor(region_of(provincia), levels = REGION_LEVELS))

# covariates at entry: client anchoring, legal form, entry-year rubro
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
    entrada_c = entrada - WIN0,
    intensidad_entrada = log1p(n_adj_entrada)) |>
  filter(!is.na(forma))

cat(sprintf("N = %d proveedores | eventos (salidas) %d | censurados %d\n",
            nrow(d), sum(d$evento), sum(1 - d$evento)))

# ── Kaplan-Meier ─────────────────────────────────────────────────────────────
km_tab <- function(fit, etiqueta) {
  s <- summary(fit, times = 1:8, extend = TRUE)
  tibble(modelo = etiqueta,
         grupo = if (is.null(s$strata)) "todos" else as.character(s$strata),
         t = s$time, n_risk = s$n.risk, n_event = s$n.event,
         surv = round(s$surv, 3), lo = round(s$lower, 3),
         hi = round(s$upper, 3))
}
km_region <- survfit(Surv(dur, evento) ~ region, data = d)
km_terr <- survfit(Surv(dur, evento) ~ territorial, data = d)
km_both <- survfit(Surv(dur, evento) ~ region + territorial, data = d)
km <- bind_rows(km_tab(km_region, "region"), km_tab(km_terr, "territorial"),
                km_tab(km_both, "region x territorial"))
write_tab(km, "tab_km_survival.csv")

lr_region <- survdiff(Surv(dur, evento) ~ region, data = d)
lr_terr <- survdiff(Surv(dur, evento) ~ territorial, data = d)
lr <- tibble(
  prueba = c("log-rank region", "log-rank territorial en entrada"),
  chisq = round(c(lr_region$chisq, lr_terr$chisq), 2),
  gl = c(length(lr_region$n) - 1, length(lr_terr$n) - 1)) |>
  mutate(p = signif(pchisq(chisq, gl, lower.tail = FALSE), 3))
write_tab(lr, "tab_km_logrank.csv")

# ── Cox ──────────────────────────────────────────────────────────────────────
cox1 <- coxph(Surv(dur, evento) ~ region + territorial + forma + rubro +
                entrada_c + intensidad_entrada, data = d)
cox2 <- coxph(Surv(dur, evento) ~ region * territorial + forma + rubro +
                entrada_c + intensidad_entrada, data = d)
# cox.zph flags the controls (sector, entry year, entry intensity), not the
# terms of interest; model 3 removes the assumption for them by stratifying on
# sector and entry cohort and by banding entry intensity
d <- d |> mutate(intensidad_banda = cut(n_adj_entrada, c(0, 1, 5, 20, Inf),
                                        labels = c("1", "2-5", "6-20", "20+")))
cox3 <- coxph(Surv(dur, evento) ~ region + territorial + forma +
                intensidad_banda + strata(rubro) + strata(entrada), data = d)
# entry intensity keeps violating proportionality, so model 4 stratifies on it
# too: the terms of interest are then compared within cells that share sector,
# entry cohort and initial award volume
cox4 <- coxph(Surv(dur, evento) ~ region + territorial + forma +
                strata(rubro) + strata(entrada) + strata(intensidad_banda),
              data = d)
# and because the anchoring effect itself is not exactly constant, model 5
# reports it separately for the first three years and afterwards
ds <- survSplit(Surv(dur, evento) ~ ., data = d, cut = 3, episode = "periodo")
ds$periodo <- factor(ds$periodo, labels = c("anios 1-3", "anios 4+"))
cox5 <- coxph(Surv(tstart, dur, evento) ~ region + territorial * periodo +
                forma + strata(rubro) + strata(entrada) +
                strata(intensidad_banda), data = ds)

tidy_cox <- function(fit, etiqueta) {
  tidy(fit, exponentiate = TRUE, conf.int = TRUE) |>
    transmute(modelo = etiqueta, termino = term, HR = round(estimate, 3),
              ic_lo = round(conf.low, 3), ic_hi = round(conf.high, 3),
              z = round(statistic, 2), p = signif(p.value, 3))
}
cox_tab <- bind_rows(tidy_cox(cox1, "aditivo"),
                     tidy_cox(cox2, "con interaccion region x territorial"),
                     tidy_cox(cox3, "estratificado por rubro y cohorte"),
                     tidy_cox(cox4, "estratificado + intensidad"),
                     tidy_cox(cox5, "efecto por periodo"))
write_tab(cox_tab, "tab_cox_model.csv")

ph_of <- function(fit, etiqueta) {
  as.data.frame(cox.zph(fit)$table) |> tibble::rownames_to_column("termino") |>
    transmute(modelo = etiqueta, termino, chisq = round(chisq, 2), gl = df,
              p = signif(p, 3))
}
ph_tab <- bind_rows(ph_of(cox1, "aditivo"),
                    ph_of(cox3, "estratificado por rubro y cohorte"),
                    ph_of(cox4, "estratificado + intensidad"))
write_tab(ph_tab, "tab_cox_ph_test.csv")

modelos <- list(aditivo = cox1, `con interaccion` = cox2,
                `estratificado` = cox3, `estratificado + intensidad` = cox4,
                `efecto por periodo` = cox5)
fit_stats <- tibble(
  modelo = names(modelos),
  n = sapply(modelos, function(f) f$n),
  eventos = sapply(modelos, function(f) f$nevent),
  concordancia = round(sapply(modelos,
                              function(f) summary(f)$concordance[1]), 3),
  AIC = round(sapply(modelos, AIC), 1),
  lrt_p_interaccion = c(NA, signif(anova(cox1, cox2)$`Pr(>|Chi|)`[2], 3),
                        NA, NA, NA))
write_tab(fit_stats, "tab_cox_fit.csv")

# ── Supplementary figure ─────────────────────────────────────────────────────
curvas <- km |> filter(modelo == "region x territorial") |>
  mutate(codigo = sub(".*region=([^,]+),.*", "\\1", grupo),
         region = factor(REGION_LABELS[codigo], levels = REGION_LABELS),
         territorial = ifelse(grepl("territorial=anclado", grupo),
                              "Territorially anchored client at entry",
                              "Other client at entry"))
# Six curves in one panel need six distinguishable line colours, and the
# journal prints in greyscale. Faceting by region leaves two curves per panel,
# which linetype alone separates unambiguously in black and white.
p <- ggplot(curvas, aes(t, 100 * surv, linetype = territorial)) +
  geom_step(linewidth = 0.5, colour = "#1a1a1a") +
  facet_wrap(~region, nrow = 1) +
  scale_linetype_manual(values = c(2, 1), name = NULL) +
  scale_x_continuous(breaks = 1:8) +
  labs(x = "Years since first award",
       y = "Suppliers still winning (%)") +
  theme_house() +
  guides(linetype = guide_legend(nrow = 1))
pub(p, "S3", 3.4)

cat("\nLog-rank:\n"); print(lr)
cat("\nCox, términos de interés en cada especificación:\n")
print(cox_tab |> filter(grepl("region|territorial|forma", termino)), n = 40)
cat("\nAjuste:\n"); print(fit_stats)
cat("\nProporcionalidad (cox.zph):\n"); print(ph_tab)
