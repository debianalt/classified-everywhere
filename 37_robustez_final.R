# 37 — The three referee-round checks, fired in advance (30 Aug 2026).
#
#  (a) exit-definition sensitivity: the generalist review called last-award
#      exit "fatal"; here suppliers whose last award falls in 2024-25 are
#      right-censored (exit = two or more silent years inside the window)
#      and the anchoring HR is reported under both definitions;
#  (b) the 24-jurisdiction gradient with controls: anchored demand ->
#      natural persons conditioned on company density (log stock) and
#      budget transparency (ITPP);
#  (c) a constant panel of SUPPLIERS (winners in every year): does the
#      between-province Theil share move on it?
#
# Run: Rscript 37_robustez_final.R

source("theme_house.R", chdir = TRUE)
suppressPackageStartupMessages({library(survival); library(broom)})

todo <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia))
adj <- todo |> filter(between(ejercicio, WIN0, WIN1))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))

# ── (a) exit sensitivity ─────────────────────────────────────────────────────
primera_vez <- todo |> group_by(cuit) |>
  summarise(primera = min(ejercicio), .groups = "drop")
spell <- adj |>
  group_by(cuit, provincia) |>
  summarise(entrada = min(ejercicio), fin = max(ejercicio), .groups = "drop") |>
  inner_join(primera_vez, by = "cuit") |>
  filter(primera >= WIN0, entrada <= WIN1 - 1) |>
  mutate(dur = fin - entrada + 1,
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
                           labels = c("1", "2-5", "6-20", "20+")),
    evento_pub = as.integer(fin < WIN1),          # published definition
    evento_2y = as.integer(fin <= WIN1 - 2)) |>   # >=2 silent years = exit
  filter(!is.na(forma))

fit_of <- function(ev) {
  f <- coxph(as.formula(paste0("Surv(dur, ", ev, ") ~ region + territorial + ",
                               "forma + strata(rubro) + strata(entrada) + ",
                               "strata(intensidad_banda)")), data = d)
  tidy(f, exponentiate = TRUE, conf.int = TRUE) |>
    filter(term == "territorialanclado") |>
    transmute(definicion = ev, HR = round(estimate, 3),
              ic_lo = round(conf.low, 3), ic_hi = round(conf.high, 3),
              eventos = sum(d[[ev]]))
}
sens <- bind_rows(fit_of("evento_pub"), fit_of("evento_2y"))
write_tab(sens, "tab_salida_sensibilidad.csv")
cat("(a) HR de anclaje bajo ambas definiciones de salida:\n")
print(as.data.frame(sens), row.names = FALSE)

# ── (b) gradient with controls ───────────────────────────────────────────────
grad <- read.csv("tables/tab_gradiente_provincias.csv")
den <- read_parquet(file.path(DIR_PROC, "denominators_provincia_anio.parquet")) |>
  filter(ejercicio == 2025) |> select(provincia, stock_sociedades)
cov <- read_parquet(file.path(DIR_PROC, "covariates_provincia_anio.parquet")) |>
  filter(between(ejercicio, WIN0, WIN1)) |>
  group_by(provincia) |>
  summarise(itpp = mean(itpp, na.rm = TRUE), .groups = "drop")
g <- grad |> left_join(den, by = "provincia") |> left_join(cov, by = "provincia") |>
  mutate(lstock = log(stock_sociedades))
stopifnot(nrow(g) == 24, !any(is.na(g$lstock)))
# the unconditioned coefficient here must reproduce the published one; if the
# gradient table is ever written rounded again, this fails instead of quietly
# publishing a different number (script 21 writes it at analysis precision)
rho_crudo <- cor(g$share_anclado, g$share_persona_fisica, method = "spearman")
stopifnot(abs(rho_crudo - 0.637) < 0.001)

# partial rank correlation via residuals of rank regressions
prank <- function(y, x, z) {
  ry <- resid(lm(rank(y) ~ z)); rx <- resid(lm(rank(x) ~ z))
  unname(cor(ry, rx))
}
ctl <- tibble(
  especificacion = c("sin controles (Spearman)",
                     "parcial | log stock de sociedades",
                     "parcial | log stock + ITPP",
                     "OLS: coef de anclado (shares), con log stock + ITPP"),
  valor = round(c(
    rho_crudo,
    prank(g$share_persona_fisica, g$share_anclado, cbind(g$lstock)),
    prank(g$share_persona_fisica, g$share_anclado, cbind(g$lstock, g$itpp)),
    coef(lm(share_persona_fisica ~ share_anclado + lstock + itpp, g))[2]), 3))
write_tab(ctl, "tab_gradiente_controles.csv")
cat("\n(b) gradiente con controles (24 jurisdicciones):\n")
print(as.data.frame(ctl), row.names = FALSE)

# ── (c) constant panel of suppliers ──────────────────────────────────────────
constantes <- adj |> distinct(cuit, ejercicio) |> count(cuit) |>
  filter(n == WIN1 - WIN0 + 1) |> pull(cuit)
cat(sprintf("\n(c) proveedores presentes los %d anios: %s\n",
            WIN1 - WIN0 + 1, format(length(constantes), big.mark = ",")))
ipc <- read.csv(file.path(DIR_PROC, "ipc_anual.csv"))
wp <- adj |> filter(cuit %in% constantes, moneda == "ARS") |>
  left_join(ipc |> select(ejercicio = anio, deflactor_2024), by = "ejercicio") |>
  mutate(real = monto * deflactor_2024)
theil_t <- function(x) {
  x <- x[x > 0 & !is.na(x)]; if (length(x) < 2) return(NA_real_)
  mu <- mean(x); mean((x / mu) * log(x / mu))
}
serie <- wp |> group_by(ejercicio, cuit, provincia) |>
  summarise(m = sum(real), .groups = "drop") |>
  group_by(ejercicio) |>
  group_modify(~{
    d <- .x; mu <- mean(d$m); n <- nrow(d)
    parts <- d |> group_by(provincia) |>
      summarise(ng = n(), mug = mean(m), .groups = "drop")
    between <- sum((parts$ng / n) * (parts$mug / mu) * log(parts$mug / mu))
    tibble(n_prov = length(unique(d$provincia)), n_sup = n,
           theil_total = theil_t(d$m), between = between,
           share_between = between / theil_t(d$m))
  }) |> ungroup() |>
  mutate(across(where(is.numeric), ~round(.x, 4)))
write_tab(serie, "tab_theil_panel_proveedores.csv")
print(as.data.frame(serie |> select(ejercicio, n_sup, share_between)),
      row.names = FALSE)
