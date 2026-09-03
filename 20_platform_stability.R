# 20 — Platform onboarding as a threat to the time series.
#
# COMPR.AR did not start with the whole national administration on it. The
# army, the largest buyer in the record, has no awards before 2019; the navy,
# the air force and the border force enter in 2018. The 2017 platform holds 77
# buying organisms and 3.4% territorially anchored awards, against 117 and 55%
# in 2019. Any series that starts in 2017 therefore measures the arrival of
# buyers as well as anything happening in the market.
#
# This script (a) documents the onboarding, (b) recomputes the headline series
# on a balanced panel of buyers present in every year of a stable window, and
# (c) compares the two, so the published window can be chosen on evidence.
#
# Run: Rscript 20_platform_stability.R

source("theme_house.R", chdir = TRUE)

Y0 <- 2019   # first year with the army on the platform
Y1 <- 2025

theil_between_share <- function(firm) {
  d <- firm[firm$monto > 0 & !is.na(firm$monto), ]
  if (nrow(d) < 2) return(NA_real_)
  x <- d$monto; mu <- mean(x); n <- length(x)
  total <- mean((x / mu) * log(x / mu))
  parts <- d |> group_by(provincia) |>
    summarise(ng = n(), mug = mean(monto), .groups = "drop")
  sum((parts$ng / n) * (parts$mug / mu) * log(parts$mug / mu)) / total
}

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo)

# ── (a) onboarding ───────────────────────────────────────────────────────────
por_anio <- adj |> group_by(ejercicio) |>
  summarise(n_adj = n(), organismos = n_distinct(organismo),
            share_anclado = round(mean(anclaje_territorial), 3),
            share_seguridad = round(mean(tipo_organismo == "seguridad_defensa"),
                                    3), .groups = "drop")
write_tab(por_anio, "tab_platform_por_anio.csv")

altas <- adj |> group_by(organismo) |>
  summarise(primer_ejercicio = min(ejercicio), ultimo_ejercicio = max(ejercicio),
            n_adj = n(), tipo = first(tipo_organismo),
            anclaje = first(anclaje_territorial), .groups = "drop") |>
  arrange(primer_ejercicio, desc(n_adj))
write_tab(altas, "tab_platform_altas_organismos.csv")

# ── (b) balanced panel of buyers ─────────────────────────────────────────────
anios <- Y0:Y1
panel <- adj |> filter(between(ejercicio, Y0, Y1)) |>
  distinct(organismo, ejercicio) |> count(organismo) |>
  filter(n == length(anios)) |> pull(organismo)

cobertura <- adj |> filter(between(ejercicio, Y0, Y1)) |>
  summarise(organismos_panel = length(panel),
            organismos_total = n_distinct(organismo),
            share_adj_en_panel = round(mean(organismo %in% panel), 3),
            share_monto_en_panel = round(
              sum(monto[organismo %in% panel & moneda == "ARS"], na.rm = TRUE) /
                sum(monto[moneda == "ARS"], na.rm = TRUE), 3))
write_tab(cobertura, "tab_platform_panel_cobertura.csv")

# ── (c) the series both ways ─────────────────────────────────────────────────
ars <- adj |> filter(!is.na(provincia), moneda == "ARS")
serie <- bind_rows(lapply(2017:Y1, function(yr) {
  todo <- ars |> filter(ejercicio == yr) |>
    group_by(cuit, provincia) |>
    summarise(monto = sum(monto, na.rm = TRUE), .groups = "drop")
  bal <- ars |> filter(ejercicio == yr, organismo %in% panel) |>
    group_by(cuit, provincia) |>
    summarise(monto = sum(monto, na.rm = TRUE), .groups = "drop")
  tibble(ejercicio = yr,
         share_between_todos = theil_between_share(todo),
         n_firmas_todos = nrow(todo),
         share_between_panel = if (nrow(bal) > 1) theil_between_share(bal)
           else NA_real_,
         n_firmas_panel = nrow(bal))
}))
write_tab(serie |> mutate(across(where(is.numeric), ~round(.x, 4))),
          "tab_platform_theil_panel.csv")

# demand composition on the same panel, to see whether the rise in anchored
# demand is also an onboarding effect
dem <- bind_rows(lapply(c("todos", "panel"), function(w) {
  d <- adj |> filter(!is.na(provincia), between(ejercicio, Y0, Y1))
  if (w == "panel") d <- d |> filter(organismo %in% panel)
  d |> mutate(region = region_of(provincia)) |>
    group_by(region, era) |>
    summarise(universo = w, n = n(),
              share_anclado = round(mean(anclaje_territorial), 3),
              .groups = "drop")
}))
write_tab(dem, "tab_platform_demanda_panel.csv")

# does anything of the trend survive on the balanced panel? bootstrap the
# change between the first and last year of the stable window, and test the
# monotone trend across the seven years
set.seed(42)
B <- 1000
firm_of <- function(yr, solo_panel) {
  d <- ars |> filter(ejercicio == yr)
  if (solo_panel) d <- d |> filter(organismo %in% panel)
  d |> group_by(cuit, provincia) |>
    summarise(monto = sum(monto, na.rm = TRUE), .groups = "drop")
}
reps_of <- function(firm) replicate(B, theil_between_share(
  firm[sample.int(nrow(firm), replace = TRUE), ]))

tendencia <- bind_rows(lapply(c(FALSE, TRUE), function(solo) {
  f0 <- firm_of(Y0, solo); f1 <- firm_of(Y1, solo)
  d <- reps_of(f1) - reps_of(f0)
  s <- serie |> filter(between(ejercicio, Y0, Y1))
  v <- if (solo) s$share_between_panel else s$share_between_todos
  ct <- suppressWarnings(cor.test(s$ejercicio, v, method = "spearman"))
  tibble(universo = ifelse(solo, "panel balanceado", "todos los compradores"),
         inicio = theil_between_share(f0), fin = theil_between_share(f1),
         delta = fin - inicio,
         ic_lo = unname(quantile(d, 0.025, na.rm = TRUE)),
         ic_hi = unname(quantile(d, 0.975, na.rm = TRUE)),
         p_delta_menor_igual_0 = mean(d <= 0, na.rm = TRUE),
         rho_tendencia = unname(round(ct$estimate, 3)),
         p_tendencia = signif(ct$p.value, 3))
}))
write_tab(tendencia |> mutate(across(where(is.numeric), ~round(.x, 4))),
          "tab_platform_tendencia.csv")

# and the procedure gap, which is a composition of awards rather than a level
proc <- bind_rows(lapply(c("todos", "panel"), function(w) {
  d <- adj |> filter(!is.na(provincia), between(ejercicio, Y0, Y1))
  if (w == "panel") d <- d |> filter(organismo %in% panel)
  d |> mutate(region = region_of(provincia)) |>
    group_by(region, era) |>
    summarise(universo = w, n = n(),
              share_directa = round(mean(grepl("Directa", procedimiento,
                                               ignore.case = TRUE)), 3),
              .groups = "drop")
})) |> arrange(region, era, universo)
write_tab(proc, "tab_platform_procedimiento_panel.csv")

cat("\nPlataforma por año:\n"); print(por_anio, n = 12)
cat("\nCobertura del panel balanceado", Y0, "-", Y1, ":\n"); print(cobertura)
cat("\nTheil between-provincia: todos los compradores vs panel balanceado:\n")
print(serie |> mutate(across(starts_with("share"), ~round(100 * .x, 1))), n = 12)
cat("\nDemanda anclada por región y era, ambos universos:\n")
print(dem |> arrange(region, era, universo), n = 20)
cat("\nTendencia en la ventana estable:\n"); print(tendencia, width = 140)
cat("\nContratación directa por región y era, ambos universos:\n")
print(proc, n = 20)
