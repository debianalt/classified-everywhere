# 05 — Supplier demography and survival (canonical R port of
# 05_demography_survival.py; same outputs).
# Run: Rscript 05_demography_survival.R

source("theme_house.R", chdir = TRUE)

ERA_OF_YEAR <- function(y) ifelse(y <= 2019, "macri",
                                  ifelse(y <= 2023, "fernandez", "milei"))

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_clean.parquet")) |>
  filter(es_nuevo, !is.na(provincia))

firm_years <- adj |>
  count(cuit, provincia, ejercicio, name = "n") |>
  mutate(region = region_of(provincia))

life <- firm_years |>
  group_by(cuit, provincia, region) |>
  summarise(entrada = min(ejercicio), salida = max(ejercicio),
            anios_activo = n_distinct(ejercicio), .groups = "drop") |>
  mutate(cohorte_era = ERA_OF_YEAR(entrada))

# ── Entry rates by region-year ───────────────────────────────────────────────
active <- firm_years |> group_by(region, ejercicio) |>
  summarise(activos = n_distinct(cuit), .groups = "drop")
entries <- life |> count(region, entrada, name = "entradas") |>
  rename(ejercicio = entrada)
ee <- active |> left_join(entries, by = c("region", "ejercicio")) |>
  mutate(tasa_entrada = entradas / activos)
write_tab(ee, "tab_entry_exit_region.csv")

# ── Cohort persistence across eras ───────────────────────────────────────────
active_by_era <- firm_years |>
  mutate(era = ERA_OF_YEAR(ejercicio)) |>
  distinct(cuit, era) |>
  mutate(v = TRUE) |>
  pivot_wider(names_from = era, values_from = v, values_fill = FALSE)

surv_of <- function(cuits, target) {
  mean(active_by_era[[target]][match(cuits, active_by_era$cuit)], na.rm = FALSE)
}

pairs <- list(c("macri", "fernandez"), c("macri", "milei"),
              c("fernandez", "milei"))
cs <- bind_rows(lapply(c("NEA", "Metro", "Resto"), function(reg) {
  bind_rows(lapply(pairs, function(pr) {
    cuits <- life$cuit[life$region == reg & life$cohorte_era == pr[1]]
    if (!length(cuits)) return(NULL)
    tibble(region = reg, cohorte = pr[1], sobrevive_en = pr[2],
           n_cohorte = length(cuits),
           tasa_supervivencia = round(surv_of(cuits, pr[2]), 3))
  }))
}))
cs <- bind_rows(cs, bind_rows(lapply(sort(NEA), function(prov) {
  cuits <- life$cuit[life$provincia == prov & life$cohorte_era == "macri"]
  if (!length(cuits)) return(NULL)
  tibble(region = prov, cohorte = "macri", sobrevive_en = "milei",
         n_cohorte = length(cuits),
         tasa_supervivencia = round(surv_of(cuits, "milei"), 3))
})))
write_tab(cs, "tab_cohort_survival.csv")

# ── Discrete survival curves (entrants <= 2021) ──────────────────────────────
curves <- bind_rows(lapply(REGION_LEVELS, function(reg) {
  sub <- life |> filter(region == reg, entrada <= 2021)
  bind_rows(lapply(0:5, function(k) {
    at_risk <- sub |> filter(entrada + k <= 2026)
    tibble(region = reg, k = k, n = nrow(sub),
           surv = mean(at_risk$salida >= at_risk$entrada + k))
  }))
})) |>
  mutate(region = factor(region, levels = REGION_LEVELS))

p <- ggplot(curves, aes(k, surv, colour = region)) +
  geom_line() + geom_point() +
  scale_colour_manual(values = REGION_COLS) +
  labs(x = "Años desde la primera adjudicación",
       y = "P(sigue adjudicando en el año k o después)",
       title = "Supervivencia de proveedores (entrantes 2016-2021)") +
  theme_house()
save_fig(p, "fig_survival_by_region.png")

cat("\nPersistencia de cohortes:\n"); print(cs, n = 30)
cat("\nEntradas por región (2022+):\n")
print(ee |> filter(ejercicio >= 2022) |>
        mutate(tasa_entrada = round(tasa_entrada, 3)), n = 20)
