# 12 — Robustness battery (canonical R port of 12_robustness.py).
# Run: Rscript 12_robustness.R

source("theme_house.R", chdir = TRUE)

ERA_OF_YEAR <- function(y) ifelse(y <= 2019, "macri",
                                  ifelse(y <= 2023, "fernandez", "milei"))

theil_between_share <- function(firm, value = "monto", group = "provincia") {
  d <- firm[firm[[value]] > 0 & !is.na(firm[[value]]), ]
  if (nrow(d) < 2) return(NA_real_)
  mu <- mean(d[[value]]); n <- nrow(d)
  x <- d[[value]]
  total <- mean((x / mu) * log(x / mu))
  parts <- d |> group_by(.data[[group]]) |>
    summarise(ng = n(), mug = mean(.data[[value]]), .groups = "drop")
  between <- sum((parts$ng / n) * (parts$mug / mu) * log(parts$mug / mu))
  between / total
}

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia), moneda == "ARS")

# the 2016-2019 baseline predates the arrival of the armed forces on the
# platform (see 20_platform_stability.R), so the mix it fixes is the mix of a
# different state; the 2019 baseline is the first one taken on the full set of
# buyers, and both are reported
base_mix <- adj |> filter(between(ejercicio, 2016, 2019)) |>
  count(tipo_organismo) |> mutate(share = n / sum(n)) |>
  select(tipo_organismo, share_base = share)
base_mix_org <- adj |> filter(between(ejercicio, 2016, 2019)) |>
  count(organismo) |> mutate(share = n / sum(n)) |>
  select(organismo, share_base_org = share)
base_mix_19 <- adj |> filter(ejercicio == WIN0) |>
  count(tipo_organismo) |> mutate(share = n / sum(n)) |>
  select(tipo_organismo, share_base_19 = share)

rt <- bind_rows(lapply(sort(unique(adj$ejercicio)), function(yr) {
  g <- adj |> filter(ejercicio == yr)
  firm <- g |> group_by(cuit, provincia) |>
    summarise(monto = sum(monto, na.rm = TRUE), .groups = "drop")
  g2 <- g |> filter(tipo_organismo != "seguridad_defensa")
  firm2 <- g2 |> group_by(cuit, provincia) |>
    summarise(monto = sum(monto, na.rm = TRUE), .groups = "drop")
  yr_mix <- g |> count(tipo_organismo) |> mutate(share = n / sum(n))
  w <- g |> left_join(yr_mix |> select(tipo_organismo, share_yr = share),
                      by = "tipo_organismo") |>
    left_join(base_mix, by = "tipo_organismo") |>
    mutate(w = coalesce(share_base / share_yr, 0),
           monto_w = monto * w)
  firm3 <- w |> group_by(cuit, provincia) |>
    summarise(monto = sum(monto_w, na.rm = TRUE), .groups = "drop")
  # finer variant: constant mix at organism level (162 buyers)
  yr_org <- g |> count(organismo) |> mutate(share_yr_org = n / sum(n)) |>
    select(organismo, share_yr_org)
  w4 <- g |> left_join(yr_org, by = "organismo") |>
    left_join(base_mix_org, by = "organismo") |>
    mutate(monto_w = monto * coalesce(share_base_org / share_yr_org, 0))
  firm4 <- w4 |> group_by(cuit, provincia) |>
    summarise(monto = sum(monto_w, na.rm = TRUE), .groups = "drop")
  w5 <- g |> left_join(yr_mix |> select(tipo_organismo, share_yr = share),
                       by = "tipo_organismo") |>
    left_join(base_mix_19, by = "tipo_organismo") |>
    mutate(monto_w = monto * coalesce(share_base_19 / share_yr, 0))
  firm5 <- w5 |> group_by(cuit, provincia) |>
    summarise(monto = sum(monto_w, na.rm = TRUE), .groups = "drop")
  tibble(ejercicio = yr,
         variante = c("baseline", "sin_seguridad", "mix_constante",
                      "mix_constante_organismo", "mix_constante_2019"),
         share_between = c(theil_between_share(firm),
                           theil_between_share(firm2),
                           theil_between_share(firm3),
                           theil_between_share(firm4),
                           theil_between_share(firm5)))
}))
write_tab(rt, "tab_robustez_theil.csv")

d <- rt |> filter(between(ejercicio, 2017, 2025))
p <- ggplot(d, aes(ejercicio, 100 * share_between, colour = variante,
                   shape = variante, linetype = variante)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = c(2019.5, 2023.5), linetype = "dotted",
             colour = "grey50", linewidth = 0.4) +
  scale_colour_manual(values = c(baseline = "#333333",
                                 sin_seguridad = "#d62728",
                                 mix_constante = "#1f77b4")) +
  labs(x = NULL, y = "Theil between-provincia (% del total)",
       title = "Robustez de la fractura territorial (montos ARS, 2017-2025)") +
  theme_house()
save_fig(p, "fig_robustez_theil.png")

# ── Survival stratified by client type ───────────────────────────────────────
# Territorial-client position measured AT COHORT ENTRY (first-year awards
# only) to avoid future-data leakage (external review 2.6).
cliente <- adj |> group_by(cuit) |>
  filter(ejercicio == min(ejercicio)) |>
  summarise(territorial = mean(anclaje_territorial),
            provincia = first(provincia),
            entrada = first(ejercicio), .groups = "drop") |>
  mutate(cliente_territorial = territorial > 0.5,
         region = region_of(provincia),
         cohorte = ERA_OF_YEAR(entrada))

act_era <- adj |> mutate(era = ERA_OF_YEAR(ejercicio)) |>
  distinct(cuit, era) |> mutate(v = TRUE) |>
  pivot_wider(names_from = era, values_from = v, values_fill = FALSE)

rs <- cliente |> filter(cohorte == "macri") |>
  left_join(act_era, by = "cuit") |>
  group_by(region, cliente_territorial) |>
  summarise(n = n(),
            surv_fernandez = round(mean(coalesce(fernandez, FALSE)), 3),
            surv_milei = round(mean(coalesce(milei, FALSE)), 3),
            .groups = "drop")
write_tab(rs, "tab_robustez_supervivencia.csv")

# ── Supplier-level exit ratio ────────────────────────────────────────────────
den <- read_parquet(file.path(DIR_PROC, "denominators_provincia_anio.parquet"))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
stock <- den |> filter(ejercicio == 2025) |> select(provincia, stock = stock_sociedades)
soc_cuits <- master |> filter(!is.na(personeria),
                              personeria != "Persona Fisica") |> pull(cuit)
ex <- adj |> group_by(provincia) |>
  summarise(firmas_ganadoras = n_distinct(cuit),
            sociedades_ganadoras = n_distinct(cuit[cuit %in% soc_cuits]),
            .groups = "drop") |>
  left_join(stock, by = "provincia") |>
  mutate(exit_ratio_firmas = (firmas_ganadoras / sum(firmas_ganadoras)) /
           (stock / sum(stock, na.rm = TRUE)),
         exit_ratio_soc = (sociedades_ganadoras / sum(sociedades_ganadoras)) /
           (stock / sum(stock, na.rm = TRUE)),
         nea = provincia %in% NEA)
write_tab(ex |> mutate(across(where(is.numeric), ~round(.x, 4))),
          "tab_exit_ratio_v2.csv")

cat("Theil between-share (%):\n")
print(rt |> mutate(share_between = round(100 * share_between, 1)) |>
        pivot_wider(names_from = variante, values_from = share_between), n = 12)
cat("\nSupervivencia cohorte Macri por región x cliente territorial:\n")
print(rs)
cat("\nExit ratio a nivel firma (NEA + referencia):\n")
print(ex |> filter(provincia %in% c(sort(NEA), "CABA", "Buenos Aires",
                                    "Córdoba")) |>
        select(provincia, firmas_ganadoras, sociedades_ganadoras,
               exit_ratio_firmas, exit_ratio_soc) |>
        mutate(across(where(is.numeric), ~round(.x, 3))))
