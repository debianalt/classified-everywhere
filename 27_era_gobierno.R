# 27 — The market across three presidencies, on the balanced panel.
#
# Supports the closing block of section 6: yearly volume, constant-peso
# amounts, procedure and demand composition on the 72 buyers present in every
# year of the window, so that presidency comparisons are not contaminated by
# platform onboarding (script 20). The Macri presidency is represented by 2019
# alone; amounts are ARS deflated to 2024 pesos with the IPC deflator built in
# script 02, as in 19_deflated_and_scale.R. COVID emergency purchases ran
# under Administrative Decision 409/2020 outside this platform's regular
# record, which is why 2020 shows no surge here.
#
# Run: Rscript 27_era_gobierno.R

source("theme_house.R", chdir = TRUE)

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, between(ejercicio, WIN0, WIN1))

panel <- adj |> distinct(organismo, ejercicio) |> count(organismo) |>
  filter(n == WIN1 - WIN0 + 1) |> pull(organismo)

ipc_path <- file.path(DIR_PROC, "ipc_anual.csv")
if (!file.exists(ipc_path)) stop("falta ipc_anual.csv: correr 02 primero")
p <- adj |> filter(organismo %in% panel) |>
  left_join(read.csv(ipc_path) |> select(ejercicio = anio, deflactor_2024),
            by = "ejercicio") |>
  mutate(real = ifelse(moneda == "ARS", monto * deflactor_2024, NA_real_))
stopifnot(!any(is.na(p$deflactor_2024)))

# ── (a) the yearly series the presidency block quotes ────────────────────────
anual <- p |> group_by(ejercicio) |> summarise(
  n_adj = n(),
  pct_directa = round(100 * mean(grepl("Directa", procedimiento,
                                       ignore.case = TRUE)), 1),
  pct_anclado = round(100 * mean(anclaje_territorial), 1),
  pct_salud_adj = round(100 * mean(tipo_organismo == "salud_social"), 1),
  pct_salud_monto = round(100 * sum(real[tipo_organismo == "salud_social"],
                                    na.rm = TRUE) / sum(real, na.rm = TRUE), 1),
  monto_real_bn = round(sum(real, na.rm = TRUE) / 1e9, 0),
  .groups = "drop")
write_tab(anual, "tab_era_anual_panel.csv")

# ── (b) demand composition by organ type and presidency ──────────────────────
comp <- p |> count(era, tipo_organismo) |> group_by(era) |>
  mutate(pct_adj = round(100 * n / sum(n), 1)) |> ungroup() |> select(-n) |>
  tidyr::pivot_wider(names_from = era, values_from = pct_adj) |>
  select(tipo_organismo, macri, fernandez, milei)
write_tab(comp, "tab_era_composicion_panel.csv")

cat("\nSerie anual, panel balanceado (", length(panel), "organismos):\n")
print.data.frame(anual)
cat("\nComposicion de la demanda por presidencia, % de adjudicaciones:\n")
print.data.frame(comp)
