# 19 — Two closing checks for the publication run.
#
# (a) Deflated era cuts. The yearly Theil decompositions are scale-invariant,
#     so inflation cannot drive them; the era-pooled amount cut of script 03
#     is not, because it adds nominal pesos across years of very different
#     price levels. Recomputed here in constant 2024 pesos.
#
# (b) Scale within legal form. The external review asked for a size control on
#     the sole-proprietor result. Neither register carries one: the company
#     registry has no employment or turnover field and the mass taxpayer file
#     records no province for natural persons. What can be measured is the
#     scale of the exchange itself, within legal form and across regions.
#
# Run: Rscript 19_deflated_and_scale.R

source("theme_house.R", chdir = TRUE)

theil_t <- function(x) {
  x <- x[x > 0 & !is.na(x)]
  if (length(x) < 2) return(NA_real_)
  mu <- mean(x)
  mean((x / mu) * log(x / mu))
}

theil_decompose <- function(df, value, group) {
  d <- df[df[[value]] > 0 & !is.na(df[[value]]), ]
  if (nrow(d) < 2) return(c(total = NA, between = NA, within = NA))
  mu <- mean(d[[value]]); n <- nrow(d)
  parts <- d |> group_by(.data[[group]]) |>
    summarise(ng = n(), mug = mean(.data[[value]]),
              tg = theil_t(.data[[value]]), .groups = "drop")
  between <- sum((parts$ng / n) * (parts$mug / mu) * log(parts$mug / mu))
  c(total = theil_t(d[[value]]), between = between,
    within = sum((parts$ng / n) * (parts$mug / mu) *
                   ifelse(is.na(parts$tg), 0, parts$tg)))
}

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia), moneda == "ARS",
         between(ejercicio, WIN0, WIN1))

ipc_path <- file.path(DIR_PROC, "ipc_anual.csv")
if (!file.exists(ipc_path)) stop("falta ipc_anual.csv: correr 02 primero")
ipc <- read.csv(ipc_path)
adj <- adj |> left_join(ipc |> select(ejercicio = anio, deflactor_2024),
                        by = "ejercicio") |>
  mutate(monto_real = monto * deflactor_2024)
stopifnot(!any(is.na(adj$deflactor_2024)))

# Dolares del año de la adjudicacion: cada monto NOMINAL al tipo de cambio
# oficial promedio de su propio ejercicio. Con inflacion anual de tres digitos
# una tasa unica no dice nada sobre lo que valia una adjudicacion de 2019.
tc_path <- file.path(DIR_PROC, "tipo_cambio_anual.csv")
if (!file.exists(tc_path)) stop("falta tipo_cambio_anual.csv: correr 02 primero")
adj <- adj |>
  left_join(read.csv(tc_path) |> select(ejercicio = anio, tc_oficial_prom),
            by = "ejercicio") |>
  mutate(monto_usd = monto / tc_oficial_prom)
stopifnot(!any(is.na(adj$tc_oficial_prom)))

# ── (a) era cuts, nominal vs constant 2024 pesos ─────────────────────────────
cortes <- bind_rows(lapply(ERAS, function(e) {
  bind_rows(lapply(c("monto", "monto_real"), function(v) {
    firm <- adj |> filter(era == e) |>
      group_by(cuit, provincia) |>
      summarise(monto = sum(monto, na.rm = TRUE),
                monto_real = sum(monto_real, na.rm = TRUE), .groups = "drop") |>
      mutate(region = ifelse(provincia %in% NEA, "NEA", "Resto"))
    r_reg <- theil_decompose(firm, v, "region")
    r_prov <- theil_decompose(firm, v, "provincia")
    tibble(era = e, medida = ifelse(v == "monto", "nominal",
                                    "constante 2024"),
           theil_total = r_prov["total"],
           between_provincia = r_prov["between"],
           between_region_nea = r_reg["between"],
           share_between_provincia = r_prov["between"] / r_prov["total"])
  }))
}))
write_tab(cortes |> mutate(across(where(is.numeric), ~round(.x, 4))),
          "tab_theil_era_deflactado.csv")

# ── (b) scale of the exchange, within legal form ─────────────────────────────
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
forma_of <- function(p) ifelse(p == "Persona Fisica", "Persona fisica",
                               ifelse(is.na(p), NA, "Sociedad"))

sup <- adj |>
  group_by(cuit, provincia) |>
  summarise(n_adj = n(), monto_real = sum(monto_real, na.rm = TRUE),
            monto_usd = sum(monto_usd, na.rm = TRUE),
            n_organismos = n_distinct(organismo),
            n_rubros = n_distinct(rubro_principal),
            anios = n_distinct(ejercicio),
            share_directa = mean(grepl("Directa", procedimiento,
                                       ignore.case = TRUE)),
            share_anclado = mean(anclaje_territorial), .groups = "drop") |>
  left_join(master |> select(cuit, personeria), by = "cuit") |>
  mutate(forma = forma_of(personeria),
         region = factor(region_of(provincia), levels = REGION_LEVELS)) |>
  filter(!is.na(forma))

escala <- sup |>
  group_by(forma, region) |>
  summarise(n_proveedores = n(),
            monto_mediano = round(median(monto_real)),
            monto_medio = round(mean(monto_real)),
            monto_mediano_usd = round(median(monto_usd)),
            monto_medio_usd = round(mean(monto_usd)),
            valor_mediano_por_adj = round(median(monto_real / n_adj)),
            valor_mediano_por_adj_usd = round(median(monto_usd / n_adj)),
            adj_medianas = median(n_adj),
            organismos_medianos = median(n_organismos),
            rubros_medianos = median(n_rubros),
            anios_medianos = median(anios),
            share_directa = round(mean(share_directa), 3),
            share_anclado = round(mean(share_anclado), 3),
            .groups = "drop")
write_tab(escala, "tab_sole_proprietor_scale.csv")

# formal test: is the scale of a peripheral sole proprietor different from a
# metropolitan one? (rank test on award value, within legal form)
pf <- sup |> filter(forma == "Persona fisica", region %in% c("NEA", "Metro"))
soc <- sup |> filter(forma == "Sociedad", region %in% c("NEA", "Metro"))
pruebas <- tibble(
  comparacion = c("Persona fisica: NEA vs Metro (valor por adjudicacion)",
                  "Sociedad: NEA vs Metro (valor por adjudicacion)"),
  n_nea = c(sum(pf$region == "NEA"), sum(soc$region == "NEA")),
  n_metro = c(sum(pf$region == "Metro"), sum(soc$region == "Metro")),
  mediana_nea = c(median(pf$monto_real[pf$region == "NEA"] /
                           pf$n_adj[pf$region == "NEA"]),
                  median(soc$monto_real[soc$region == "NEA"] /
                           soc$n_adj[soc$region == "NEA"])),
  mediana_metro = c(median(pf$monto_real[pf$region == "Metro"] /
                             pf$n_adj[pf$region == "Metro"]),
                    median(soc$monto_real[soc$region == "Metro"] /
                             soc$n_adj[soc$region == "Metro"])),
  p = c(wilcox.test(I(monto_real / n_adj) ~ region, data = pf)$p.value,
        wilcox.test(I(monto_real / n_adj) ~ region, data = soc)$p.value)) |>
  mutate(across(c(mediana_nea, mediana_metro), round),
         p = signif(p, 3))
write_tab(pruebas, "tab_sole_proprietor_test.csv")

# Representation ratios are NOT computed here. They count distinct suppliers
# against the provincial stock of companies and have nothing to do with
# deflation, whereas this script filters to peso-denominated awards in order to
# deflate them. Applying that filter drops suppliers that only ever won in
# foreign currency and biases the count, which is why an earlier version of this
# block produced a second set of ratios that disagreed with script 21 in the
# third decimal. Script 21 is canonical; the ratios live in
# tab_gradiente_provincias.csv.

cat("\nCortes por era, nominal vs constante 2024:\n")
print(cortes |> mutate(across(where(is.numeric), ~round(.x, 3))), n = 12)
cat("\nEscala por forma juridica y region (montos en pesos de 2024):\n")
print(escala, n = 12)
cat("\nPruebas de rango:\n"); print(pruebas)
