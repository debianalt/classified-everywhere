# 07 — Legibility rates and representation ratios (canonical R port of
# 07_legibility_exit.py; same outputs).
# Run: Rscript 07_legibility_exit.R

source("theme_house.R", chdir = TRUE)

master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_clean.parquet")) |>
  filter(es_nuevo, !is.na(provincia))
den <- read_parquet(file.path(DIR_PROC, "denominators_provincia_anio.parquet"))
cov <- read_parquet(file.path(DIR_PROC, "covariates_provincia_anio.parquet"))

stock <- den |> filter(ejercicio == 2025) |> select(provincia, stock_sociedades)
reg <- master |> filter(!is.na(provincia))
ars <- adj |> filter(moneda == "ARS")

t <- reg |> count(provincia, name = "sipro_total") |>
  left_join(reg |> filter(!is.na(personeria), personeria != "Persona Fisica") |>
              count(provincia, name = "sipro_sociedades"),
            by = "provincia") |>
  left_join(reg |> filter(adjudicado) |> count(provincia, name = "adjudicados"),
            by = "provincia") |>
  left_join(stock, by = "provincia") |>
  mutate(legibilidad_total_x1000 = 1000 * sipro_total / stock_sociedades,
         legibilidad_soc_x1000 = 1000 * sipro_sociedades / stock_sociedades,
         registro_muerto = 1 - adjudicados / sipro_total) |>
  left_join(adj |> count(provincia, name = "n_adj") |>
              mutate(share_adj_n = n_adj / sum(n_adj)) |> select(-n_adj),
            by = "provincia") |>
  left_join(ars |> group_by(provincia) |>
              summarise(m = sum(monto, na.rm = TRUE), .groups = "drop") |>
              mutate(share_adj_monto = m / sum(m)) |> select(-m),
            by = "provincia") |>
  mutate(share_stock = stock_sociedades / sum(stock_sociedades, na.rm = TRUE),
         exit_ratio_n = share_adj_n / share_stock,
         exit_ratio_monto = share_adj_monto / share_stock) |>
  left_join(cov |> filter(between(ejercicio, 2016, 2024)) |>
              group_by(provincia) |>
              summarise(itpp_media_2016_2024 = mean(itpp, na.rm = TRUE),
                        .groups = "drop"),
            by = "provincia") |>
  left_join(read.csv("data/raw/covariates/intra_2024.csv") |>
              select(provincia, intra_2024), by = "provincia") |>
  mutate(nea = provincia %in% NEA)
write_tab(t |> mutate(across(where(is.numeric), ~round(.x, 4))),
          "tab_legibilidad_exit.csv")

corr <- cor(t |> select(legibilidad_soc_x1000, exit_ratio_n, registro_muerto,
                        itpp_media_2016_2024),
            method = "spearman", use = "pairwise.complete.obs")

# ── CONTRAT.AR annex ─────────────────────────────────────────────────────────
con <- read.csv("data/raw/contratar_onc-contratar-contratos.csv",
                colClasses = "character")
ubi <- read.csv("data/raw/contratar_onc-contratar-ubicacion-geografica.csv",
                colClasses = "character") |>
  distinct(numero_obra, .keep_all = TRUE) |>
  select(numero_obra, provincia_nombre)
ca <- con |> left_join(ubi, by = "numero_obra") |>
  mutate(monto = suppressWarnings(as.numeric(contrato_monto))) |>
  group_by(provincia_nombre) |>
  summarise(n_contratos = n(),
            n_contratistas = n_distinct(contratista_cuit),
            monto_total = sum(monto, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(n_contratos))
write_tab(ca, "tab_contratar_provincia_ejecucion.csv")

# ── Figure: exit ratio ───────────────────────────────────────────────────────
d <- t |> filter(!is.na(exit_ratio_n)) |> arrange(exit_ratio_n) |>
  mutate(provincia = factor(provincia, levels = provincia))
p <- ggplot(d, aes(exit_ratio_n, provincia, fill = nea)) +
  geom_col() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_fill_manual(values = c(`TRUE` = "#d62728", `FALSE` = "#7fb3d5"),
                    guide = "none") +
  labs(x = "Share adjudicaciones / share stock de sociedades (2016-2026)",
       y = NULL,
       title = "Ratio de representación en el mercado estatal nacional\n(>1 = sobre-representada; NEA en rojo)") +
  theme_house()
save_fig(p, "fig_exit_ratio.png", width = 8, height = 7)

sel <- c(sort(NEA), "CABA", "Buenos Aires", "Córdoba", "Santa Fe")
cat("\nLegibilidad y exit (NEA + referencia):\n")
print(t |> filter(provincia %in% sel) |>
        select(provincia, legibilidad_soc_x1000, registro_muerto, exit_ratio_n,
               exit_ratio_monto, itpp_media_2016_2024) |>
        mutate(across(where(is.numeric), ~round(.x, 3))))
cat("\nSpearman:\n"); print(round(corr, 3))
cat("\nCONTRAT.AR NEA:\n")
print(ca |> filter(toupper(provincia_nombre) %in% toupper(NEA)))
