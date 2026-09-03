# 24 — Inequality of award amounts WITHIN each jurisdiction.
#
# Script 03 decomposes supplier inequality into between- and within-province
# components year by year, which gives one number per year and does not let
# jurisdictions be compared with each other. This one gives a value per
# jurisdiction: how unequally the national state's spending is spread across
# the suppliers of each province and of the federal capital.
#
# Two cautions built in. Amounts are deflated to 2024 pesos, because a
# cross-sectional index pooled over 2019-2025 would otherwise add pesos of very
# different value. And Theil T has a mechanical ceiling of log(n), so the raw
# index rises with the number of suppliers; the normalised index divides it out.
# That is the same size dependence that turned out to drive the RV result in
# script 22, and it is checked rather than assumed.
#
# Run: Rscript 24_theil_jurisdiccion.R

source("theme_house.R", chdir = TRUE)

theil_t <- function(x) {
  x <- x[x > 0 & !is.na(x)]
  if (length(x) < 2) return(NA_real_)
  mu <- mean(x)
  mean((x / mu) * log(x / mu))
}

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia), moneda == "ARS",
         between(ejercicio, WIN0, WIN1))
ipc <- read.csv(file.path(DIR_PROC, "ipc_anual.csv"))
adj <- adj |> left_join(ipc |> select(ejercicio = anio, deflactor_2024),
                        by = "ejercicio") |>
  mutate(monto_real = monto * deflactor_2024)
stopifnot(!any(is.na(adj$deflactor_2024)))

por_firma <- adj |> group_by(provincia, cuit) |>
  summarise(monto_real = sum(monto_real, na.rm = TRUE), .groups = "drop")

grad <- read.csv(file.path(DIR_TAB, "tab_gradiente_provincias.csv"),
                 encoding = "UTF-8")
conc <- read.csv(file.path(DIR_TAB, "tab_concentracion.csv"),
                 encoding = "UTF-8")

tj <- por_firma |> group_by(provincia) |>
  summarise(proveedores = n(), theil = theil_t(monto_real), .groups = "drop") |>
  mutate(theil_norm = theil / log(proveedores)) |>
  left_join(grad |> select(provincia, grupo, share_anclado,
                           share_persona_fisica), by = "provincia") |>
  left_join(conc |> select(provincia, share_prov_lider), by = "provincia") |>
  mutate(across(c(theil, theil_norm), ~round(.x, 3))) |>
  arrange(desc(theil_norm))
stopifnot(nrow(tj) == 24)
write_tab(tj, "tab_theil_jurisdiccion.csv")

sp <- function(a, b, lab) {
  ct <- suppressWarnings(cor.test(tj[[a]], tj[[b]], method = "spearman"))
  tibble(relacion = lab, rho = round(unname(ct$estimate), 3),
         p = signif(ct$p.value, 3), jurisdicciones = nrow(tj))
}
cors <- bind_rows(
  sp("theil", "proveedores", "Theil crudo vs n de proveedores"),
  sp("theil_norm", "proveedores", "Theil normalizado vs n de proveedores"),
  sp("theil_norm", "share_anclado", "Theil normalizado vs demanda anclada"),
  sp("theil_norm", "share_persona_fisica",
     "Theil normalizado vs proveedores persona física"),
  sp("theil_norm", "share_prov_lider",
     "Theil normalizado vs share del proveedor principal"))
write_tab(cors, "tab_theil_jurisdiccion_cor.csv")

cat("\nDesigualdad dentro de cada jurisdiccion (montos de 2024):\n")
print(tj |> select(provincia, grupo, proveedores, theil, theil_norm,
                   share_anclado), n = 24)
cat("\nCorrelaciones:\n"); print(cors)
cat(sprintf("\nTheil normalizado: de %.3f (%s) a %.3f (%s)\n",
            max(tj$theil_norm), tj$provincia[which.max(tj$theil_norm)],
            min(tj$theil_norm), tj$provincia[which.min(tj$theil_norm)]))
