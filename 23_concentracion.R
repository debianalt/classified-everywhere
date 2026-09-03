# 23 — Supplier concentration behind the regional aggregates.
#
# The procedural claim of the first draft — direct contracting rises in the
# north-east whilst it falls everywhere else — does not survive being asked at
# the level of the jurisdiction. It does not survive being asked of the
# suppliers either: one provisioner carries almost a quarter of the region's
# awards, and the regional series changes sign when that one supplier is left
# out. This script measures the concentration and reports the series with and
# without the leading supplier of each region.
#
# ANONYMISATION (project rule, 27 Aug): the leading north-eastern provisioner is
# a natural person. No identifier and no name is written to any output, printed
# to the console, or used in a comment. Outputs carry rank and proportions only.
#
# Run: Rscript 23_concentracion.R

source("theme_house.R", chdir = TRUE)

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia), between(ejercicio, WIN0, WIN1)) |>
  mutate(directa = grepl("Directa", procedimiento, ignore.case = TRUE),
         grupo = grupo6_of(provincia))

n_total <- nrow(adj)

# ── concentration of the leading supplier, by jurisdiction ───────────────────
por_prov <- adj |> group_by(provincia, grupo) |>
  summarise(n_adj = n(), proveedores = n_distinct(cuit), .groups = "drop")

lider <- adj |> count(provincia, cuit, name = "n_lider") |>
  group_by(provincia) |> slice_max(n_lider, n = 1, with_ties = FALSE) |>
  ungroup()

# the leading supplier's own profile, carried only as attributes
perfil <- adj |> semi_join(lider, by = c("provincia", "cuit")) |>
  group_by(provincia) |>
  summarise(personeria_lider = names(sort(table(personeria),
                                          decreasing = TRUE))[1],
            cliente_lider = names(sort(table(tipo_organismo),
                                       decreasing = TRUE))[1],
            anios_lider = n_distinct(ejercicio),
            directa_lider = round(mean(directa), 3),
            .groups = "drop")

conc <- por_prov |>
  left_join(lider |> select(provincia, n_lider), by = "provincia") |>
  left_join(perfil, by = "provincia") |>
  mutate(share_prov_lider = round(n_lider / n_adj, 3),
         share_pais_lider = round(n_lider / n_total, 4)) |>
  select(-n_lider) |>
  arrange(desc(share_prov_lider))

# share of the leading supplier's region carried by that supplier
conc <- conc |>
  left_join(adj |> count(grupo, name = "n_grupo"), by = "grupo") |>
  left_join(adj |> semi_join(lider, by = c("provincia", "cuit")) |>
              count(provincia, name = "n_l"), by = "provincia") |>
  mutate(share_region_lider = round(n_l / n_grupo, 3)) |>
  select(provincia, grupo, n_adj, proveedores, share_prov_lider,
         share_region_lider, share_pais_lider, personeria_lider,
         cliente_lider, anios_lider, directa_lider)
write_tab(conc, "tab_concentracion.csv")

# ── the north-eastern procedural series, with and without its leading supplier
nea_lider <- lider |> semi_join(adj |> filter(grupo == "North-east") |>
                                  count(provincia, wt = n()) |>
                                  slice_max(n, n = 1, with_ties = FALSE),
                                by = "provincia") |> pull(cuit)

serie <- bind_rows(
  adj |> filter(grupo == "North-east") |>
    group_by(era) |> summarise(universo = "todos", n_adj = n(),
                               share_directa = mean(directa), .groups = "drop"),
  adj |> filter(grupo == "North-east", !cuit %in% nea_lider) |>
    group_by(era) |> summarise(universo = "sin el principal", n_adj = n(),
                               share_directa = mean(directa), .groups = "drop")) |>
  mutate(era = factor(era, levels = ERAS), share_directa = round(share_directa, 3)) |>
  arrange(universo, era)
write_tab(serie, "tab_concentracion_nea_serie.csv")

d <- serie |> group_by(universo) |>
  summarise(delta = round(share_directa[era == "milei"] -
                            share_directa[era == "macri"], 3), .groups = "drop")

cat("\nConcentracion del proveedor principal, por jurisdiccion:\n")
print(conc |> select(provincia, grupo, n_adj, share_prov_lider,
                     share_region_lider, personeria_lider, cliente_lider,
                     anios_lider, directa_lider), n = 24)
cat("\nSerie procedimental del nordeste, con y sin su proveedor principal:\n")
print(serie, n = 6)
cat("\nDelta macri->milei:\n"); print(d)
cat("\n(sin identificadores en ninguna salida)\n")
