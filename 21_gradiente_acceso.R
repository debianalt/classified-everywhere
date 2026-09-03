# 21 — The gradient of access to the national state.
#
# The first pass treated the north-east as an exception. It is not: the share
# of awards coming from territorially anchored organs runs continuously from
# 0.52 in the federal capital to 0.94 in Chaco, and the Patagonian provinces
# sit interleaved with the north-eastern ones. What distinguishes the extremes
# is the form of access — natural persons against companies — not a regional
# peculiarity. This script builds the province-level table the argument rests
# on, and the group summary.
#
# The grouping is the six standard regions of Argentina, which partition the 24
# jurisdictions with no residual. An earlier version kept three named groups and
# a "rest of country" of thirteen provinces, which averaged the north-west and
# the pampean provinces together: identical anchored demand, seventeen points
# apart on the share of suppliers that are natural persons.
#
# Run: Rscript 21_gradiente_acceso.R

source("theme_house.R", chdir = TRUE)

N_MIN <- 50

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia), between(ejercicio, WIN0, WIN1))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
den <- read_parquet(file.path(DIR_PROC, "denominators_provincia_anio.parquet"))

por_adj <- adj |> group_by(provincia) |>
  summarise(n_adj = n(), proveedores = n_distinct(cuit),
            share_anclado = mean(anclaje_territorial),
            share_seguridad = mean(tipo_organismo == "seguridad_defensa"),
            share_directa = mean(grepl("Directa", procedimiento,
                                       ignore.case = TRUE)),
            .groups = "drop")

por_prov <- adj |> distinct(cuit, provincia) |>
  left_join(master |> select(cuit, personeria), by = "cuit") |>
  filter(!is.na(personeria)) |>
  group_by(provincia) |>
  summarise(share_persona_fisica = mean(personeria == "Persona Fisica"),
            .groups = "drop")

soc <- master |> filter(!is.na(personeria),
                        personeria != "Persona Fisica") |> pull(cuit)
stock <- den |> filter(ejercicio == WIN1) |>
  select(provincia, stock = stock_sociedades)
repr <- adj |> group_by(provincia) |>
  summarise(firmas = n_distinct(cuit),
            sociedades = n_distinct(cuit[cuit %in% soc]), .groups = "drop") |>
  left_join(stock, by = "provincia") |>
  mutate(ratio_firmas = (firmas / sum(firmas)) /
           (stock / sum(stock, na.rm = TRUE)),
         ratio_sociedades = (sociedades / sum(sociedades)) /
           (stock / sum(stock, na.rm = TRUE)))

g <- por_adj |>
  inner_join(por_prov, by = "provincia") |>
  inner_join(repr |> select(provincia, ratio_firmas, ratio_sociedades),
             by = "provincia") |>
  filter(proveedores >= N_MIN) |>
  mutate(grupo = grupo6_of(provincia), nea = provincia %in% NEA) |>
  arrange(desc(share_anclado))
stopifnot(!any(is.na(g$grupo)), nrow(g) == 24)
# scripts 13, 24, 26 and 37 recompute statistics from this file, so it is an
# interchange table and not a display one: rounding it to three decimals made
# the Spearman of the gradient come back as 0.633 there against the 0.637
# computed here. Presentation rounding belongs to the manuscript's table.
write_tab(g |> mutate(across(where(is.numeric), ~round(.x, 6))),
          "tab_gradiente_provincias.csv")

grupos <- g |> mutate(grupo = factor(grupo, levels = GROUP_LEVELS)) |>
  group_by(grupo) |>
  summarise(provincias = n(), n_proveedores = sum(proveedores),
            share_anclado = round(weighted.mean(share_anclado, n_adj), 3),
            share_seguridad = round(weighted.mean(share_seguridad, n_adj), 3),
            share_persona_fisica = round(weighted.mean(share_persona_fisica,
                                                       proveedores), 3),
            share_directa = round(weighted.mean(share_directa, n_adj), 3),
            ratio_firmas = round(median(ratio_firmas), 3),
            ratio_sociedades = round(median(ratio_sociedades), 3),
            .groups = "drop") |>
  arrange(desc(share_anclado))
write_tab(grupos, "tab_gradiente_grupos.csv")

# the two dimensions of the gradient move together: anchored demand and
# personal access are not independent descriptions of the same provinces
ct <- suppressWarnings(cor.test(g$share_anclado, g$share_persona_fisica,
                                method = "spearman"))
ct2 <- suppressWarnings(cor.test(g$share_anclado, g$ratio_sociedades,
                                 method = "spearman"))
coh <- tibble(
  relacion = c("demanda anclada vs proveedores persona física",
               "demanda anclada vs ratio de sociedades"),
  rho = round(c(unname(ct$estimate), unname(ct2$estimate)), 3),
  p = signif(c(ct$p.value, ct2$p.value), 3),
  provincias = nrow(g))
write_tab(coh, "tab_gradiente_coherencia.csv")

# the same six groups over time, so that every panel of the published figure
# names its categories identically
serie_grupo <- adj |>
  mutate(grupo = grupo6_of(provincia)) |>
  group_by(grupo, era) |>
  summarise(n_adj = n(),
            share_anclado = round(mean(anclaje_territorial), 3),
            share_directa = round(mean(grepl("Directa", procedimiento,
                                             ignore.case = TRUE)), 3),
            share_abierta = round(mean(grepl("Licitaci|Concurso",
                                             procedimiento,
                                             ignore.case = TRUE)), 3),
            .groups = "drop")
write_tab(serie_grupo, "tab_gradiente_grupo_era.csv")

# The same series at the level of the jurisdiction, which is the unit the paper
# describes: a region has no government, no procurement regime and no fiscal
# relation with the nation, so a regional average of procedure mixes quantities
# that belong to different political units. The n by era are reported because
# the Macri era is 2019 alone and several jurisdictions carry very few awards in
# it, which is what decides whether a change can be read at all.
serie_juris <- adj |>
  group_by(provincia, era) |>
  summarise(n_adj = n(),
            share_directa = mean(grepl("Directa", procedimiento,
                                       ignore.case = TRUE)),
            .groups = "drop") |>
  pivot_wider(names_from = era,
              values_from = c(n_adj, share_directa)) |>
  mutate(delta = round(share_directa_milei - share_directa_macri, 3),
         across(starts_with("share_directa"), ~round(.x, 3)),
         grupo = grupo6_of(provincia)) |>
  select(provincia, grupo, starts_with("share_directa"), delta,
         starts_with("n_adj")) |>
  arrange(desc(delta))
write_tab(serie_juris, "tab_directa_jurisdiccion_era.csv")
cat(sprintf("\nDirecta por jurisdiccion: %d bajan, %d suben (macri->milei)\n",
            sum(serie_juris$delta < 0), sum(serie_juris$delta > 0)))
print(serie_juris, n = 24, width = 130)

cat("\nSerie por grupo y era:\n"); print(serie_grupo, n = 18)
cat("\nGradiente por provincia:\n")
print(g |> select(provincia, grupo, proveedores, share_anclado,
                  share_persona_fisica, ratio_firmas, ratio_sociedades) |>
        mutate(across(where(is.numeric), ~round(.x, 3))), n = 30)
cat("\nResumen por grupo:\n"); print(grupos)
cat("\nCoherencia del gradiente:\n"); print(coh)
