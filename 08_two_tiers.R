# 08 — Two tiers of state-mediated exchange (canonical R port of
# 08_two_tiers.py; same outputs).
# Run: Rscript 08_two_tiers.R

source("theme_house.R", chdir = TRUE)
suppressPackageStartupMessages({library(readxl); library(stringi)})

PROV_CANON <- c("Buenos Aires", "CABA", "Catamarca", "Chaco", "Chubut",
                "Córdoba", "Corrientes", "Entre Ríos", "Formosa", "Jujuy",
                "La Pampa", "La Rioja", "Mendoza", "Misiones", "Neuquén",
                "Río Negro", "Salta", "San Juan", "San Luis", "Santa Cruz",
                "Santa Fe", "Santiago del Estero", "Tierra del Fuego",
                "Tucumán")
norm_key <- function(s) toupper(trimws(stri_trans_general(s, "Latin-ASCII")))
LOOKUP <- setNames(PROV_CANON, norm_key(PROV_CANON))
LOOKUP[c("G.C.B.A.", "GCBA", "CAPITAL FEDERAL", "TIERRA DEL FUEGO (**)",
         "STGO. DEL ESTERO", "SGO. DEL ESTERO")] <-
  c("CABA", "CABA", "CABA", "Tierra del Fuego",
    "Santiago del Estero", "Santiago del Estero")

xlsx <- "data/raw/dnap_ocupacion_salarios_1987_2024.xlsx"
sheets <- excel_sheets(xlsx)
emp <- bind_rows(lapply(sheets, function(sheet) {
  year <- suppressWarnings(as.integer(sheet))
  if (is.na(year)) return(NULL)
  df <- suppressMessages(read_excel(xlsx, sheet = sheet, col_names = FALSE,
                                    .name_repair = "minimal"))
  # readxl drops leading empty columns in some sheets: locate the header cell
  hdr_row <- NA; name_col <- NA
  for (j in seq_len(min(4, ncol(df)))) {
    colj <- trimws(as.character(df[[j]]))
    hit <- which(colj %in% c("PROVINCIAS", "JURISDICCIONES"))
    if (length(hit)) { hdr_row <- hit[1]; name_col <- j; break }
  }
  if (is.na(hdr_row)) { cat("WARN", sheet, "sin header\n"); return(NULL) }
  nombres <- trimws(as.character(df[[name_col]]))
  planta_col <- name_col + 2
  hab_col <- name_col + 4
  rows <- list()
  for (i in seq(hdr_row + 1, nrow(df))) {
    nm <- nombres[i]
    if (is.na(nm) || nm == "") next
    key <- norm_key(nm)
    if (startsWith(key, "TOTAL")) break
    prov <- LOOKUP[key]
    if (is.na(prov)) next
    rows[[length(rows) + 1]] <- tibble(
      provincia = unname(prov), anio = year,
      planta = suppressWarnings(as.numeric(df[[planta_col]][i])),
      habitantes = suppressWarnings(as.numeric(df[[hab_col]][i])))
  }
  bind_rows(rows)
})) |> filter(!is.na(planta)) |>
  mutate(empleo_x1000hab = 1000 * planta / habitantes)

write_parquet(emp, file.path(DIR_PROC, "empleo_publico_panel.parquet"))
write_tab(emp, "tab_empleo_publico_provincia.csv")
cat("panel:", nrow(emp), "filas,", min(emp$anio), "-", max(emp$anio), ",",
    n_distinct(emp$provincia), "provincias\n")

# ── Long series figure ───────────────────────────────────────────────────────
nat <- emp |> group_by(anio) |>
  summarise(empleo_x1000hab = 1000 * sum(planta) / sum(habitantes),
            .groups = "drop") |>
  mutate(provincia = "Total 24 jurisdicciones")
d1 <- bind_rows(emp |> filter(provincia %in% NEA) |>
                  select(provincia, anio, empleo_x1000hab), nat)
p1 <- ggplot(d1, aes(anio, empleo_x1000hab, colour = provincia)) +
  annotate("rect", xmin = c(1989, 2003, 2015.9, 2023.9),
           xmax = c(1999, 2015, 2019.9, 2024.5),
           ymin = -Inf, ymax = Inf, alpha = 0.06, fill = "grey40") +
  geom_line(aes(linetype = provincia == "Total 24 jurisdicciones")) +
  scale_colour_manual(values = c(Chaco = "#d62728", Corrientes = "#ff7f0e",
                                 Formosa = "#9467bd", Misiones = "#8c564b",
                                 `Total 24 jurisdicciones` = "black")) +
  scale_linetype_manual(values = c(`TRUE` = "dashed", `FALSE` = "solid"),
                        guide = "none") +
  labs(x = NULL, y = "Empleo público provincial por 1.000 habitantes",
       title = "Planta pública provincial, 1987-2024 (DNAP)") +
  theme_house()
save_fig(p1, "fig_empleo_largo_nea.png", width = 10, height = 5)

# ── Two-tier scatter ─────────────────────────────────────────────────────────
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
adjud <- master |> filter(adjudicado, !is.na(provincia)) |>
  count(provincia, name = "proveedores_adj")
e24 <- emp |> filter(anio == 2024) |>
  select(provincia, empleo_x1000hab, habitantes)
tt <- e24 |> left_join(adjud, by = "provincia") |>
  mutate(proveedores_x100khab = 1e5 * proveedores_adj / habitantes,
         nea = provincia %in% NEA)
p2 <- ggplot(tt, aes(empleo_x1000hab, proveedores_x100khab, colour = nea)) +
  geom_point(aes(size = nea)) +
  ggrepel::geom_text_repel(aes(label = provincia), size = 2.4,
                           show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = "#d62728", `FALSE` = "#7fb3d5"),
                      guide = "none") +
  scale_size_manual(values = c(`TRUE` = 3, `FALSE` = 2), guide = "none") +
  labs(x = "Empleo público provincial por 1.000 hab (2024)",
       y = "Proveedores adjudicados por 100.000 hab (2016-2026)",
       title = "Dos pisos del intercambio mediado por el Estado (NEA en rojo)") +
  theme_house()
save_fig(p2, "fig_two_tiers_scatter.png", width = 8, height = 6)

cat("\nEmpleo x1000 hab NEA (años seleccionados):\n")
print(emp |> filter(provincia %in% NEA,
                    anio %in% c(1987, 1995, 2003, 2015, 2024)) |>
        mutate(v = round(empleo_x1000hab, 1)) |>
        select(provincia, anio, v) |>
        pivot_wider(names_from = anio, values_from = v))
