# 26 — Robustness of the anchoring flag.
#
# The gradient's central correlation (anchored demand vs natural-person
# supply, rho = 0.637) rests on a hand-audited binary flag. Two variant flags
# bound it: a NARROW one keeping only the armed and border forces (dropping
# the road agency, the parks administration and migrations), and a BROAD one
# adding the four organisms the audit (Table S2) names as conservatively left
# unflagged: the food-safety service, the federal prison service, the airport
# security police and the social-security administration. The same base as
# script 21: new awards, window, province known.
#
# Run: Rscript 26_anclaje_robustez.R

source("theme_house.R", chdir = TRUE)

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia), between(ejercicio, WIN0, WIN1))
grad <- read.csv(file.path(DIR_TAB, "tab_gradiente_provincias.csv"),
                 encoding = "UTF-8")

norm_org <- function(x) chartr("ÁÉÍÓÚÜÑ", "AEIOUUN", toupper(x))

NARROW <- "EJERCITO|ARMADA|FUERZA AEREA|GENDARMERIA|PREFECTURA"
EXTRA  <- paste0("SANIDAD Y CALIDAD AGROALIMENTARIA|SERVICIO PENITENCIARIO|",
                 "SEGURIDAD AEROPORTUARIA|",
                 "ADMINISTRACION NACIONAL DE LA SEGURIDAD SOCIAL")

adj <- adj |> mutate(
  org_n = norm_org(organismo),
  anclado_angosto = grepl(NARROW, org_n),
  anclado_amplio  = anclaje_territorial | grepl(EXTRA, org_n))

cat("organismos captados por el flag angosto:",
    n_distinct(adj$organismo[adj$anclado_angosto]), "\n")
cat("organismos agregados por el flag amplio:",
    n_distinct(adj$organismo[adj$anclado_amplio & !adj$anclaje_territorial]),
    "->", paste(unique(adj$organismo[adj$anclado_amplio &
                                       !adj$anclaje_territorial]),
                collapse = "; "), "\n")

tab <- adj |> group_by(provincia) |>
  summarise(share_actual  = mean(anclaje_territorial),
            share_angosto = mean(anclado_angosto),
            share_amplio  = mean(anclado_amplio), .groups = "drop") |>
  inner_join(grad |> select(provincia, share_anclado, share_persona_fisica),
             by = "provincia")

# the recomputed baseline must reproduce the canonical table, which stores
# three decimals
stopifnot(nrow(tab) == 24,
          max(abs(tab$share_actual - tab$share_anclado)) < 5e-4)

rho <- sapply(c("share_actual", "share_angosto", "share_amplio"), function(v) {
  ct <- suppressWarnings(cor.test(tab[[v]], tab$share_persona_fisica,
                                  method = "spearman"))
  c(rho = unname(ct$estimate), p = ct$p.value)
})
print(round(t(rho), 4))
cat("rango del flag angosto:", round(min(tab$share_angosto), 3), "-",
    round(max(tab$share_angosto), 3), "\n")
cat("rango del flag amplio: ", round(min(tab$share_amplio), 3), "-",
    round(max(tab$share_amplio), 3), "\n")
cat("spearman entre rankings (actual vs angosto, actual vs amplio):",
    round(cor(tab$share_actual, tab$share_angosto, method = "spearman"), 3),
    round(cor(tab$share_actual, tab$share_amplio, method = "spearman"), 3),
    "\n")

out <- tab |> select(provincia, share_actual, share_angosto, share_amplio,
                     share_persona_fisica) |>
  arrange(desc(share_actual)) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))
write_tab(out, "tab_anclaje_robustez.csv")
write_tab(data.frame(flag = colnames(rho), rho = round(rho["rho", ], 3),
                     p = signif(rho["p", ], 3)),
          "tab_anclaje_robustez_rho.csv")
