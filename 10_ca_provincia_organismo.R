# 10 — Correspondence analysis: territorial supply x state demand
# (canonical R port of 10_ca_provincia_organismo.py; FactoMineR::CA).
# Axis signs are arbitrary; comparisons with the Python pass are structural.
# Run: Rscript 10_ca_provincia_organismo.R

source("theme_house.R", chdir = TRUE)
suppressPackageStartupMessages({library(FactoMineR); library(ggrepel)})

# window: before 2019 the platform holds a different set of buyers, so an era
# comparison across the rollout would compare states, not markets. The Macri
# era is therefore represented by 2019 alone (see 20_platform_stability.R).
adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia), between(ejercicio, WIN0, WIN1))

run_ca <- function(tab) CA(tab, ncp = 2, graph = FALSE)

coords_all <- list()
plots <- list()
for (e in ERAS) {
  xt <- adj |> filter(era == e) |> count(provincia, tipo_organismo) |>
    pivot_wider(names_from = tipo_organismo, values_from = n,
                values_fill = 0) |>
    as.data.frame()
  rownames(xt) <- xt$provincia; xt$provincia <- NULL
  xt <- xt[rowSums(xt) >= 30, ]
  ca <- run_ca(as.matrix(xt))
  rows <- as.data.frame(ca$row$coord)[, 1:2]
  cols <- as.data.frame(ca$col$coord)[, 1:2]
  names(rows) <- names(cols) <- c("dim1", "dim2")
  inertia <- ca$eig[1:2, 2]
  coords_all[[e]] <- bind_rows(
    rows |> mutate(era = e, punto = rownames(rows), clase = "provincia"),
    cols |> mutate(era = e, punto = rownames(cols), clase = "tipo_organismo")) |>
    mutate(inercia1 = round(inertia[1], 1), inercia2 = round(inertia[2], 1))
  d <- coords_all[[e]] |>
    mutate(nea = clase == "provincia" & punto %in% NEA)
  plots[[e]] <- ggplot(d, aes(dim1, dim2)) +
    geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
    geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.3) +
    geom_point(data = subset(d, clase == "provincia"),
               aes(colour = nea, size = nea)) +
    geom_point(data = subset(d, clase == "tipo_organismo"),
               shape = 17, size = 3, colour = "#2ca02c") +
    geom_text_repel(aes(label = punto,
                        fontface = ifelse(clase == "tipo_organismo",
                                          "bold", "plain")),
                    size = 2.3, max.overlaps = 30, seed = 42) +
    scale_colour_manual(values = c(`TRUE` = "#d62728", `FALSE` = "#7fb3d5"),
                        guide = "none") +
    scale_size_manual(values = c(`TRUE` = 2.6, `FALSE` = 1.6), guide = "none") +
    labs(title = sprintf("%s (dim1 %.0f%%, dim2 %.0f%%)", e,
                         inertia[1], inertia[2]),
         x = NULL, y = NULL) +
    theme_house(9)
}
tab <- bind_rows(coords_all) |>
  select(era, punto, clase, dim1, dim2, inercia1, inercia2)
write_tab(tab, "tab_ca_coords_provincia_tipo.csv")

library(patchwork)
p <- plots[[1]] + plots[[2]] + plots[[3]] +
  patchwork::plot_annotation(
    title = "CA: provincias (oferta territorial) × tipos de organismo (demanda estatal)")
save_fig(p, "../_archive/figures_working/fig_ca_provincia_tipo.png",
         width = 16, height = 5.4)

# ── CA rubro x tipo (pooled) ─────────────────────────────────────────────────
top_rubros <- adj |> count(rubro_principal, sort = TRUE) |>
  slice_head(n = 20) |> pull(rubro_principal)
xt2 <- adj |> filter(rubro_principal %in% top_rubros) |>
  count(rubro_principal, tipo_organismo) |>
  pivot_wider(names_from = tipo_organismo, values_from = n, values_fill = 0) |>
  as.data.frame()
rownames(xt2) <- xt2$rubro_principal; xt2$rubro_principal <- NULL
ca2 <- run_ca(as.matrix(xt2))
r2 <- as.data.frame(ca2$row$coord)[, 1:2] |>
  setNames(c("dim1", "dim2")) |>
  mutate(punto = substr(rownames(ca2$row$coord), 1, 22), clase = "rubro")
c2 <- as.data.frame(ca2$col$coord)[, 1:2] |>
  setNames(c("dim1", "dim2")) |>
  mutate(punto = rownames(ca2$col$coord), clase = "tipo")
p2 <- ggplot(bind_rows(r2, c2), aes(dim1, dim2)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_point(aes(shape = clase, colour = clase, size = clase)) +
  geom_text_repel(aes(label = punto, fontface = ifelse(clase == "tipo",
                                                       "bold", "plain")),
                  size = 2.3, max.overlaps = 40, seed = 42) +
  scale_shape_manual(values = c(rubro = 16, tipo = 17), guide = "none") +
  scale_colour_manual(values = c(rubro = "#7f7f7f", tipo = "#2ca02c"),
                      guide = "none") +
  scale_size_manual(values = c(rubro = 1.6, tipo = 3), guide = "none") +
  labs(title = sprintf("CA: rubros × tipos de organismo (dim1 %.0f%%, dim2 %.0f%%)",
                       ca2$eig[1, 2], ca2$eig[2, 2]),
       x = NULL, y = NULL) +
  theme_house()
save_fig(p2, "../_archive/figures_working/fig_ca_rubro_tipo.png",
         width = 9, height = 7)

cat("\nDistancia NEA al polo seguridad_defensa (por era):\n")
for (e in ERAS) {
  d <- coords_all[[e]]
  pole <- d |> filter(punto == "seguridad_defensa")
  neap <- d |> filter(clase == "provincia", punto %in% NEA)
  dist <- sqrt((neap$dim1 - pole$dim1)^2 + (neap$dim2 - pole$dim2)^2)
  cat(sprintf("  %-10s %s\n", e,
              paste(sprintf("%s=%.2f", neap$punto, dist), collapse = "  ")))
}
