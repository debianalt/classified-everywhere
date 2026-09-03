# 11 — The geometry of the supplier space (canonical R version).
# Upgrades over the Python pass: GDAtools::speMCA (Le Roux & Rouanet
# orthodoxy), supplementary variables with test values, and REAL
# class-specific MCA (csMCA: subcloud within the reference space) for the
# fracture test — the Python per-stratum MCAs were an approximation.
# Run: Rscript 11_mca_v2.R

source("theme_house.R", chdir = TRUE)
suppressPackageStartupMessages({library(GDAtools); library(cluster)})

set.seed(42)

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
adj_win <- adj |> filter(es_nuevo, !is.na(provincia),
                         between(ejercicio, WIN0, WIN1))

m <- build_supplier_space(adj, master)
X <- active_matrix(m)
cat(sprintf("Espacio %d-%d: %d proveedores, %d categorías activas\n",
            WIN0, WIN1, nrow(m), sum(sapply(X, nlevels))))

RARE <- rare_categories(X)
cat("Categorías pasivadas (<5%):", paste(RARE, collapse = ", "), "\n")
mca <- speMCA(X, excl = excl_index(X, RARE), ncp = 5)
mr <- modif.rate(mca)
mrate <- mr$modif$mrate
benz <- tibble(dim = seq_len(nrow(mr$raw)),
               eigenvalue = mr$raw$eigen,
               pct_raw = mr$raw$rate,
               pct_benzecri = c(mrate, rep(NA, nrow(mr$raw) - length(mrate))))
write_tab(benz, "tab_mca2_benzecri.csv")
cat(sprintf("Benzécri: dim1 %.1f%%, dim2 %.1f%%\n", mrate[1], mrate[2]))

coords <- as.data.frame(mca$ind$coord)[, 1:2] |>
  setNames(c("dim1", "dim2")) |> as_tibble()
m <- bind_cols(m, coords)

# active category coordinates and contributions: the table the Results section
# reads the axes from (signs of MCA axes are arbitrary and can flip between
# runs, so the poles must be re-read from here after every re-run)
# category counts come from the data, not from the model: in a specific MCA
# `var$weight` is a relative weight over the retained categories and cannot be
# read as a frequency
conteos <- unlist(lapply(names(X), function(v) {
  setNames(as.integer(table(X[[v]])), paste0(v, ".", levels(X[[v]])))
}))
cat_tab <- as.data.frame(mca$var$coord)[, 1:2] |>
  setNames(c("dim1", "dim2")) |>
  tibble::rownames_to_column("categoria") |>
  mutate(variable = sub("\\..*$", "", categoria),
         ctr_dim1 = round(mca$var$contrib[, 1], 2),
         ctr_dim2 = round(mca$var$contrib[, 2], 2),
         n = unname(conteos[categoria]),
         pct = round(100 * n / nrow(X), 1)) |>
  arrange(dim1)
stopifnot(!any(is.na(cat_tab$n)))
write_tab(cat_tab |> mutate(across(where(is.numeric), ~round(.x, 3))),
          "tab_mca2_categorias.csv")

# silhouette scan
idx <- sample(nrow(m), min(5000, nrow(m)))
sil <- sapply(3:8, function(k) {
  km <- kmeans(coords, centers = k, nstart = 10)
  mean(silhouette(km$cluster[idx], dist(coords[idx, ]))[, 3])
})
best_k <- (3:8)[which.max(sil)]
cat("silhouette:", paste(3:8, round(sil, 3), sep = "=", collapse = " "),
    "| best k =", best_k, "\n")
m$cluster <- kmeans(coords, centers = best_k, nstart = 10)$cluster

# ── Supplementary variables: coords + test values ────────────────────────────
sup_tab <- bind_rows(lapply(c("S_intensidad", "S_tenure",
                              "S_cohorte", "S_fundamento",
                              "provincia", "estrato"),
                            function(v) {
  sv <- suppressWarnings(varsup(mca, factor(m[[v]])))
  as.data.frame(sv$coord)[, 1:2] |>
    setNames(c("dim1", "dim2")) |>
    mutate(variable = v, categoria = rownames(sv$coord),
           typic_dim1 = as.data.frame(sv$typic)[, 1],
           typic_dim2 = as.data.frame(sv$typic)[, 2])
}))
write_tab(sup_tab, "tab_mca2_supvars.csv")

# ── Provincial centroids per era + dispersion ────────────────────────────────
act <- adj_win |> distinct(cuit, era) |>
  inner_join(m |> select(cuit, provincia, dim1, dim2), by = "cuit")
cent <- act |> group_by(provincia, era) |>
  summarise(dim1 = mean(dim1), dim2 = mean(dim2), .groups = "drop")
write_tab(cent, "tab_mca2_centroides_provincia_era.csv")
disp <- cent |> group_by(era) |>
  summarise(dispersion_centroides =
              sqrt(sum(c(var(dim1), var(dim2)) * (n() - 1) / n())),
            .groups = "drop") |>
  mutate(era = factor(era, levels = ERAS)) |> arrange(era)

# ── Fracture test: REAL csMCA per stratum, congruence of dim1 ────────────────
cs_coords <- function(sel) {
  res <- csMCA(X, subcloud = sel, excl = excl_index(X, RARE), ncp = 3)
  cc <- as.data.frame(res$var$coord)[, 1:3]
  rownames(cc) <- rownames(res$var$coord)
  list(coord = cc, mrate = modif.rate(res)$modif$mrate[1])
}
base <- cs_coords(m$estrato == "core")
# axes of a specific MCA can reorder across subclouds: congruence per metro
# axis = the best absolute correlation over the stratum's dims 1-3, with the
# matching dim reported (full matrix kept in the table).
cg <- bind_rows(lapply(c("core", "middle", "outer"), function(s) {
  r <- cs_coords(m$estrato == s)
  common <- intersect(rownames(base$coord), rownames(r$coord))
  cmat <- abs(cor(base$coord[common, ], r$coord[common, ]))
  tibble(estrato = s, n = sum(m$estrato == s),
         benzecri_dim1 = round(r$mrate, 3),
         congruencia_dim1_bestmatch = round(max(cmat[1, ]), 3),
         dim_que_matchea = which.max(cmat[1, ]),
         congruencia_dim1_vs_dim1 = round(cmat[1, 1], 3),
         congruencia_dim2_bestmatch = round(max(cmat[2, ]), 3))
}))
write_tab(cg, "tab_mca2_congruencia.csv")

# ── Figure ───────────────────────────────────────────────────────────────────
cats <- as.data.frame(mca$var$coord)[, 1:2] |>
  setNames(c("dim1", "dim2")) |>
  mutate(categoria = rownames(mca$var$coord))
provs <- sup_tab |> filter(variable == "provincia")
cent_nea <- cent |> filter(provincia %in% NEA) |>
  mutate(era = factor(era, levels = ERAS))
m$region <- factor(region_of(m$provincia), levels = REGION_LEVELS)

p <- ggplot(m |> slice_sample(n = 8000), aes(dim1, dim2)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_point(size = 0.4, alpha = 0.12, colour = "grey60") +
  stat_ellipse(aes(colour = region), level = 0.5, linewidth = 0.6) +
  geom_text(data = cats, aes(label = substr(categoria, 1, 24)),
            size = 2.5, colour = "#1f4e79", fontface = "bold") +
  geom_point(data = provs, aes(shape = categoria %in% NEA),
             size = 2, colour = "#d62728") +
  ggrepel::geom_text_repel(
    data = provs |> filter(categoria %in% c(NEA, "CABA", "Buenos Aires",
                                            "Córdoba")),
    aes(label = categoria), size = 2.4, colour = "#a00000", seed = 42) +
  geom_point(data = cent_nea, aes(fill = era), shape = 21, size = 2.4,
             colour = "#7b241c") +
  scale_colour_manual(values = REGION_COLS) +
  scale_shape_manual(values = c(`TRUE` = 15, `FALSE` = 0), guide = "none") +
  labs(x = sprintf("Dim 1 (%.0f%% Benzécri)", mr$mrate[1]),
       y = sprintf("Dim 2 (%.0f%% Benzécri)", mr$mrate[2]),
       title = "El espacio social de los proveedores del Estado (speMCA)",
       subtitle = "Categorías activas; provincias suplementarias; elipses de concentración por región; centroides NEA por era") +
  theme_house()
# diagnostic render, not a submission figure: it goes to the archive so that
# figures/ holds only the twelve figures of the manuscript
save_fig(p, "../_archive/figures_working/fig_mca2_espacio.png",
         width = 10, height = 8)

cat("\nDispersión de centroides por era:\n"); print(disp)
cat("\nCongruencia dim1 vs metro (csMCA real):\n"); print(cg)
cat("\nSuplementarias con |typic| máximo en dim2 (lectura del eje):\n")
print(sup_tab |> filter(variable != "provincia") |>
        arrange(desc(abs(typic_dim2))) |> head(6) |>
        mutate(across(where(is.numeric), ~round(.x, 2))))
