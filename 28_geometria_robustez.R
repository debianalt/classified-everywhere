# 28 — Geometric shields: the cloud with concentration ellipses (Figure S6),
# the third axis, and the enlarged-battery robustness check (Table S22).
#
# Three outputs for the GDA-strict reader, none of which touches the published
# solution (asserted below):
#   (a) Figure S6 — the cloud of individuals on axes 1-2 with kappa = 2
#       concentration ellipses (86.47% under normality) and centroids by
#       stratum. The ellipses overlap heavily and the centroids order
#       cleanly: composition, not separation, is the finding.
#   (b) The third axis, reported in the supplement: category coordinates and
#       contributions, stratum positions and test values.
#   (c) A four-variable specification adding the SIPRO registration cohort
#       (fecha_preinscripcion, 100% coverage) to the active battery, with the
#       same 5% passivation rule. The declared activity (56.5% coverage,
#       confounded with legal form) and the ARCA founding year (companies
#       only) cannot be active; trajectories must not be. Axis-by-axis
#       correlation of individual coordinates against the published solution.
#
# Run: Rscript 28_geometria_robustez.R

source("theme_house.R", chdir = TRUE)
suppressMessages(library(GDAtools))

master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
m <- build_supplier_space(adj, master)

X <- active_matrix(m)
excl <- rare_categories(X)
mca <- speMCA(X, excl = excl_index(X, excl), ncp = 3)
mr <- modif.rate(mca)$modif$mrate
stopifnot(abs(mr[1] - 43.6) < 0.15, abs(mr[2] - 33.0) < 0.15)  # C10 canon intact

# ── (a) Figure S6: the cloud with concentration ellipses by stratum ─────────────────
ind <- as.data.frame(mca$ind$coord[, 1:2])
names(ind) <- c("d1", "d2")
ind$Stratum <- factor(STRATUM_LABELS[m$estrato], levels = STRATUM_LABELS)
cent <- ind |> group_by(Stratum) |>
  summarise(d1 = mean(d1), d2 = mean(d2), .groups = "drop")
# individuals stack on identical profiles: draw the lattice of distinct
# positions, sized by how many suppliers share the profile
perfiles <- ind |> count(d1 = round(d1, 2), d2 = round(d2, 2))

# the cloud and its centroids become canonical tables so that script 13 can
# draw Figure 1a without recomputing the space: 2,263 distinct profiles carry
# all 10,580 individuals
write_tab(perfiles, "tab_c10_nube_perfiles.csv")
write_tab(cent |> mutate(across(where(is.numeric), ~round(.x, 4))),
          "tab_c10_centroides.csv")

STRATUM_LTY <- c(Core = "solid", Middle = "dashed", Outer = "dotted")
p <- ggplot(ind, aes(d1, d2)) +
  geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.25) +
  geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.25) +
  geom_point(data = perfiles, aes(size = n), colour = "grey70",
             alpha = 0.55, shape = 16, show.legend = FALSE) +
  scale_size_area(max_size = 2.4) +
  stat_ellipse(aes(colour = Stratum, linetype = Stratum),
               type = "norm", level = 0.8647, linewidth = 0.5) +
  geom_point(data = cent, aes(shape = Stratum), size = 2, colour = "#1a1a1a") +
  ggrepel::geom_text_repel(data = cent, aes(label = Stratum),
                           colour = "#333333", size = 2.6, seed = 7,
                           nudge_x = c(-0.25, 0.3, -0.25),
                           nudge_y = c(-0.12, 0.1, 0.18),
                           segment.colour = "grey60",
                           segment.linewidth = 0.2) +
  scale_colour_manual(values = stratum_pal()) +
  scale_linetype_manual(values = STRATUM_LTY) +
  scale_shape_manual(values = c(Core = 16, Middle = 17, Outer = 15)) +
  coord_fixed() +
  labs(x = gda_axis_labs(mr[1], mr[2])[1],
       y = gda_axis_labs(mr[1], mr[2])[2]) +
  guides(shape = "none",
         colour = guide_legend(NULL), linetype = guide_legend(NULL)) +
  theme_house()

pub(p, "S6", height = 4.5)

# ── (b) the third axis ───────────────────────────────────────────────────────
ej3 <- data.frame(categoria = rownames(mca$var$coord),
                  coord3 = round(mca$var$coord[, 3], 2),
                  ctr3 = round(mca$var$contrib[, 3], 1)) |>
  arrange(desc(ctr3))
write_tab(ej3, "tab_eje3_categorias.csv")

sv <- supvar(mca, factor(m$estrato, levels = c("core", "middle", "outer")))
estr3 <- data.frame(estrato = rownames(sv$coord),
                    round(sv$coord[, 1:3], 3),
                    typic3 = round(sv$typic[, 3], 1))
write_tab(estr3, "tab_eje3_estratos.csv")

# (the former part (c), a four-variable check adding the registration cohort,
# was superseded on 29 Aug 2026 when the cohort entered the canonical battery)

cat("\nBenzecri canon (C10):", round(mr[1:3], 1), "\n")
cat("\nEje 3 — categorias por contribucion:\n")
print(head(ej3, 8), row.names = FALSE)
cat("\nEstratos, ejes 1-3 (coord y vtest3):\n"); print(estr3, row.names = FALSE)
