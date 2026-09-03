# 36 — Trajectories in the space (author's decision, 29 Aug 2026).
#
# The Cox apparatus leaves the body: survival modelling is foreign to the
# social-space tradition, whilst trajectories in the plane are its native
# instrument. Here each supplier active in two or more presidencies gets an
# era-specific profile on the time-varying actives (modal client, procedure
# profile, scale at the PUBLISHED global tercile cuts) with its constant
# actives (legal form, registration cohort), and is projected as a
# supplementary individual into the published space. Displacement along the
# axis of access answers the jury's questions geometrically:
#   (a) do suppliers tied to anchored organs at entry move less along the
#       axis of access?
#   (b) do those who survive into the third presidency drift towards the
#       corporate pole?
#   (c) how does displacement differ by stratum?
#
# Measurable-trajectory caveat (declared in S27 and the body): a natural
# person who incorporates takes a new taxpayer identifier, so it appears as
# an exit plus an entry; the displacement measured here is that of the same
# identifier, within its legal form.
#
# Run: Rscript 36_trayectorias_espacio.R

source("theme_house.R", chdir = TRUE)
suppressPackageStartupMessages({library(GDAtools); library(ggrepel)
  library(patchwork)})

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
m <- build_supplier_space(adj, master)
X <- active_matrix(m)
RARE <- rare_categories(X)
mca <- speMCA(X, excl = excl_index(X, RARE), ncp = 2)
mr <- modif.rate(mca)$modif$mrate
stopifnot(abs(mr[1] - 43.6) < 0.15, abs(mr[2] - 33.0) < 0.15)

# sanity: supplementary projection of the ACTIVE rows must reproduce the
# active coordinates (correlation ~ 1), or the projection is wrong
sup_chk <- supind(mca, X[1:2000, ])
stopifnot(cor(sup_chk$coord[, 2], mca$ind$coord[1:2000, 2]) > 0.999)

# ── global cuts of the published scale variable ──────────────────────────────
ipc <- read.csv(file.path(DIR_PROC, "ipc_anual.csv"))
w <- adj |> filter(es_nuevo, !is.na(provincia),
                   between(ejercicio, WIN0, WIN1), moneda == "ARS") |>
  left_join(ipc |> select(ejercicio = anio, deflactor_2024),
            by = "ejercicio") |>
  mutate(real = monto * deflactor_2024)
cortes_escala <- quantile(
  w |> group_by(cuit) |> summarise(med = median(real), .groups = "drop") |>
    pull(med), c(1 / 3, 2 / 3))

# ── era profiles on the time-varying actives ─────────────────────────────────
era_prof <- w |>
  group_by(cuit, era) |>
  summarise(
    n_adj = n(),
    A_cliente = names(sort(table(tipo_organismo), decreasing = TRUE))[1],
    p_dir = mean(grepl("Directa", procedimiento, ignore.case = TRUE)),
    med_real = median(real), .groups = "drop") |>
  mutate(
    A_directa = cut(p_dir, c(-Inf, 1e-9, 0.5, Inf),
                    labels = c("sin_directa", "mixto", "mayoria_directa")),
    A_escala = cut(med_real, c(-Inf, cortes_escala, Inf),
                   labels = c("chica", "media", "grande")))

fijo <- m |> transmute(cuit = as.character(cuit), A_personeria, A_registro,
                       estrato)
tray <- era_prof |> mutate(cuit = as.character(cuit)) |>
  inner_join(fijo, by = "cuit") |>
  group_by(cuit) |> filter(n() >= 2) |> ungroup()
cat(sprintf("proveedores con 2+ eras: %s | perfiles era: %s\n",
            format(length(unique(tray$cuit)), big.mark = ","),
            format(nrow(tray), big.mark = ",")))

# same collapsing rule as build_supplier_space: top-8 sectors truncated to 18
# characters, the rest to OTROS — otherwise era-profiles fall outside the
# published factor levels and drop as NA
top_rubros_pub <- sub("^A_rubro.", "", fixed = TRUE,
                      grep("^A_rubro.", fixed = TRUE, rownames(mca$var$coord),
                           value = TRUE))
rubro_era <- w |> group_by(cuit, era) |>
  summarise(rubro_modal = names(sort(table(rubro_principal),
                                     decreasing = TRUE))[1],
            .groups = "drop") |>
  mutate(cuit = as.character(cuit),
         A_rubro = ifelse(substr(rubro_modal, 1, 18) %in% top_rubros_pub,
                          substr(rubro_modal, 1, 18), "OTROS")) |>
  select(cuit, era, A_rubro)
tray <- tray |> left_join(rubro_era, by = c("cuit", "era"))

Xe <- tray |> transmute(
  A_personeria = factor(A_personeria, levels = levels(X$A_personeria)),
  A_rubro = factor(A_rubro, levels = levels(X$A_rubro)),
  A_cliente = factor(A_cliente, levels = levels(X$A_cliente)),
  A_escala = factor(A_escala, levels = levels(X$A_escala)),
  A_directa = factor(A_directa, levels = levels(X$A_directa)),
  A_registro = factor(A_registro, levels = levels(X$A_registro))) |>
  as.data.frame()
Xe <- Xe[, names(X)]
ok <- complete.cases(Xe)
cat(sprintf("perfiles proyectables: %s (%.1f%%)\n",
            format(sum(ok), big.mark = ","), 100 * mean(ok)))

proy <- supind(mca, Xe[ok, ])
tray_ok <- tray[ok, ] |>
  mutate(dim1 = proy$coord[, 1], dim2 = proy$coord[, 2])

# ── displacement first -> last observed era ──────────────────────────────────
ORD <- setNames(seq_along(ERAS), ERAS)
anclada <- adj |> filter(es_nuevo, between(ejercicio, WIN0, WIN1)) |>
  group_by(cuit) |> filter(ejercicio == min(ejercicio)) |>
  summarise(anclado_entrada = mean(anclaje_territorial) > 0.5,
            .groups = "drop") |> mutate(cuit = as.character(cuit))

d <- tray_ok |>
  mutate(o = ORD[era]) |>
  group_by(cuit, A_personeria, A_registro, estrato) |>
  arrange(o, .by_group = TRUE) |>
  summarise(eras = n(), llega_milei = any(era == "milei"),
            d1_ini = first(dim1), d2_ini = first(dim2),
            d1_fin = last(dim1), d2_fin = last(dim2), .groups = "drop") |>
  mutate(delta2 = d2_fin - d2_ini, delta1 = d1_fin - d1_ini) |>
  left_join(anclada, by = "cuit") |>
  filter(!is.na(anclado_entrada))
cat(sprintf("trayectorias medidas: %s\n", format(nrow(d), big.mark = ",")))

res <- d |>
  mutate(grupo_ancla = ifelse(anclado_entrada, "anclado", "no_anclado"),
         grupo_llega = ifelse(llega_milei, "llega_a_milei", "no_llega")) |>
  group_by(grupo_ancla, grupo_llega) |>
  summarise(n = n(),
            abs_d2_mediana = round(median(abs(delta2)), 3),
            d2_mediana = round(median(delta2), 3),
            hacia_corporativo = round(mean(delta2 > 0), 3),
            .groups = "drop")
write_tab(res, "tab_trayectorias_grupos.csv")
cat("\ndesplazamiento en el eje de acceso (dim2), por grupo:\n")
print(as.data.frame(res), row.names = FALSE)

por_estrato <- d |> group_by(estrato) |>
  summarise(n = n(), abs_d2_mediana = round(median(abs(delta2)), 3),
            d2_mediana = round(median(delta2), 3), .groups = "drop")
write_tab(por_estrato, "tab_trayectorias_estrato.csv")
print(as.data.frame(por_estrato), row.names = FALSE)

# stability of position: share whose |delta2| stays under a quarter SD of axis
sd2 <- sd(mca$ind$coord[, 2])
esta <- d |> mutate(grupo_ancla = ifelse(anclado_entrada, "anclado",
                                         "no_anclado")) |>
  group_by(grupo_ancla) |>
  summarise(n = n(), quieto_025sd = round(mean(abs(delta2) < 0.25 * sd2), 3),
            .groups = "drop")
write_tab(esta, "tab_trayectorias_quietud.csv")
cat(sprintf("\nSD del eje 2: %.3f; quietos (<0.25 SD):\n", sd2))
print(as.data.frame(esta), row.names = FALSE)

# ── Figure 5: mean trajectory arrows in the plane ────────────────────────────
cats <- read.csv(file.path(DIR_TAB, "tab_c10_categorias.csv")) |>
  mutate(etiqueta = CAT_LABELS[categoria])
flechas <- d |>
  mutate(grupo = paste0(ifelse(anclado_entrada, "Anchored at entry",
                               "Other entry"),
                        ifelse(llega_milei, ", persists", ", exits"))) |>
  group_by(grupo) |>
  summarise(n = n(), x0 = mean(d1_ini), y0 = mean(d2_ini),
            x1 = mean(d1_fin), y1 = mean(d2_fin), .groups = "drop")
write_tab(flechas |> mutate(across(where(is.numeric), ~round(.x, 3))),
          "tab_trayectorias_flechas.csv")

# the house three-level grey ladder (as in theme_house.R): the trajectory
# series take the two dark levels and the background cloud the light one. The
# earlier grey of the "Other" series was #a3a3a3 against a #a6a6a6 cloud —
# the same value for data and background, which is what read as washed out.
# validate_palette.js on #1a1a1a,#666666,#999999: worst adjacent pair
# #999999 vs #666666, deltaE 17.3 normal and 17.3 deutan/tritan, above the 15
# floor. Its chroma and lightness-band failures ask for colour, which the
# journal does not print; the contrast warning on #999999 is answered by the
# direct label every cloud point carries.
TRAJ_COLS <- c("Anchored at entry, persists" = "#1a1a1a",
               "Anchored at entry, exits" = "#1a1a1a",
               "Other entry, persists" = "#666666",
               "Other entry, exits" = "#666666")
# a dash pattern needs length to show itself and the mean displacements have
# none, so exits/persists is carried by the tail marker, not the line type
TRAJ_SHAPES <- c("Anchored at entry, persists" = 16,
                 "Anchored at entry, exits" = 1,
                 "Other entry, persists" = 16,
                 "Other entry, exits" = 1)
# Two panels, because one cannot carry both facts. The four arrows are mean
# displacements of groups, and at the scale of the plane they are a few pixels
# long: that near-invisibility is itself the finding, so panel (a) keeps true
# scale. But the reader also has to see WHAT the arrows do, and the first
# version marked them with rings — which in a cloud of this tradition read as
# concentration ellipses, the device Figure S6 actually uses. The marks are now
# rectangles, the idiom Figure 2 already uses for its inset, and panels (b) and
# (c) magnify them at one common scale.
ventana <- function(v) {
  cx <- mean(c(v$x0, v$x1)); cy <- mean(c(v$y0, v$y1))
  list(cx = cx, cy = cy,
       ext = max(diff(range(c(v$x0, v$x1))), diff(range(c(v$y0, v$y1)))))
}
w_anc <- ventana(flechas[grepl("^Anchored", flechas$grupo), ])
w_otr <- ventana(flechas[grepl("^Other", flechas$grupo), ])
# one half-width for both windows, so the two magnifications are equal and the
# panels can be compared by eye
HALF <- max(w_anc$ext, w_otr$ext) / 2 + 0.035

# Labels must stay off the marked windows: a label box or a leader line
# crossing one hides the arrows it is meant to point at. repel avoids points,
# and an empty rectangle holds none, so the obstacle is a grid that fills it.
caja <- function(w, n = 5) {
  g <- expand.grid(dx = seq(-HALF, HALF, length.out = n),
                   dy = seq(-HALF, HALF, length.out = n))
  data.frame(dim.1 = w$cx + g$dx, dim.2 = w$cy + g$dy, etiqueta = "")
}
# Exactly one category falls inside each window ("Other sectors" and "No
# direct contracting"). Its label has to be placed outside, so its leader line
# must cross the box whatever repel does. Those two are labelled in the
# magnified panel instead, where there is room and where the label also tells
# the reader what the arrows sit next to; panel (a) keeps their points.
dentro <- function(w) {
  abs(cats$dim.1 - w$cx) <= HALF & abs(cats$dim.2 - w$cy) <= HALF
}
en_ventana <- dentro(w_anc) | dentro(w_otr)
cats_a <- cats[, c("dim.1", "dim.2", "etiqueta")]
cats_a$etiqueta[en_ventana] <- ""
cats_rep <- rbind(cats_a, caja(w_anc), caja(w_otr))

# the grid keeps the boxes out; the nudge keeps the leader lines out too, by
# pushing a nearby category away from the window it sits on before repel starts
R_LIBRE <- HALF + 0.22
radial <- function(x, y) {
  d_a <- sqrt((x - w_anc$cx)^2 + (y - w_anc$cy)^2)
  d_o <- sqrt((x - w_otr$cx)^2 + (y - w_otr$cy)^2)
  cx <- ifelse(d_a < d_o, w_anc$cx, w_otr$cx)
  cy <- ifelse(d_a < d_o, w_anc$cy, w_otr$cy)
  d <- pmin(d_a, d_o)
  empuje <- pmax(R_LIBRE - d, 0)
  ang <- atan2(y - cy, x - cx)
  cbind(empuje * cos(ang), empuje * sin(ang))
}
nud <- radial(cats_rep$dim.1, cats_rep$dim.2)

marco <- function(w) {
  annotate("rect", xmin = w$cx - HALF, xmax = w$cx + HALF,
           ymin = w$cy - HALF, ymax = w$cy + HALF,
           fill = NA, colour = "#7f7f7f", linewidth = 0.3)
}

f5a <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.3) +
  geom_point(data = cats, aes(dim.1, dim.2), colour = "#999999", size = 1.1) +
  # leader lines on the same settings as Fig 1: at the default
  # min.segment.length a displaced label reaches the page with no line to its
  # point, and "Registered 2016-18" then reads as the label of SRL.
  # max.overlaps is Inf because the obstacle rows raise the overlap count and a
  # finite limit would drop real labels to make room for empty ones.
  geom_text_repel(data = cats_rep, aes(dim.1, dim.2, label = etiqueta),
                  size = 2.0, colour = "#4d4d4d", seed = 7,
                  max.overlaps = Inf, max.iter = 100000,
                  force = 6, force_pull = 0.35,
                  nudge_x = nud[, 1], nudge_y = nud[, 2],
                  min.segment.length = 0.15, box.padding = 0.34,
                  point.padding = 0.10,
                  segment.colour = "grey55", segment.size = 0.2) +
  marco(w_anc) + marco(w_otr) +
  # the segment takes no legend: with two layers mapping to the same variable
  # ggplot merges the guides and stacks an arrow glyph over the tail marker in
  # every key. The marker alone carries both distinctions.
  geom_segment(data = flechas,
               aes(x = x0, y = y0, xend = x1, yend = y1, colour = grupo),
               linewidth = 0.9, show.legend = FALSE,
               arrow = arrow(length = unit(2.5, "pt"), type = "closed")) +
  geom_point(data = flechas,
             aes(x0, y0, colour = grupo, shape = grupo),
             size = 1.6, stroke = 0.6) +
  scale_colour_manual(values = TRAJ_COLS, name = NULL) +
  scale_shape_manual(values = TRAJ_SHAPES, name = NULL) +
  # the finding is the length of the arrows, which is only readable if the two
  # axes are at the same scale
  coord_fixed() +
  labs(x = gda_axis_labs(mr[1], mr[2])[1],
       y = gda_axis_labs(mr[1], mr[2])[2], tag = "a") +
  # override.aes goes on one guide only: the two merge, and repeating it there
  # only raises a "duplicated override.aes" warning
  guides(colour = guide_legend(nrow = 2, override.aes = list(size = 2.2)),
         shape = guide_legend(nrow = 2)) +
  theme_house()

# the magnified windows. Both carry the same half-width, so one magnification
# serves both, and the ticks carry the units: that is the scale this needs.
sobre_flechas <- function(n = 7) {
  do.call(rbind, lapply(seq_len(nrow(flechas)), function(i) {
    s <- seq(0, 1, length.out = n)
    data.frame(dim.1 = flechas$x0[i] + s * (flechas$x1[i] - flechas$x0[i]),
               dim.2 = flechas$y0[i] + s * (flechas$y1[i] - flechas$y0[i]),
               etiqueta = "")
  }))
}

zoom <- function(w, titulo, tag) {
  ggplot() +
    geom_point(data = cats, aes(dim.1, dim.2), colour = "#cccccc", size = 1.1) +
    # the same lesson as panel (a), one scale down: the label has to be kept
    # off the arrows, and repel only avoids points, so the arrows are sampled
    # into empty-label rows that it must route around
    geom_text_repel(data = rbind(cats[dentro(w), c("dim.1", "dim.2", "etiqueta")],
                                 sobre_flechas()),
                    aes(dim.1, dim.2, label = etiqueta),
                    size = 2.0, colour = "#4d4d4d", seed = 7,
                    max.overlaps = Inf, max.iter = 40000, force = 5,
                    box.padding = 0.5, point.padding = 0.2,
                    min.segment.length = 0.15, segment.colour = "grey55",
                    segment.size = 0.2) +
    geom_segment(data = flechas,
                 aes(x = x0, y = y0, xend = x1, yend = y1, colour = grupo),
                 linewidth = 0.7, show.legend = FALSE,
                 arrow = arrow(length = unit(4, "pt"), type = "closed")) +
    geom_point(data = flechas, aes(x0, y0, colour = grupo, shape = grupo),
               size = 2.0, stroke = 0.7, show.legend = FALSE) +
    scale_colour_manual(values = TRAJ_COLS) +
    scale_shape_manual(values = TRAJ_SHAPES) +
    scale_x_continuous(n.breaks = 3) +
    scale_y_continuous(n.breaks = 3) +
    coord_fixed(xlim = c(w$cx - HALF, w$cx + HALF),
                ylim = c(w$cy - HALF, w$cy + HALF), expand = FALSE) +
    labs(x = NULL, y = NULL, title = titulo, tag = tag) +
    theme_house()
}

f5 <- (f5a | (zoom(w_anc, "Anchored at entry", "b") /
                zoom(w_otr, "Other entry", "c"))) +
  plot_layout(widths = c(2, 1), guides = "collect") &
  theme(legend.position = "bottom")

pub(f5, "5", 4.6)
