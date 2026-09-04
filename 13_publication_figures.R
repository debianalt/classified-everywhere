# 13 — Publication figures (house rules: Fig<N> file names, TIFF+PNG,
# identical fonts and legend orders, analysis window everywhere).
# Regenerates from canonical tables/ — no recomputation.
#
# Numbering for the BJS submission (figures sit on separate sheets and do not
# count against the 8,000-word limit):
#   Fig1  the supplier space: active categories, and the jurisdictions in it
#   Fig2  the cloud of individuals with concentration ellipses (script 28)
#   Fig3  the territorial gradient mapped: anchored demand and the demand
#         that produces it, as a choropleth and the units at their seats
#   Fig3  the gradient of access, and the procedure gap that widens along it
#   Fig4  the geography of the exchange (script 35)
#   Fig5  trajectories in the space (script 36)
#   Fig6  what the record supports and what it does not: the between-province
#         series on all buyers and on a balanced panel of buyers
# Figures are numbered in order of appearance (house rule). The local names
# f3 and f4 in this script build Fig3 and Fig6; the renumberings moved the
# tags, not the variables.
#   FigS1 correspondence analysis of provinces x buyer types, per era
#   FigS2 cohort persistence stratified by client anchoring
# (FigS3 and FigS4 are written by scripts 17 and 18.)
#
# Run: Rscript 13_publication_figures.R

source("theme_house.R", chdir = TRUE)
suppressPackageStartupMessages({library(patchwork); library(ggrepel)
  library(sf)})

# Figures are authored at final print width (W_DOUBLE / W_SINGLE in
# theme_house.R) with type at final size, so nothing is reduced on the page.
# pub() and gda_axis_labs() live in theme_house.R: every submission figure,
# in this script and in 28, 35 and 36, is written by the same function.

grad <- read.csv(file.path(DIR_TAB, "tab_gradiente_provincias.csv")) |>
  mutate(grupo = factor(grupo, levels = GROUP_LEVELS))

# ── Fig 1: the supplier space (specific MCA), four panels ──────────────────
# The canonical order of the tradition: the cloud of individuals first, then
# the categories that give the axes their meaning, then the supplementary
# elements read in the space they took no part in building.
benz <- read.csv(file.path(DIR_TAB, "tab_mca2_benzecri.csv"))
sup <- read.csv(file.path(DIR_TAB, "tab_mca2_supvars.csv"))
cats <- read.csv(file.path(DIR_TAB, "tab_mca2_categorias.csv")) |>
  mutate(etiqueta = CAT_LABELS[categoria],
         grupo = factor(VAR_LABELS[variable], levels = VAR_LABELS))
stopifnot(!any(is.na(cats$etiqueta)))

ejes <- c(sprintf("Dimension 1 (%.1f%%, Benzécri)", benz$pct_benzecri[1]),
          sprintf("Dimension 2 (%.1f%%, Benzécri)", benz$pct_benzecri[2]))

f1b <- ggplot(cats, aes(dim1, dim2, colour = grupo, shape = grupo)) +
  geom_hline(yintercept = 0, colour = "grey75", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey75", linewidth = 0.3) +
  geom_point(aes(size = n), alpha = 0.9) +
  geom_text_repel(aes(label = etiqueta), size = 2.1, seed = 3,
                  colour = "#333333", max.overlaps = 40,
                  force = 3, force_pull = 0.5, max.iter = 20000,
                  point.padding = 0.25,
                  min.segment.length = 0.15, segment.size = 0.2,
                  segment.colour = "grey65", box.padding = 0.40,
                  show.legend = FALSE) +
  scale_colour_manual(values = var_pal(), name = NULL) +
  scale_shape_manual(values = VAR_SHAPES, name = NULL) +
  # the size encoding is decodable: an unlabelled size ramp is not
  scale_size_area(max_size = 4, name = "Suppliers",
                  breaks = c(1000, 3000, 5000),
                  labels = c("1,000", "3,000", "5,000"),
                  limits = c(0, 6000)) +
  # each panel carries its own legend into half the text width. Two rows for
  # the six variables, as in panels (c) and (d), so the three legend blocks of
  # the figure end at the same height (3 Sep 2026). No size legend: nine keys
  # do not fit in two rows of half the text width (tried: the size row was
  # clipped at the right edge), and panel (a) already sets the convention that
  # point area is proportional to the number of suppliers, said in the caption;
  # the counts per category are in Table S8.
  guides(colour = guide_legend(order = 1, nrow = 2,
                               override.aes = list(size = 2)),
         shape = guide_legend(order = 1, nrow = 2,
                              override.aes = list(size = 2)),
         size = "none") +
  # principal coordinates: both axes carry the same unit, so the plane is
  # drawn isometrically. Without this the eye reads distances that the
  # geometry does not contain (Le Roux and Rouanet 2010). The active
  # categories already spread almost squarely, so no limit padding here:
  # squaring this panel shrinks the scale and the centre-left labels collide.
  coord_fixed() +
  labs(x = ejes[1], y = ejes[2], tag = "b") +
  theme_house()

provs <- sup |> filter(variable == "provincia") |>
  left_join(grad |> select(categoria = provincia, grupo), by = "categoria") |>
  mutate(grupo = factor(ifelse(is.na(as.character(grupo)), "Rest of country",
                               as.character(grupo)), levels = GROUP_LEVELS))
f1c <- ggplot(provs, aes(dim1, dim2, colour = grupo, fill = grupo,
                         shape = grupo)) +
  geom_hline(yintercept = 0, colour = "grey75", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey75", linewidth = 0.3) +
  geom_point(size = 1.7, stroke = 0.4) +
  geom_text_repel(aes(label = categoria), size = 2.1, seed = 11,
                  colour = "#333333", max.overlaps = 40, force = 4, force_pull = 0.5,
                  point.padding = 0.25,
                  max.iter = 20000,
                  min.segment.length = 0.15, segment.size = 0.2,
                  segment.colour = "grey65", box.padding = 0.36,
                  show.legend = FALSE) +
  scale_colour_manual(values = group_pal(), name = NULL) +
  scale_fill_manual(values = group_pal(), name = NULL) +
  # shape carries region as well as hue, so the six survive greyscale printing
  scale_shape_manual(values = GROUP_SHAPES, name = NULL) +
  guides(colour = guide_legend(nrow = 2), fill = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2)) +
  # isometric like panel (a). The supplementary points occupy a much smaller
  # region than the active categories, so the panel is at a closer zoom; the
  # caption says so. The jurisdictions spread twice as far on axis 2 as on
  # axis 1, which under a fixed aspect leaves a tall thin panel with no room
  # for labels, so the x range is padded to the y range: more empty plane,
  # same scale on both axes.
  # pad 0.30, not the default 0.10: the jurisdictions crowd the middle of the
  # plane, and with the frame tight around them the labels have nowhere to go
  # — Neuquén and Río Negro then print on top of each other whatever seed
  # repel is given. Empty plane at the edges is where the labels escape to.
  coord_fixed(xlim = sq_lims(provs$dim1, provs$dim2, pad = 0.30)$x,
              ylim = sq_lims(provs$dim1, provs$dim2, pad = 0.30)$y) +
  labs(x = ejes[1], y = ejes[2], tag = "c") +
  theme_house()

# (a) the cloud of individuals, and nothing on top of it. The lattice of
# distinct profiles carries all 10,580 suppliers: an identical profile on the
# six actives is one point, sized by how many share it. No stratum centroids
# and no ellipses: the strata are an estimation device with no standing of
# their own in the argument, and marking them on the figure that presents the
# space would give them one. Their centroids and concentration ellipses are in
# Figure S6, where the comparison that needs them is made.
nube <- read.csv(file.path(DIR_TAB, "tab_c10_nube_perfiles.csv"))

f1a <- ggplot(nube, aes(d1, d2)) +
  geom_hline(yintercept = 0, colour = "grey75", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey75", linewidth = 0.3) +
  geom_point(aes(size = n), colour = "grey65", alpha = 0.6, shape = 16) +
  scale_size_area(max_size = 2.4, guide = "none") +
  coord_fixed() +
  labs(x = ejes[1], y = ejes[2], tag = "a") +
  theme_house()

# (d) the rest of the supplementary battery: intensity and tenure, which are
# trajectories, and the founding cohort, which comes from the companies
# register and took no part in building the space. The unmatched residual of
# the cohort sits far out on the first axis and is not drawn.
sup_otras <- sup |>
  filter(variable %in% names(SUPVAR_LABELS), categoria != "sin_match") |>
  mutate(etiqueta = SUP_LABELS[categoria],
         grupo = factor(SUPVAR_LABELS[variable], levels = SUPVAR_LABELS))
stopifnot(!any(is.na(sup_otras$etiqueta)))

f1d <- ggplot(sup_otras, aes(dim1, dim2, shape = grupo)) +
  geom_hline(yintercept = 0, colour = "grey75", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey75", linewidth = 0.3) +
  geom_point(size = 1.9, colour = "#1a1a1a") +
  geom_text_repel(aes(label = etiqueta), size = 2.1, seed = 9,
                  colour = "#333333", max.overlaps = 30, force = 3,
                  min.segment.length = 0.15, segment.size = 0.2,
                  segment.colour = "grey65", box.padding = 0.4,
                  show.legend = FALSE) +
  scale_shape_manual(values = SUPVAR_SHAPES, name = NULL) +
  guides(shape = guide_legend(nrow = 2)) +
  coord_fixed(xlim = sq_lims(sup_otras$dim1, sup_otras$dim2, pad = 0.22)$x,
              ylim = sq_lims(sup_otras$dim1, sup_otras$dim2, pad = 0.22)$y) +
  labs(x = ejes[1], y = ejes[2], tag = "d") +
  theme_house()

pub((f1a | f1b) / (f1c | f1d), "1", 7.6)

# ── Fig 2: the territorial gradient, mapped ──────────────────────────────────
# Three choropleths of the 24 jurisdictions. Quintile bins per panel: the
# journal prints greyscale and asks that tints stay distinguishable, which a
# continuous ramp cannot promise. The frame is cropped to the continental
# territory; no supplier in the record is domiciled beyond it. CABA is
# invisible at national scale and is the low extreme of the gradient, so each
# panel carries it as an inset over the ocean.
conc <- read.csv(file.path(DIR_TAB, "tab_concentracion.csv"),
                 encoding = "UTF-8")
geo <- st_read(file.path(DIR_RAW, "georef_provincias.geojson"),
               quiet = TRUE) |>
  st_make_valid() |>
  mutate(provincia = dplyr::case_match(
    nombre,
    "Ciudad Autónoma de Buenos Aires" ~ "CABA",
    "Tierra del Fuego, Antártida e Islas del Atlántico Sur" ~
      "Tierra del Fuego",
    .default = nombre)) |>
  left_join(grad |> select(provincia, share_anclado, share_persona_fisica),
            by = "provincia") |>
  left_join(conc |> select(provincia, share_prov_lider), by = "provincia")
geo <- suppressWarnings(
  st_crop(geo, xmin = -74.1, ymin = -55.6, xmax = -53.3, ymax = -21.7))
# the South Atlantic islands sit inside the frame and would print with Tierra
# del Fuego's tint; no supplier in the record is domiciled on them
islas <- st_as_sfc(st_bbox(c(xmin = -62.5, ymin = -53.6,
                             xmax = -56.0, ymax = -50.5), crs = st_crs(geo)))
geo <- suppressWarnings(st_difference(geo, islas))
stopifnot(nrow(geo) == 24, !any(is.na(geo$share_anclado)),
          !any(is.na(geo$share_prov_lider)))

MAP_GREYS <- c("#f2f2f2", "#cccccc", "#a3a3a3", "#737373", "#404040")

# inset box shared by both panels, offshore of the Buenos Aires coast. Each
# panel also draws the extent it enlarges as a rectangle on the national map,
# and the leader line (no arrowhead, QGIS convention) runs from that
# rectangle's south-eastern corner — the side facing the inset — to the box
# centre; the inset's white background overdraws the interior stretch, so the
# visible line meets the frame edge wherever the grob settles. Without the
# rectangle the line appears to start from empty map: panel (b) keeps the
# capital's marks out of the national scale on purpose (they are drawn in the
# inset), so nothing marks the place the reader is being sent from. Both
# panels enlarge the same extent, so both draw the same rectangle.
INSET_BOX <- c(xmin = -57.0, xmax = -53.4, ymin = -43.8, ymax = -37.8)
INSET_CX <- mean(INSET_BOX[c("xmin", "xmax")])
INSET_CY <- mean(INSET_BOX[c("ymin", "ymax")])

# the enlarged extent, drawn on the national map, plus the leader line out of
# its south-eastern corner. A true CABA bbox is 0.2 degrees across and prints
# as a dot, so the rectangle is grown to a floor of 0.45 degrees a side.
extent_marks <- function(bb, minimo = 0.45) {
  cx <- mean(bb[c("xmin", "xmax")]); cy <- mean(bb[c("ymin", "ymax")])
  hw <- max(diff(bb[c("xmin", "xmax")]), minimo) / 2
  hh <- max(diff(bb[c("ymin", "ymax")]), minimo) / 2
  list(annotate("rect", xmin = cx - hw, xmax = cx + hw,
                ymin = cy - hh, ymax = cy + hh,
                fill = NA, colour = "grey45", linewidth = 0.3),
       annotate("segment", x = cx + hw, y = cy - hh,
                xend = INSET_CX, yend = INSET_CY,
                colour = "grey55", linewidth = 0.25))
}

map_panel <- function(metric, titulo, tag) {
  g <- geo
  v <- 100 * g[[metric]]
  qs <- unique(quantile(v, probs = seq(0, 1, length.out = 6)))
  g$bin <- cut(v, qs, include.lowest = TRUE)
  fills <- setNames(MAP_GREYS[seq_len(nlevels(g$bin))], levels(g$bin))
  # one printed number ends a bin and begins the next, so ranges stay
  # contiguous; a decimal appears only where the break is not whole
  fmt <- function(x) ifelse(abs(x - round(x)) < 0.05,
                            sprintf("%.0f", x), sprintf("%.1f", x))
  rangos <- paste0(fmt(head(qs, -1)), "–", fmt(tail(qs, -1)))
  caba <- ggplot(g[g$provincia == "CABA", ]) +
    geom_sf(aes(fill = bin), colour = "grey35", linewidth = 0.2,
            show.legend = FALSE) +
    scale_fill_manual(values = fills, drop = FALSE) +
    labs(title = "CABA") +
    theme_void(base_size = 8) +
    theme(plot.title = element_text(size = 5.5, hjust = 0.5,
                                    margin = margin(b = 1)),
          plot.background = element_rect(colour = "grey55", fill = "white",
                                         linewidth = 0.25),
          plot.margin = margin(1, 2, 1, 2))
  ggplot(g) +
    geom_sf(aes(fill = bin), colour = "white", linewidth = 0.12) +
    scale_fill_manual(values = fills, labels = rangos, drop = FALSE,
                      name = NULL) +
    extent_marks(st_bbox(g[g$provincia == "CABA", ])) +
    annotation_custom(ggplotGrob(caba),
                      xmin = INSET_BOX[["xmin"]], xmax = INSET_BOX[["xmax"]],
                      ymin = INSET_BOX[["ymin"]], ymax = INSET_BOX[["ymax"]]) +
    coord_sf(expand = FALSE, datum = NA) +
    # the panel letter is folded into the title: a plot tag prints at the top
    # left corner, exactly where these two-line titles start
    labs(title = paste0("(", tag, ") ", titulo)) +
    theme_house() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          legend.position = "inside",
          legend.position.inside = c(0.76, 0.20),
          legend.key.height = unit(7, "pt"),
          legend.key.width = unit(9, "pt"),
          legend.key.spacing.y = unit(1, "pt"),
          legend.text = element_text(size = 6, margin = margin(l = 3)))
}

f2a <- map_panel("share_anclado",
                 "Awards from anchored\norgans (%)", "a")
# ── Fig 2 (b): the purchasing units at their seats ───────────────────────────
# Each point is a locality x organ-type aggregate of purchasing units, at the
# seat of the purchasing office (NOT the delivery point). Seats from the
# hand-verified national gazetteer (33_uoc_gazetteer.py); outside the trivial
# capital administrative block, units whose seat is neither hand-verified nor
# cross-checked are not drawn, so the map is a floor, consistent with the
# paper's treatment of unmatched units.
gaz <- read.csv(file.path(DIR_RAW, "uoc_gazetteer.csv"), encoding = "UTF-8") |>
  mutate(es_caba = !is.na(localidad) & toupper(localidad) == "CABA") |>
  filter(!is.na(lat), verificado == 1 | es_caba)
TIPO4 <- c(seguridad_defensa = "Security and defence",
           infraestructura = "Infrastructure",
           salud_social = "Health and social",
           administracion = "Other", ciencia_universidad = "Other",
           cultura_educacion = "Other")
gaz$tipo4 <- factor(TIPO4[gaz$tipo_organismo],
                    levels = c("Security and defence", "Infrastructure",
                               "Health and social", "Other"))
# deterministic ring offset so several types at one seat stay separable; the
# capital's marks take a small one, since they are read inside the inset
agregar_marcas <- function(d, radio) {
  d |>
    group_by(localidad, provincia, tipo4) |>
    summarise(lat = first(lat), lon = first(lon), n_adj = sum(n_adj),
              unidades = n(), .groups = "drop") |>
    group_by(localidad, provincia) |>
    mutate(k = row_number() - 1,
           lon = lon + ifelse(n() > 1, radio * cos(2 * pi * k / n()), 0),
           lat = lat + ifelse(n() > 1, radio * sin(2 * pi * k / n()), 0)) |>
    ungroup()
}
gaz <- gaz |> filter(!is.na(tipo4))
SHAPES4 <- c("Security and defence" = 16, "Infrastructure" = 17,
             "Health and social" = 15, "Other" = 5)
# The inset holds the capital and nothing else. Its units are one locality
# carrying 89,927 awards, against a largest mark of 5,177 anywhere else, so
# they cannot share the national map's size scale without collapsing every
# provincial mark; the caption declares that the inset has its own. The
# conurbation around the capital does not need enlarging — its largest mark is
# 4,070, below the national maximum — so its garrisons (Campo de Mayo, El
# Palomar, Ezeiza) are drawn on the national map with everyone else.
pts_nac <- agregar_marcas(gaz |> filter(!es_caba), 0.22)
base_caba <- geo[geo$provincia == "CABA", ]
# every unit of the capital carries one placeholder coordinate for the whole
# administrative block, so the ring is a convention either way; centring it on
# the polygon keeps the four marks inside the outline they belong to
caba_c <- suppressWarnings(st_coordinates(st_centroid(st_geometry(base_caba))))
pts_caba <- agregar_marcas(gaz |> filter(es_caba), 0) |>
  mutate(k = row_number() - 1, m = n(),
         lon = caba_c[1, 1] + 0.05 * cos(2 * pi * k / m),
         lat = caba_c[1, 2] + 0.05 * sin(2 * pi * k / m))
caba_pts <- ggplot(base_caba) +
  geom_sf(fill = "#f2f2f2", colour = "grey55", linewidth = 0.15) +
  geom_point(data = pts_caba, aes(lon, lat, size = n_adj, shape = tipo4),
             colour = "#333333", alpha = 0.75, stroke = 0.5,
             show.legend = FALSE) +
  scale_size_area(max_size = 3.2, limits = c(1, max(pts_caba$n_adj))) +
  scale_shape_manual(values = SHAPES4, drop = FALSE) +
  coord_sf(expand = FALSE, datum = NA) +
  labs(title = "CABA") +
  theme_void(base_size = 8) +
  theme(plot.title = element_text(size = 5.5, hjust = 0.5,
                                  margin = margin(b = 1)),
        plot.background = element_rect(colour = "grey55", fill = "white",
                                       linewidth = 0.25),
        plot.margin = margin(1, 2, 1, 2))
f2b <- ggplot(geo) +
  # a flat base map needs grey borders to show a province at all; the white
  # ones of panel (a) do their work there by separating the quintile tints
  geom_sf(fill = "#f7f7f7", colour = "#cfcfcf", linewidth = 0.2) +
  geom_point(data = pts_nac, aes(lon, lat, size = n_adj, shape = tipo4),
             colour = "#333333", alpha = 0.75, stroke = 0.5) +
  scale_size_area(max_size = 4.5, name = "Awards",
                  breaks = c(100, 1000, 4000)) +
  scale_shape_manual(values = SHAPES4, name = NULL, drop = FALSE) +
  extent_marks(st_bbox(base_caba)) +
  annotation_custom(ggplotGrob(caba_pts),
                    xmin = INSET_BOX[["xmin"]], xmax = INSET_BOX[["xmax"]],
                    ymin = INSET_BOX[["ymin"]], ymax = INSET_BOX[["ymax"]]) +
  coord_sf(expand = FALSE, datum = NA) +
  labs(title = "(b) Purchasing units at their seats") +
  guides(shape = guide_legend(override.aes = list(size = 2.2)),
         size = guide_legend(override.aes = list(shape = 16))) +
  theme_house() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), axis.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.76, 0.20),
        legend.key.height = unit(7, "pt"),
        legend.key.width = unit(9, "pt"),
        legend.key.spacing.y = unit(0.5, "pt"),
        legend.text = element_text(size = 6, margin = margin(l = 3)),
        legend.title = element_text(size = 6))
pub(f2a + f2b, "2", 4.9)

# ── Fig 5: the gradient of access, and the procedure gap ─────────────────────
f3a <- ggplot(grad, aes(100 * share_anclado, 100 * share_persona_fisica,
                        colour = grupo, fill = grupo, shape = grupo)) +
  # inherit.aes = FALSE, or the region mapping silently splits this into six
  # per-region fits instead of the single line the caption describes
  geom_smooth(data = grad, inherit.aes = FALSE,
              aes(100 * share_anclado, 100 * share_persona_fisica),
              method = "lm", formula = y ~ x, se = FALSE, colour = "grey72",
              linewidth = 0.35, linetype = "dashed") +
  geom_point(aes(size = proveedores), stroke = 0.4) +
  geom_text_repel(aes(label = provincia), size = 2.1, seed = 42,
                  colour = "#333333", max.overlaps = 40,
                  min.segment.length = 0.15, segment.size = 0.2,
                  segment.colour = "grey65", box.padding = 0.24,
                  show.legend = FALSE) +
  scale_colour_manual(values = group_pal(), name = NULL) +
  scale_fill_manual(values = group_pal(), name = NULL) +
  scale_shape_manual(values = GROUP_SHAPES, name = NULL) +
  scale_size_area(max_size = 4, name = "Suppliers",
                  breaks = c(500, 2000, 3900),
                  labels = c("500", "2,000", "3,900")) +
  guides(colour = guide_legend(order = 1, nrow = 2),
         fill = guide_legend(order = 1, nrow = 2),
         shape = guide_legend(order = 1, nrow = 2),
         size = guide_legend(order = 2, nrow = 2)) +
  labs(x = "Awards from territorially anchored organs (%)",
       y = "Suppliers that are natural persons (%)", tag = "a") +
  theme_house()

# the change in the procedure mix, jurisdiction by jurisdiction. An earlier
# version plotted six regional lines across the three periods, which made the
# north-east look like the country's one exception; at the level of the unit
# that actually holds a government, the exceptions are one Patagonian and one
# north-eastern jurisdiction, and a north-eastern province moves the other way.
proc <- read.csv(file.path(DIR_TAB, "tab_directa_jurisdiccion_era.csv"),
                 encoding = "UTF-8") |>
  mutate(grupo = factor(grupo, levels = GROUP_LEVELS))
f3b <- ggplot(proc, aes(100 * delta, reorder(provincia, delta),
                        colour = grupo, fill = grupo, shape = grupo)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.35) +
  geom_segment(aes(x = 0, xend = 100 * delta, yend = provincia),
               linewidth = 0.32) +
  geom_point(size = 1.5, stroke = 0.35) +
  scale_colour_manual(values = group_pal(), guide = "none") +
  scale_fill_manual(values = group_pal(), guide = "none") +
  scale_shape_manual(values = GROUP_SHAPES, guide = "none") +
  # the unit (percentage points) is stated in the caption: with it on the
  # axis the label overran the right edge of the sheet (4 Sep 2026)
  labs(x = "Change in direct contracting, 2019 to 2024–25",
       y = NULL, tag = "b") +
  theme_house() +
  theme(panel.grid.major.y = element_blank(),
        plot.margin = margin(5.5, 10, 5.5, 5.5))

pub(f3a + f3b + plot_layout(widths = c(1.3, 1), guides = "collect") &
      theme(legend.position = "bottom"), "3", 4.45)

# ── Fig 6: the series on all buyers and on a balanced panel ──────────────────
serie <- read.csv(file.path(DIR_TAB, "tab_platform_theil_panel.csv")) |>
  filter(between(ejercicio, WIN0, WIN1)) |>
  select(ejercicio, `All buyers` = share_between_todos,
         `Balanced panel of buyers` = share_between_panel) |>
  pivot_longer(-ejercicio, names_to = "universo", values_to = "share")
boot <- read.csv(file.path(DIR_TAB, "tab_boot_theil_ci.csv"))
f4 <- ggplot(serie, aes(ejercicio, 100 * share, colour = universo,
                        shape = universo, linetype = universo)) +
  geom_ribbon(data = boot, inherit.aes = FALSE,
              aes(ejercicio, ymin = 100 * ic_lo, ymax = 100 * ic_hi),
              fill = "grey60", alpha = 0.18) +
  geom_line(linewidth = 0.5) + geom_point(size = 1.9) +
  scale_colour_manual(values = c(`All buyers` = "#1a1a1a",
                                 `Balanced panel of buyers` = "#808080"),
                      name = NULL) +
  scale_shape_manual(values = c(16, 17), name = NULL) +
  scale_linetype_manual(values = c("solid", "22"), name = NULL) +
  scale_x_continuous(breaks = WIN0:WIN1) +
  labs(x = NULL, y = "Between-province share of supplier Theil T (%)") +
  theme_house()
pub(f4, "6", 3.0)

# ── Fig S1: CA provinces x buyer types, per era ──────────────────────────────
ca <- read.csv(file.path(DIR_TAB, "tab_ca_coords_provincia_tipo.csv"),
               encoding = "UTF-8") |>
  mutate(era = factor(ERA_LABELS[era], levels = ERA_LABELS),
         nea = clase == "provincia" & punto %in% NEA,
         # the buyer-type codes are Spanish in the data; a published figure
         # never shows them
         etiqueta = ifelse(clase == "tipo_organismo", TIPO_LABELS[punto], punto))
stopifnot(!any(is.na(ca$etiqueta)))
# Ten text labels in a panel a third of the text width wide collide whatever
# the repel settings are given (tried: box padding to 0.7, force to 12, 40,000
# iterations, three seeds, and a padded plane). The buyer types therefore carry
# a shape legend instead of a label in every panel, which removes eighteen of
# the labels and leaves the four north-eastern provinces named on the plane —
# the comparison the figure exists to make (3 Sep 2026).
fs1 <- ggplot(ca, aes(dim1, dim2)) +
  geom_hline(yintercept = 0, colour = "grey75", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey75", linewidth = 0.3) +
  geom_point(data = subset(ca, clase == "provincia"),
             aes(colour = nea, size = nea, shape = nea), show.legend = FALSE) +
  geom_point(data = subset(ca, clase == "tipo_organismo"),
             aes(shape = etiqueta), size = 1.9, colour = "#666666",
             fill = "#666666") +
  geom_text_repel(data = subset(ca, nea),
                  aes(label = etiqueta), colour = "#1a1a1a",
                  size = 1.9, max.overlaps = Inf, box.padding = 0.5,
                  point.padding = 0.3, force = 6, max.iter = 20000,
                  min.segment.length = 0,
                  segment.size = 0.2, segment.colour = "grey65", seed = 3,
                  show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = "#1a1a1a", `FALSE` = "#b3b3b3"),
                      guide = "none") +
  scale_size_manual(values = c(`TRUE` = 1.9, `FALSE` = 1.1), guide = "none") +
  scale_shape_manual(values = c(TIPO_SHAPES, `TRUE` = 16, `FALSE` = 1),
                     breaks = names(TIPO_SHAPES), name = NULL) +
  guides(shape = guide_legend(nrow = 2,
                              override.aes = list(size = 1.9,
                                                  colour = "#666666"))) +
  facet_wrap(~era, nrow = 1) +
  # empty plane for the labels to escape into: under coord_fixed the panel
  # height follows the data range, so padding the range is what buys room —
  # extra device height alone becomes whitespace (3 Sep 2026, labels collided)
  coord_fixed(xlim = range(ca$dim1) + c(-0.2, 0.2),
              ylim = range(ca$dim2) + c(-0.35, 0.35)) +
  labs(x = "Dimension 1", y = "Dimension 2") +
  theme_house()
pub(fs1, "S1", 3.4)

# ── Fig S2: cohort persistence, stratified by client anchoring ───────────────
rs <- read.csv(file.path(DIR_TAB, "tab_robustez_supervivencia.csv")) |>
  pivot_longer(c(surv_fernandez, surv_milei), names_to = "hasta",
               values_to = "surv") |>
  mutate(hasta = recode(hasta,
                        surv_fernandez = "Still winning under Fernández",
                        surv_milei = "Still winning under Milei"),
         cliente = ifelse(cliente_territorial, "Territorially anchored client",
                          "Other client"),
         region = factor(REGION_LABELS[region], levels = REGION_LABELS))
fs2 <- ggplot(rs, aes(region, 100 * surv, fill = cliente)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  facet_wrap(~hasta) +
  scale_fill_manual(values = c(`Territorially anchored client` = "#404040",
                               `Other client` = "#bfbfbf"), name = NULL) +
  labs(x = NULL, y = "Macri-cohort suppliers still winning (%)") +
  theme_house()
pub(fs2, "S2", 3.2)

cat("done: Fig1, Fig2, Fig3, Fig6 (main) + FigS1-FigS2 (supplement), png+tiff+eps\n")
