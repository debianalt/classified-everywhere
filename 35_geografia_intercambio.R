# 35 — The geography of the exchange (author's decision, 29 Aug 2026).
#
# The distance of the exchange, award by award: great-circle distance between
# the seat of the purchasing unit (gazetteer of script 33) and the supplier's
# registered locality (SIPRO via script 34). The metric realisation of the
# axis of access: the provisioning relation should be a relation of
# proximity; expertise and scale should travel. All statistics on the full
# joined record; the k >= 20 threshold applies only to which arcs Figure 4
# draws (project anonymisation rule, 27 Aug).
#
# Caveat carried in methods and caption: seat of the purchasing office to
# fiscal domicile — the administrative geometry of the relation, not freight.
#
# Run: Rscript 35_geografia_intercambio.R

source("theme_house.R", chdir = TRUE)
suppressPackageStartupMessages({library(GDAtools); library(sf)
  library(patchwork)})

fl <- read_parquet(file.path(DIR_PROC, "flujos_adjudicacion.parquet"))

# distance in km (haversine, vectorised)
hav_km <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 +
    cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 6371 * asin(pmin(1, sqrt(a)))
}
fl$km <- hav_km(fl$lat_u, fl$lon_u, fl$lat_p, fl$lon_p)

# anchoring of the buying organism
anc <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")) |>
  distinct(organismo, anclaje_territorial)
fl <- fl |> left_join(anc, by = c("saf" = "organismo"))

cat(sprintf("adjudicaciones con distancia: %s | mediana %.0f km | p90 %.0f\n",
            format(nrow(fl), big.mark = ","), median(fl$km),
            quantile(fl$km, 0.9)))

# ── supplier positions: the published space, asserted ────────────────────────
adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
m <- build_supplier_space(adj, master)
X <- active_matrix(m)
mca <- speMCA(X, excl = excl_index(X, rare_categories(X)), ncp = 2)
mr <- modif.rate(mca)$modif$mrate
stopifnot(abs(mr[1] - 43.6) < 0.15, abs(mr[2] - 33.0) < 0.15)
m$dim2 <- mca$ind$coord[, 2]
m$q2 <- cut(m$dim2, quantile(m$dim2, seq(0, 1, 0.25)),
            labels = paste0("Q", 1:4), include.lowest = TRUE)

fl <- fl |> left_join(
  m |> transmute(cuit = as.character(cuit), A_personeria, A_escala,
                 A_directa, A_registro, estrato, q2),
  by = "cuit")

med <- function(d, v) d |> filter(!is.na(.data[[v]])) |>
  group_by(categoria = .data[[v]]) |>
  summarise(n_adj = n(), km_mediana = round(median(km)),
            km_p25 = round(quantile(km, 0.25)),
            km_p75 = round(quantile(km, 0.75)), .groups = "drop") |>
  mutate(variable = v, .before = 1)

# (a) the axis of access, in kilometres
bat <- bind_rows(med(fl, "A_personeria"), med(fl, "A_escala"),
                 med(fl, "A_directa"), med(fl, "A_registro"), med(fl, "q2"))
write_tab(bat, "tab_distancia_categorias.csv")
cat("\n(a) km por categoria del eje de acceso:\n")
print(as.data.frame(bat |> select(-km_p25, -km_p75)), row.names = FALSE)

# (b) by organ type and anchoring
org <- bind_rows(
  med(fl |> mutate(anclado = ifelse(anclaje_territorial, "anclado",
                                    "no_anclado")), "anclado"),
  med(fl, "tipo_organismo"))
write_tab(org, "tab_distancia_organos.csv")
cat("\n(b) km por anclaje y tipo de organo:\n")
print(as.data.frame(org |> select(-km_p25, -km_p75)), row.names = FALSE)

# (c) by supplier stratum
est <- med(fl, "estrato")
write_tab(est, "tab_distancia_estratos.csv")
cat("\n(c) km por estrato del proveedor:\n")
print(as.data.frame(est), row.names = FALSE)

# share of the exchange that is same-locality, by anchoring — the local door
puerta <- fl |> filter(!is.na(anclaje_territorial)) |>
  group_by(anclado = ifelse(anclaje_territorial, "anclado", "no_anclado")) |>
  summarise(n_adj = n(), share_local_25km = round(mean(km <= 25), 3),
            share_100km = round(mean(km <= 100), 3),
            km_mediana = round(median(km)), .groups = "drop")
write_tab(puerta, "tab_distancia_puerta_local.csv")
cat("\npuerta local (<=25 km) por anclaje:\n")
print(as.data.frame(puerta), row.names = FALSE)

# ── Figure 4: the flows, k >= 20 ─────────────────────────────────────────────
flow <- fl |>
  group_by(loc_uoc, lat_u, lon_u, loc_prov, lat_p, lon_p,
           anclado = ifelse(anclaje_territorial, "Anchored organ",
                            "Programmatic organ")) |>
  summarise(n_adj = n(), km = median(km), .groups = "drop") |>
  filter(n_adj >= 20, !is.na(anclado))
write_tab(flow |> mutate(km = round(km)) |> arrange(desc(n_adj)),
          "tab_flujos_k20.csv")
# display threshold above the anonymity floor: 643 arcs at >=20 wash the map;
# the caption states both numbers. Thick curves first and thin ones on top
# (3 Sep 2026): a 70-award curve drawn under a 1,300-award bundle vanished.
arcos <- flow |> filter(km > 5, n_adj >= 50) |> arrange(desc(n_adj)) |>
  mutate(id = row_number())
locales <- flow |> filter(km <= 5) |>
  group_by(lat_u, lon_u, anclado) |>
  summarise(n_adj = sum(n_adj), .groups = "drop") |>
  arrange(desc(n_adj))
cat(sprintf("
figura: %d curvas (>5 km, n>=50), %d discos locales (n>=20)
",
            nrow(arcos), nrow(locales)))

# One panel per kind of organ (author's decision, 3 Sep 2026). The argument
# cuts the state in two from section 2, Table 2 orders the jurisdictions by
# the anchored share of demand and Figure 2a maps that share, so the map of
# the exchange takes the same cut. Superimposed, the two series hid each
# other and colour could not separate them; side by side each panel carries
# one story, and the contrast between them is the finding of section 6: the
# anchored state buys at the garrison door and from the capital, the
# programmatic state buys from the capital, whether next door or far away.
#
# Curves are computed here as quadratic Béziers instead of geom_curve, so
# that each can carry an arrowhead at mid-path as well as at its end (author's
# request): where many curves converge the end heads pile up, and the
# mid-path head is the one that still says where the curve goes. Right-hand
# bow, as geom_curve's positive curvature draws; the two halves share the
# midpoint so the path is continuous.
bezier <- function(d, bow = 0.24) {
  purrr::pmap_dfr(d |> select(id, lon_u, lat_u, lon_p, lat_p, n_adj),
                  function(id, lon_u, lat_u, lon_p, lat_p, n_adj) {
    dx <- lon_p - lon_u; dy <- lat_p - lat_u
    cx <- (lon_u + lon_p) / 2 + bow * dy
    cy <- (lat_u + lat_p) / 2 - bow * dx
    bind_rows(tibble(t = seq(0, 0.5, length.out = 21), half = 1L),
              tibble(t = seq(0.5, 1, length.out = 21), half = 2L)) |>
      mutate(id = id, n_adj = n_adj,
             x = (1 - t)^2 * lon_u + 2 * (1 - t) * t * cx + t^2 * lon_p,
             y = (1 - t)^2 * lat_u + 2 * (1 - t) * t * cy + t^2 * lat_p,
             grp = id * 2L + half - 2L)
  })
}

geo <- st_read(file.path(DIR_RAW, "georef_provincias.geojson"),
               quiet = TRUE) |> st_make_valid()
geo <- suppressWarnings(
  st_crop(geo, xmin = -74.1, ymin = -55.6, xmax = -53.3, ymax = -21.7))
islas <- st_as_sfc(st_bbox(c(xmin = -62.5, ymin = -53.6,
                             xmax = -56.0, ymax = -50.5), crs = st_crs(geo)))
geo <- suppressWarnings(st_difference(geo, islas))

# one ink for both panels: the panel title carries the series, so colour no
# longer encodes anything. The tonal hierarchy of 30 Aug still holds: the
# faintest curve (#1a1a1a at alpha 0.5 over #f7f7f7, about #888888) is darker
# than every border, so the base map never competes with the flows.
INK <- "#1a1a1a"
# shared limits, so the two panels and the one collected legend read on the
# same scale
LW_LIM <- c(50, max(arcos$n_adj))
SZ_LIM <- c(0, max(locales$n_adj))

panel_flujos <- function(serie, titulo) {
  a <- arcos |> filter(anclado == serie)
  cur <- bezier(a)
  l <- locales |> filter(anclado == serie)
  # every drawn seat carries a ring of constant size; the disc for local
  # purchases sits inside it. 24 of the 40 local discs are under 100 awards
  # and print invisible at an area scale that runs to 30,000: the ring says
  # "seat", the disc says "how much it buys at home"
  sedes <- bind_rows(a |> distinct(lat_u, lon_u),
                     l |> distinct(lat_u, lon_u)) |>
    distinct() |> mutate(clave = "Seat of a purchasing unit")
  # the suppliers' side gets its own mark: a small square with a white edge at
  # every locality where a curve's suppliers are registered
  prov <- a |> distinct(lat_p, lon_p) |>
    mutate(clave = "Locality of suppliers")
  ggplot(geo) +
    geom_sf(fill = "#f7f7f7", colour = "#e0e0e0", linewidth = 0.18) +
    geom_sf(data = st_union(geo), fill = NA, colour = "#b8b8b8",
            linewidth = 0.3) +
    geom_path(data = cur,
              aes(x, y, group = grp, linewidth = n_adj, alpha = n_adj),
              colour = INK, lineend = "round",
              arrow = arrow(length = unit(1.2, "mm"), type = "closed",
                            angle = 22)) +
    geom_point(data = prov, aes(lon_p, lat_p, shape = clave),
               size = 1.4, stroke = 0.4, colour = "white", fill = "#8c8c8c") +
    geom_point(data = sedes, aes(lon_u, lat_u, shape = clave),
               size = 1.9, stroke = 0.5, colour = "#4d4d4d", fill = "white") +
    geom_point(data = l, aes(lon_u, lat_u, size = n_adj), colour = INK) +
    # floors of width and alpha high enough that a 50-award curve prints as a
    # line; breaks inside the data (curves 50 to 1,571, discs 20 to 30,247)
    scale_linewidth_continuous(range = c(0.3, 1.3), limits = LW_LIM,
                               name = "Awards along the curve",
                               breaks = c(100, 500, 1500)) +
    scale_alpha_continuous(range = c(0.5, 0.85), limits = LW_LIM,
                           guide = "none") +
    scale_size_area(max_size = 4.2, limits = SZ_LIM, name = "Local purchases",
                    breaks = c(100, 1000, 10000, 30000)) +
    scale_shape_manual(values = c("Seat of a purchasing unit" = 21,
                                  "Locality of suppliers" = 22),
                       breaks = c("Seat of a purchasing unit",
                                  "Locality of suppliers"),
                       name = NULL) +
    coord_sf(expand = FALSE, datum = NA) +
    labs(title = titulo) +
    # the magnitude keys take a neutral grey: left to the geom default they
    # print in the ink of the data
    guides(shape = guide_legend(order = 1, nrow = 2,
                                override.aes = list(colour = c("#4d4d4d", "white"),
                                                    fill = c("white", "#8c8c8c"),
                                                    size = c(1.9, 1.4),
                                                    stroke = c(0.5, 0.4))),
           linewidth = guide_legend(order = 2, nrow = 3,
                                    override.aes = list(colour = "#4d4d4d",
                                                        alpha = 1)),
           size = guide_legend(order = 3, nrow = 4,
                               override.aes = list(colour = "#4d4d4d",
                                                   alpha = 1))) +
    theme_house() +
    theme(panel.grid = element_blank(), axis.text = element_blank(),
          axis.title = element_blank(), plot.title.position = "plot",
          legend.position = "bottom", legend.box = "horizontal",
          legend.key.height = unit(9, "pt"), legend.key.width = unit(13, "pt"),
          legend.text = element_text(size = 6.5, margin = margin(l = 3)),
          legend.title = element_text(size = 6.5),
          legend.box.spacing = unit(2, "pt"))
}

# short titles anchored to the plot edge: at panel width the long ones met in
# the middle of the sheet
f4 <- panel_flujos("Anchored organ", "(a) Anchored organs") +
  panel_flujos("Programmatic organ", "(b) Programmatic organs") +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
pub(f4, "4", 5.0)
