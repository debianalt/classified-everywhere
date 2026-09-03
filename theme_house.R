# theme_house.R — shared plotting theme and constants (house figure rules:
# identical fonts, category names and legend order across all figures).
# Sourced by every analysis script. Project: Who Sells to the State (2026_2).

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

PROJ <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), "."),
                      mustWork = FALSE)
`%||%` <- function(a, b) if (is.null(a)) b else a

# resolved relative to this file's location when sourced with chdir=TRUE
DIR_PROC <- "data/processed"
DIR_RAW  <- "data/raw"
DIR_TAB  <- "tables"
DIR_FIG  <- "figures"

NEA   <- c("Chaco", "Corrientes", "Formosa", "Misiones")
METRO <- c("CABA", "Buenos Aires")
ERAS  <- c("macri", "fernandez", "milei")

# The six standard regions of Argentina, with the two metropolitan
# jurisdictions held apart from the rest of the pampean group because they are
# the reference pole of the comparison. The six partition the 24 jurisdictions
# exactly: no residual group. An earlier version lumped thirteen provinces into
# a "rest of country" that averaged two very different things — the north-west
# and the pampean provinces carry almost identical anchored demand (0.806 and
# 0.808) and differ by seventeen points in the share of suppliers that are
# natural persons.
NOA       <- c("Catamarca", "Jujuy", "La Rioja", "Salta",
               "Santiago del Estero", "Tucumán")
CUYO      <- c("Mendoza", "San Juan", "San Luis")
PAMPEANA  <- c("Córdoba", "Entre Ríos", "La Pampa", "Santa Fe")
PATAGONIA <- c("Chubut", "Neuquén", "Río Negro", "Santa Cruz",
               "Tierra del Fuego")

# Analysis window. COMPR.AR was populated in stages: the army, the largest
# buyer in the record, has no awards before 2019, and the navy, the air force
# and the border force enter in 2018. Before 2019 the platform is a different
# and much smaller state. Every published figure therefore runs on 2019-2025;
# 2026 is a partial year. See 20_platform_stability.R.
WIN0 <- 2019
WIN1 <- 2025
ERA_LABELS <- c(macri = "Macri", fernandez = "Fernández", milei = "Milei")

region_of <- function(provincia) {
  ifelse(provincia %in% NEA, "NEA",
         ifelse(provincia %in% METRO, "Metro", "Resto"))
}

# fixed palette and legend order — identical across every figure
REGION_LEVELS <- c("NEA", "Metro", "Resto")
# same validated hues as STRATUM_COLS, so no figure in the project carries a
# palette that has not been checked
REGION_COLS <- c(NEA = "#D55E00", Metro = "#3D405B", Resto = "#0072B2")

# display names for published figures: the internal codes stay Spanish because
# they are data values joined across scripts; every figure that leaves the
# project shows the English labels and the same legend order
# "North-east", not the acronym: the same category is named "North-east" in
# Figure 1c, in Table 2 and in every supplementary table, and a category may
# not carry two names across the figures of one article (3 Sep 2026)
REGION_LABELS <- c(NEA = "North-east", Metro = "Metropolitan",
                   Resto = "Rest of country")
region_pal_pub <- function() {
  v <- if (exists("GREY") && GREY) REGION_GREYS else REGION_COLS
  setNames(unname(v), REGION_LABELS[names(v)])
}
# Strata of the supplier space (build_supplier_space). Named core / middle /
# outer rather than metropolitan / intermediate / peripheral so that they never
# collide with the six regional groups below: the "metropolitan" stratum holds
# five jurisdictions and the "Metropolitan" region holds two. The three strata
# are a coarser collapse of the six regions, and the mapping is stated once in
# the methods section.
STRATUM_LABELS <- c(core = "Core", middle = "Middle", outer = "Outer")
# drawn from the same validated set as GROUP_COLS. Checked all pairs, light
# surface: CVD separation PASS (worst 16.4), normal-vision floor PASS (18.0),
# contrast PASS; the lightness and chroma FAILs are the dark anchor, as above.
STRATUM_COLS <- c(Core = "#3D405B", Middle = "#0072B2", Outer = "#D55E00")

# ── Figure sizing: author at final print width ───────────────────────────────
# Wiley sets two column widths. A figure drawn 11 in wide and printed at 6.53 in
# is reduced to 59%, so a 10 pt label reaches the page at 5.9 pt. Every figure
# in this project is therefore authored at one of these two widths with fonts
# specified at their final size, and nothing is rescaled afterwards.
W_DOUBLE <- 6.53   # 166 mm, full text width
W_SINGLE <- 3.15   #  80 mm, one column

# province groups of the access gradient (script 21), one palette everywhere.
# Order runs by decreasing anchored demand, which is the order of the argument.
GROUP_LEVELS <- c("North-east", "Patagonia", "Pampean", "North-west", "Cuyo",
                  "Metropolitan")

# Okabe-Ito, with a dark slate anchor for the metropolitan pair. Checked with
# the palette validator on 28 Aug 2026, all pairs, light surface:
#   [PASS] normal-vision floor   worst pair #E69F00/#D55E00  dE 15.6
#   [WARN] CVD separation        worst pair #009E73/#CC79A7  dE 7.6 (deutan)
#   [FAIL] lightness / chroma    on #3D405B only
# The WARN is admissible because region is also encoded by shape (GROUP_SHAPES)
# and every point in Fig 1b and Fig 2 carries its own label, so identity never
# rests on hue alone. The two FAILs are the dark anchor: a deliberate neutral
# reference pole for the metropolitan pair, not an accident. The previous
# palette failed the normal-vision floor outright (Cuyo #17becf against pampean
# #9e9e9e, dE 12.7 — below the floor of 15).
GROUP_COLS <- c(`North-east` = "#D55E00", Patagonia = "#0072B2",
                Pampean = "#CC79A7", `North-west` = "#E69F00",
                Cuyo = "#009E73", Metropolitan = "#3D405B")
# filled shapes, so the six survive greyscale printing
GROUP_SHAPES <- c(`North-east` = 21, Patagonia = 22, Pampean = 23,
                  `North-west` = 24, Cuyo = 25, Metropolitan = 16)

# The journal prints in greyscale: "Colour must be converted to greyscale,
# ensuring that any resulting tints of black are distinguishable from each
# other where this is important to the diagram" (BJS author guidelines, 15).
# Region is therefore carried by SHAPE, which is unambiguous in six values, and
# grey is used at three well-separated levels only — six greys would not be
# distinguishable on paper. Set GREY = FALSE to inspect the colour versions.
GREY <- TRUE
GROUP_GREYS <- c(`North-east` = "#1a1a1a", Patagonia = "#4d4d4d",
                 Pampean = "#808080", `North-west` = "#1a1a1a",
                 Cuyo = "#808080", Metropolitan = "#4d4d4d")
STRATUM_GREYS <- c(Core = "#1a1a1a", Middle = "#666666", Outer = "#a6a6a6")
REGION_GREYS  <- c(NEA = "#1a1a1a", Metro = "#808080", Resto = "#4d4d4d")
group_pal   <- function() if (GREY) GROUP_GREYS   else GROUP_COLS
stratum_pal <- function() if (GREY) STRATUM_GREYS else STRATUM_COLS
region_pal  <- function() if (GREY) REGION_GREYS  else REGION_COLS

# the regional group of a jurisdiction: the single descriptive scheme of the
# paper, used by script 21 and by every published table and figure that groups
# provinces
grupo6_of <- function(provincia) {
  dplyr::case_when(
    provincia %in% METRO     ~ "Metropolitan",
    provincia %in% NEA       ~ "North-east",
    provincia %in% NOA       ~ "North-west",
    provincia %in% CUYO      ~ "Cuyo",
    provincia %in% PAMPEANA  ~ "Pampean",
    provincia %in% PATAGONIA ~ "Patagonia",
    .default = NA_character_)
}

# English names of the active categories of the supplier space. Fixed here so
# that figures and text use one name per category and never rotate synonyms.
CAT_LABELS <- c(
  "A_personeria.SA" = "SA", "A_personeria.SRL" = "SRL",
  "A_personeria.PersFisica" = "Sole proprietor",
  "A_personeria.OtraForma" = "Other legal form",
  "A_rubro.PROD. MEDICO/FARMA" = "Medical and pharmaceutical",
  "A_rubro.ALQUILER" = "Rental",
  "A_rubro.ALIMENTOS" = "Food",
  "A_rubro.EQUIPOS" = "Equipment",
  "A_rubro.SERV. PROFESIONAL " = "Professional services",
  "A_rubro.MANT. REPARACION Y" = "Maintenance and repair",
  "A_rubro.ELECTRICIDAD Y TEL" = "Electrical and telecoms",
  "A_rubro.REPUESTOS" = "Spare parts",
  "A_rubro.OTROS" = "Other sectors",
  "A_cliente.seguridad_defensa" = "Security and defence",
  "A_cliente.infraestructura" = "Infrastructure",
  "A_cliente.salud_social" = "Health and social",
  "A_cliente.ciencia_universidad" = "Science and universities",
  "A_cliente.cultura_educacion" = "Culture and education",
  "A_cliente.administracion" = "General administration",
  "A_escala.chica" = "Small median award",
  "A_escala.media" = "Mid median award",
  "A_escala.grande" = "Large median award",
  "A_escala.sin_ars" = "No peso awards",
  "A_directa.sin_directa" = "No direct contracting",
  "A_directa.mixto" = "Mixed procedure",
  "A_directa.mayoria_directa" = "Mostly direct",
  "A_registro.reg16_18" = "Registered 2016-18",
  "A_registro.reg19_21" = "Registered 2019-21",
  "A_registro.reg22plus" = "Registered 2022+")
# the supplementary battery reaching a figure, in English and in the order the
# categories are meant to be read
SUP_LABELS <- c(
  `1adj` = "1 award", `2-5adj` = "2-5 awards", `6-20adj` = "6-20 awards",
  `20+adj` = "Over 20 awards",
  `1anio` = "1 year", `2-4anios` = "2-4 years", `5+anios` = "5+ years",
  fund_pre2000 = "Founded pre-2000", fund_2001_2010 = "Founded 2001-10",
  fund_2011_2016 = "Founded 2011-16", fund_2017plus = "Founded post-2016",
  pers_fisica = "Natural person", sin_match = "Unmatched")
SUPVAR_LABELS <- c(S_intensidad = "Award intensity", S_tenure = "Tenure",
                   S_cohorte = "Founding cohort")
SUPVAR_SHAPES <- c(`Award intensity` = 16, Tenure = 17,
                   `Founding cohort` = 15)

VAR_LABELS <- c(A_personeria = "Legal form", A_rubro = "Sector",
                A_cliente = "Modal client", A_escala = "Scale",
                A_directa = "Procedure", A_registro = "Registration")
# the buyer-type codes are Spanish because they are data values joined across
# scripts; anything that reaches a published figure is relabelled here
TIPO_LABELS <- c(seguridad_defensa = "Security and defence",
                 infraestructura = "Infrastructure",
                 salud_social = "Health and social",
                 ciencia_universidad = "Science and universities",
                 cultura_educacion = "Culture and education",
                 administracion = "General administration")
# Six non-circular shapes, so the buyer types can carry a legend instead of a
# text label in every panel (Figure S1, 3 Sep 2026); circles stay reserved for
# the jurisdictions in that figure. One grey and six shapes, because six greys
# are not distinguishable in print.
TIPO_SHAPES <- c(`Security and defence` = 17, Infrastructure = 25,
                 `Health and social` = 15, `Science and universities` = 18,
                 `Culture and education` = 2, `General administration` = 0)
VAR_COLS <- c(`Legal form` = "#111111", Sector = "#1f4e79",
              `Modal client` = "#b8860b", Scale = "#0072B2",
              Procedure = "#D55E00", Registration = "#009E73")
# the greyscale counterpart: six families on three greys, with filled vs
# hollow shapes as the redundant channel so all six survive a printed page
VAR_GREYS  <- c(`Legal form` = "#1a1a1a", Sector = "#666666",
                `Modal client` = "#a6a6a6", Scale = "#1a1a1a",
                Procedure = "#666666", Registration = "#a6a6a6")
VAR_SHAPES <- c(`Legal form` = 16, Sector = 15, `Modal client` = 17,
                Scale = 1, Procedure = 0, Registration = 2)
var_pal <- function() if (GREY) VAR_GREYS else VAR_COLS

# base_size is the size the type reaches the printed page at, because figures
# are authored at final width. 8 pt body, 7 pt ticks.
theme_house <- function(base_size = 8) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25, colour = "grey92"),
      plot.title = element_text(size = base_size, face = "plain",
                                hjust = 0, margin = margin(b = 5)),
      plot.tag = element_text(size = base_size + 1, face = "bold"),
      plot.tag.position = c(0.01, 0.99),
      axis.title = element_text(size = base_size, colour = "grey20"),
      axis.text = element_text(size = base_size - 1, colour = "grey35"),
      # the key is wide enough that a mark never sits on its own label.
      # Legend text takes the tick grey (3 Sep 2026): in the default black it
      # was the darkest type on the sheet and competed with the point labels
      # (grey20); one step lighter it reads as apparatus, not as data.
      legend.text = element_text(size = base_size - 1, colour = "grey35",
                                 margin = margin(l = 2, r = 7)),
      legend.title = element_text(size = base_size - 1, colour = "grey20"),
      legend.key.height = unit(9, "pt"),
      legend.key.width = unit(13, "pt"),
      legend.key.spacing.x = unit(3, "pt"),
      legend.margin = margin(t = 0),
      # 8 pt detaches the legend from the axis title without a rule or a box
      legend.box.spacing = unit(8, "pt"),
      legend.position = "bottom",
      plot.margin = margin(4, 6, 4, 4)
    )
}

# Active space of the supplier cloud, built once and shared by every geometric
# script (11, 16, 18) so that the three cannot drift apart. Everything is
# computed inside the analysis window: a supplier active only before 2019 is
# not in the space, and the modal client of one that is comes from the window
# alone.
build_supplier_space <- function(adj, master, n_top_rubros = 8) {
  ipc_path <- file.path(DIR_PROC, "ipc_anual.csv")
  if (!file.exists(ipc_path)) stop("falta ipc_anual.csv: correr 02 primero")
  ipc <- read.csv(ipc_path)
  # The legal ground the buying organ invoked when it departed from open
  # competition (script 43). The award-level table carries no window of its
  # own: the window is applied here and nowhere else, so the two layers cannot
  # drift. An award with no ground recorded went through open competition.
  ap_path <- file.path(DIR_PROC, "adjudicaciones_apartado.parquet")
  if (!file.exists(ap_path))
    stop("falta adjudicaciones_apartado.parquet: correr 43 primero")
  apart <- read_parquet(ap_path)
  w <- adj |> dplyr::filter(es_nuevo, !is.na(provincia),
                            dplyr::between(ejercicio, WIN0, WIN1)) |>
    left_join(ipc |> select(ejercicio = anio, deflactor_2024),
              by = "ejercicio") |>
    left_join(apart |> select(doc_contractual, familia),
              by = "doc_contractual") |>
    mutate(monto_real = ifelse(moneda == "ARS", monto * deflactor_2024,
                               NA_real_),
           familia = ifelse(is.na(familia), "competitivo", familia))
  perfil <- w |> group_by(cuit) |>
    summarise(
      cliente_modal = names(sort(table(tipo_organismo), decreasing = TRUE))[1],
      rubro_modal = names(sort(table(rubro_principal), decreasing = TRUE))[1],
      n_adjudicaciones = n(),
      primer_ejercicio = min(ejercicio), ultimo_ejercicio = max(ejercicio),
      share_directa = mean(grepl("Directa", procedimiento, ignore.case = TRUE)),
      med_real = median(monto_real, na.rm = TRUE),
      n_cualitativo = sum(familia == "cualitativo"),
      n_fundamento = sum(familia != "competitivo"),
      .groups = "drop")
  m <- perfil |>
    inner_join(master |> select(cuit, personeria, provincia,
                                anio_contrato_social, fecha_preinscripcion),
               by = "cuit") |>
    filter(!is.na(provincia), !is.na(personeria))

  # the five jurisdictions that hold most of the supplier population, the ten
  # northern provinces, and the nine that are neither
  CORE  <- c(METRO, "Córdoba", "Santa Fe", "Mendoza")
  OUTER <- c(NEA, NOA)
  top_rubros <- m |> count(rubro_modal, sort = TRUE) |>
    slice_head(n = n_top_rubros) |> pull(rubro_modal)

  cortes_escala <- quantile(m$med_real, c(1/3, 2/3), na.rm = TRUE)
  m |> mutate(
    estrato = ifelse(provincia %in% CORE, "core",
                     ifelse(provincia %in% OUTER, "outer", "middle")),
    A_personeria = dplyr::case_match(
      personeria, "Persona Fisica" ~ "PersFisica", "Sociedad Anonima" ~ "SA",
      "Sociedad Responsabilidad Limitada" ~ "SRL", .default = "OtraForma"),
    A_rubro = ifelse(rubro_modal %in% top_rubros, substr(rubro_modal, 1, 18),
                     "OTROS"),
    A_cliente = cliente_modal,
    # scale of the exchange: terciles of the supplier's median deflated peso
    # award. Suppliers whose awards are all in foreign currency keep their
    # place in the cloud through a "sin_ars" category that the 5% rule
    # passivates, so N does not change with the enlarged battery.
    A_escala = ifelse(is.na(med_real), "sin_ars",
                      ifelse(med_real <= cortes_escala[1], "chica",
                             ifelse(med_real <= cortes_escala[2], "media",
                                    "grande"))),
    # mode of access, active in the six-variable battery (C10, 29 Aug 2026)
    A_directa = as.character(cut(share_directa, c(-0.01, 0.001, 0.5, 1.0),
                                 labels = c("sin_directa", "mixto",
                                            "mayoria_directa"))),
    # seniority in the register (SIPRO pre-inscription, full coverage)
    A_registro = ifelse(
      as.integer(format(as.Date(fecha_preinscripcion), "%Y")) <= 2018,
      "reg16_18",
      ifelse(as.integer(format(as.Date(fecha_preinscripcion), "%Y")) <= 2021,
             "reg19_21", "reg22plus")),
    S_intensidad = cut(n_adjudicaciones, c(0, 1, 5, 20, Inf),
                       labels = c("1adj", "2-5adj", "6-20adj", "20+adj")),
    S_tenure = cut(ultimo_ejercicio - primer_ejercicio, c(-1, 0, 3, Inf),
                   labels = c("1anio", "2-4anios", "5+anios")),
    S_directa = cut(share_directa, c(-0.01, 0.001, 0.5, 1.0),
                    labels = c("sin_directa", "mixto", "mayoria_directa")),
    # The register of justification a supplier's awards were granted under: it
    # never departed from open competition; it did so on the arithmetic ground
    # of the amount; or the state named it, on exclusivity or specialty. An act
    # of the state on the position rather than a coordinate of it, so
    # supplementary by construction — as an active it would rebuild the space.
    S_fundamento = dplyr::case_when(
      n_cualitativo > 0 ~ "consagrado",
      n_fundamento > 0 ~ "solo_aritmetico",
      .default = "sin_fundamento"),
    # founding cohort from the ARCA companies register; the register holds
    # companies only, so natural persons and unmatched companies are their own
    # categories and the variable can only ever be supplementary
    S_cohorte = dplyr::case_when(
      personeria == "Persona Fisica" ~ "pers_fisica",
      is.na(anio_contrato_social) ~ "sin_match",
      anio_contrato_social <= 2000 ~ "fund_pre2000",
      anio_contrato_social <= 2010 ~ "fund_2001_2010",
      anio_contrato_social <= 2016 ~ "fund_2011_2016",
      .default = "fund_2017plus"))
}

# The canonical active battery (C10, adopted 29 Aug 2026 after an iterative
# specification search): the juridical, sectoral, relational, economic,
# procedural and temporal coordinates of a supplier's position. Retention
# criterion: outer-core stratum gap of at least 0.5 SD of an axis; C10 gives
# 0.8 against 0.7 for the previous three-variable battery.
active_matrix <- function(m) {
  m |> select(A_personeria, A_rubro, A_cliente, A_escala, A_directa,
              A_registro) |>
    mutate(across(everything(), as.factor)) |> as.data.frame()
}

# the pre-29-Aug three-variable battery, kept for the specification checks
active_matrix_3 <- function(m) {
  m |> select(A_personeria, A_rubro, A_cliente) |>
    mutate(across(everything(), as.factor)) |> as.data.frame()
}

# Category names in the order speMCA lays out the indicator matrix.
category_names <- function(X) {
  unlist(lapply(names(X), function(v) paste0(v, ".", levels(X[[v]]))),
         use.names = FALSE)
}

# Le Roux and Rouanet's rule: a category carried by fewer than 5% of the
# individuals pulls the axes towards itself and is passivated in a specific
# MCA. The rare set is fixed once on the whole cloud (RARE_THRESH) and the
# same categories are passivated in every subcloud, so that solutions fitted
# on different strata remain comparable.
RARE_THRESH <- 0.05

rare_categories <- function(X, thresh = RARE_THRESH) {
  n <- nrow(X)
  fr <- unlist(lapply(names(X), function(v) {
    setNames(as.numeric(table(X[[v]])) / n, paste0(v, ".", levels(X[[v]])))
  }))
  names(fr)[fr < thresh]
}

excl_index <- function(X, labels) {
  which(category_names(X) %in% labels)
}

save_fig <- function(plot, name, width = W_DOUBLE, height = 4.0) {
  ggsave(file.path(DIR_FIG, name), plot, width = width, height = height,
         dpi = 200, bg = "white")
  cat("fig:", name, "\n")
}

# The one writer of submission figures. It lived as four near-identical copies
# in scripts 13, 28, 35 and 36, and only script 13's carried the width guard;
# that is how FigS6 drifted away from the house axis labels. Every submission
# figure goes through this function and no other.
pub <- function(plot, tag, height, width = W_DOUBLE) {
  stopifnot(isTRUE(all.equal(width, W_DOUBLE)) ||
              isTRUE(all.equal(width, W_SINGLE)))
  # a threshold replication must not overwrite a submission figure, and
  # figures/ holds the twelve of the manuscript and nothing else
  if (nzchar(getOption("tab_suffix", ""))) {
    cat(sprintf("Fig%s omitida (corrida de réplica)\n", tag))
    return(invisible(NULL))
  }
  ggsave(file.path(DIR_FIG, sprintf("Fig%s.png", tag)), plot,
         width = width, height = height, dpi = 300, bg = "white")
  ggsave(file.path(DIR_FIG, sprintf("Fig%s.tiff", tag)), plot,
         width = width, height = height, dpi = 300, bg = "white",
         compression = "lzw")
  # the journal asks for line artwork as EPS. A vector file carries no raster
  # resolution to lose, so the 800 dpi requirement is met by construction; the
  # fallback only applies to any element that has to be rasterised.
  ggsave(file.path(DIR_FIG, sprintf("Fig%s.eps", tag)), plot,
         width = width, height = height, device = cairo_ps, bg = "white",
         fallback_resolution = 800)
  cat(sprintf("Fig%s ok\n", tag))
}

# Square limits for an isometric cloud. A plane whose points spread much
# further on one axis than the other prints as a thin strip once the aspect is
# fixed, and the labels have nowhere to go. Padding the short axis out to the
# long one adds empty plane and changes no distance.
sq_lims <- function(x, y, pad = 0.10) {
  rx <- range(x, na.rm = TRUE); ry <- range(y, na.rm = TRUE)
  side <- max(diff(rx), diff(ry)) * (1 + pad)
  cx <- mean(rx); cy <- mean(ry)
  list(x = c(cx - side / 2, cx + side / 2),
       y = c(cy - side / 2, cy + side / 2))
}

# Axis labels for any plot drawn in principal coordinates, so that a cloud
# never reaches the page with a bare "Dimension 1".
gda_axis_labs <- function(pct1, pct2, d1 = 1, d2 = 2) {
  c(sprintf("Dimension %d (%.1f%%, Benzécri)", d1, pct1),
    sprintf("Dimension %d (%.1f%%, Benzécri)", d2, pct2))
}

write_tab <- function(df, name) {
  write.csv(df, file.path(DIR_TAB, tab_path(name)), row.names = FALSE)
  cat("tab:", tab_path(name), "\n")
}

# A replication run writes and reads its own copies. Any script that consumes
# a table another script wrote must go through this, or it will silently read
# the canonical run's output while writing replication output.
tab_path <- function(name) {
  sfx <- getOption("tab_suffix", "")
  if (nzchar(sfx)) sub("\\.csv$", paste0(sfx, ".csv"), name) else name
}

read_tab <- function(name) read.csv(file.path(DIR_TAB, tab_path(name)))

# Inclusion and equalising threshold for the per-province comparison (scripts
# 18, 22, 40, 41). The canonical run is 150 suppliers, which admits twelve
# provinces, three of them peripheral. A replication at 100 admits eighteen,
# six of them peripheral, and answers the objection that the null rests on a
# comparison thinned exactly where it matters. It does not replace the
# canonical run: drawing every province down to 100 weakens the estimate for
# all of them, and the null band of script 40 is calibrated at the threshold
# it is run with. Set RV_N_MIN in the environment to replicate; the tables
# then carry the threshold in their name and no figure is written.
rv_n_min <- function(default = 150) {
  v <- suppressWarnings(as.integer(Sys.getenv("RV_N_MIN", unset = NA)))
  if (is.na(v)) default else v
}
rv_suffix <- function(n) if (identical(as.integer(n), 150L)) "" else paste0("_n", n)
