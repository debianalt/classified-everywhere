# 49 — The space without the one-off sellers (round 36, 6 Sep 2026)
#
# A third of the 10,580 suppliers hold a single award in the window, so their
# profile on the six active variables is that one exchange: one client, one
# sector, one procedure, one amount. Methods declares the fact and nothing
# tests it, and it is the one exposure of the active battery that no
# robustness run covers. The charge writes itself: the personal pole of the
# second axis is where the one-off sellers sit (test value -19.6 on that axis,
# Table S10), the periphery carries more of them than the core (42.6 per cent
# of the outer stratum against 34.3 of the core), so the territorial
# displacement could be a measurement artefact of profiles drawn once.
#
# The run refits the space on suppliers with at least 2, 3 and 5 awards. Each
# fit is a specific MCA of its own population: the 5 per cent rule is applied
# to the restricted matrix, not inherited, so the number of active categories
# is reported alongside. Restricting the population changes which axis carries
# which opposition, so the axis of access is identified in each fit the way
# script 47 identifies it for the class-specific solutions: by correlating the
# individuals' restricted coordinates with their coordinates in the published
# space, over the same individuals, and taking the axis with the largest
# absolute correlation with the published second axis. The stratum
# coordinates and the ordering of the 24 jurisdictions are then read on that
# axis, sign-corrected.
#
# Emits tab_robustez_unaadj.csv (Table S10c).
#
# Run: Rscript 49_robustez_unaadj.R

source("theme_house.R", chdir = TRUE)
suppressMessages(library(GDAtools))

master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
adj    <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
m <- build_supplier_space(adj, master)
stopifnot(nrow(m) == 10580)

X <- active_matrix(m)
full <- speMCA(X, excl = excl_index(X, rare_categories(X)), ncp = 5)
mr <- modif.rate(full)$modif$mrate
# the published solution: if this ever moves, the table below is meaningless
stopifnot(abs(mr[1] - 43.6) < 0.1, abs(mr[2] - 33.0) < 0.1)
m$f1 <- full$ind$coord[, 1]
m$f2 <- full$ind$coord[, 2]

grad <- read_tab("tab_gradiente_provincias.csv")

# stratum and jurisdiction coordinates on one axis of one fit, sign-corrected
# so that the corporate pole is always positive
lectura <- function(mc, d, eje, signo) {
  s <- supvar(mc, factor(d$estrato, levels = c("core", "middle", "outer")))
  g <- setNames(s$coord[, eje] * signo, rownames(s$coord))
  sj <- supvar(mc, factor(d$provincia))
  jc <- setNames(sj$coord[, eje] * signo, rownames(sj$coord))
  k <- match(names(jc), grad$provincia)
  list(core = g["core"], middle = g["middle"], outer = g["outer"],
       gap = unname(g["core"] - g["outer"]),
       rho = cor(jc, grad$share_anclado[k], method = "spearman",
                 use = "complete.obs"),
       n_jur = sum(!is.na(k)))
}

fila <- function(minadj) {
  d <- if (minadj <= 1) m else m[m$n_adjudicaciones >= minadj, ]
  Xd <- active_matrix(d)
  exd <- rare_categories(Xd)
  mc <- speMCA(Xd, excl = excl_index(Xd, exd), ncp = 5)
  mrd <- modif.rate(mc)$modif$mrate
  co <- mc$ind$coord[, 1:5]
  r <- sapply(1:5, function(k) cor(co[, k], d$f2))
  eje <- which.max(abs(r))
  L <- lectura(mc, d, eje, sign(r[eje]))
  data.frame(
    minimo_adj = minadj,
    n = nrow(d),
    categorias_activas = nrow(mc$var$coord),
    benzecri_1 = round(mrd[1], 1),
    benzecri_2 = round(mrd[2], 1),
    eje_acceso = eje,
    r_con_eje_publicado = round(abs(r[eje]), 3),
    # how much of the published opposition the next axis still carries: where
    # this is large the access opposition is split over two restricted axes
    # and a single one under-reads the territorial ordering
    r_segunda = round(sort(abs(r), decreasing = TRUE)[2], 3),
    core = round(unname(L$core), 3),
    middle = round(unname(L$middle), 3),
    outer = round(unname(L$outer), 3),
    brecha_core_outer = round(L$gap, 3),
    rho_anclaje = round(L$rho, 3))
}

tab <- do.call(rbind, lapply(c(1, 2, 3, 5), fila))
# the published row has to reproduce the published numbers
stopifnot(tab$minimo_adj[1] == 1, tab$eje_acceso[1] == 2,
          abs(tab$brecha_core_outer[1] - 0.832) < 0.005)
write_tab(tab, "tab_robustez_unaadj.csv")
print(tab)

# the share of one-off sellers by stratum, quoted in the header and the note
cat("\nshare of single-award suppliers by stratum:\n")
print(round(prop.table(table(m$estrato, m$n_adjudicaciones == 1), 1)[, 2], 3))
