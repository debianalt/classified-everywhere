# 30 — The six-variable space (C10): canonical solution and diagnostics.
#
# Adopted 29 Aug 2026 after an iterative specification search (author's
# criterion: more than three actives, chosen as the backbone coordinates of a
# contractor organisation's position, retaining an outer-core stratum gap of
# at least 0.5 SD of an axis). C10 = legal form, modal sector, modal client,
# scale of the exchange, procedure profile, registration cohort. The search
# and the discarded configurations are documented in the project log; the
# pre-29-Aug three-variable solution remains available via active_matrix_3()
# for the specification checks.
#
# Decision rule coded here: if the registration cohort carries more than 25%
# of the territorial axis, fall back to C3 (the same battery without it).
#
# Run: Rscript 30_espacio_c10.R

source("theme_house.R", chdir = TRUE)
suppressMessages(library(GDAtools))

master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
m <- build_supplier_space(adj, master)
stopifnot(nrow(m) == 10580)

X <- active_matrix(m)
excl <- rare_categories(X)
mca <- speMCA(X, excl = excl_index(X, excl), ncp = 3)
mr <- modif.rate(mca)$modif$mrate

benz <- data.frame(dim = 1:3,
                   eigenvalue = round(mca$eig$eigen[1:3], 4),
                   pct_benzecri = round(mr[1:3], 1))
write_tab(benz, "tab_c10_benzecri.csv")

n_tot <- nrow(X)
frec <- unlist(lapply(names(X), function(v)
  setNames(as.numeric(table(X[[v]])), paste0(v, ".", levels(X[[v]])))))
cats <- data.frame(categoria = rownames(mca$var$coord),
                   n = frec[rownames(mca$var$coord)],
                   round(mca$var$coord[, 1:3], 2),
                   ctr = round(mca$var$contrib[, 1:3], 1),
                   # squared cosines: quality of representation per axis;
                   # cos2_k = coord^2 / (1/f_k - 1), the chi-square distance
                   # of the category to the centre
                   cos2 = round(mca$var$coord[, 1:3]^2 /
                                  (n_tot / frec[rownames(mca$var$coord)] - 1),
                                3))
write_tab(cats, "tab_c10_categorias.csv")

vars_ctr <- data.frame(variable = sub("[.].*$", "", rownames(mca$var$contrib)),
                       mca$var$contrib[, 1:3]) |>
  group_by(variable) |>
  summarise(across(everything(), ~round(sum(.x), 1)), .groups = "drop")
write_tab(vars_ctr, "tab_c10_variables.csv")

estrato <- factor(m$estrato, levels = c("core", "middle", "outer"))
sup_of <- function(f, nombre) {
  s <- supvar(mca, f)
  data.frame(variable = nombre, categoria = rownames(s$coord),
             round(s$coord[, 1:3], 3), typic = round(s$typic[, 1:3], 1))
}
supvars <- bind_rows(
  sup_of(estrato, "estrato"),
  sup_of(factor(m$S_intensidad), "intensidad"),
  sup_of(factor(m$S_tenure), "tenure"),
  sup_of(factor(m$S_cohorte), "cohorte_arca"))
write_tab(supvars, "tab_c10_supvars.csv")

juri <- sup_of(factor(m$provincia), "jurisdiccion")
write_tab(juri, "tab_c10_jurisdicciones.csv")

# gaps in SD units, per axis, and correlation with the three-variable space
ind <- mca$ind$coord[, 1:3]
gaps <- sapply(1:3, function(d) abs(mean(ind[m$estrato == "outer", d]) -
    mean(ind[m$estrato == "core", d])) / sd(ind[, d]))
ejeT <- which.max(gaps)
X3 <- active_matrix_3(m)
mca3 <- speMCA(X3, excl = excl_index(X3, rare_categories(X3)), ncp = 2)
cc <- round(abs(cor(mca3$ind$coord[, 1:2], ind)), 2)
diagn <- data.frame(medida = c("gap_DE_dim1", "gap_DE_dim2", "gap_DE_dim3",
                               "eje_territorial",
                               "r_abs_3act_dim1", "r_abs_3act_dim2",
                               "ctr_registro_ejeT_pct"),
                    valor = c(round(gaps, 2), ejeT,
                              max(cc[1, ]), max(cc[2, ]),
                              round(sum(mca$var$contrib[
                                grepl("A_registro", rownames(mca$var$contrib)),
                                ejeT]), 1)))
write_tab(diagn, "tab_c10_diagnostico.csv")

cat("\nBenzecri:", round(mr[1:3], 1), " pasivadas:", excl, "\n")
cat("\nGap outer-core (DE):", round(gaps, 2), " eje territorial:", ejeT, "\n")
cat("Contribucion de A_registro al eje territorial:",
    diagn$valor[7], "% (regla: <=25 mantiene C10)\n")
cat("\nTop 8 del eje territorial (dim", ejeT, "):\n")
top <- order(-mca$var$contrib[, ejeT])[1:8]
print(data.frame(cat = rownames(mca$var$contrib)[top],
                 coord = round(mca$var$coord[top, ejeT], 2),
                 ctr = round(mca$var$contrib[top, ejeT], 1)), row.names = FALSE)
cat("\nTop 8 dims 1 y", setdiff(1:3, c(1, ejeT)), ":\n")
for (d in setdiff(1:3, ejeT)) {
  top <- order(-mca$var$contrib[, d])[1:8]
  cat("dim", d, ":\n")
  print(data.frame(cat = rownames(mca$var$contrib)[top],
                   coord = round(mca$var$coord[top, d], 2),
                   ctr = round(mca$var$contrib[top, d], 1)), row.names = FALSE)
}
cat("\nEstratos (coord / typic):\n")
print(supvars[supvars$variable == "estrato", ], row.names = FALSE)
cat("\nJurisdicciones ordenadas por el eje territorial:\n")
print(juri[order(juri[[paste0("dim.", ejeT)]]),
           c("categoria", paste0("dim.", ejeT), paste0("typic.dim.", ejeT))],
      row.names = FALSE)
