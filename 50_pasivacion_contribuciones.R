# 50 — What the 5% rule actually removes, measured on the published space.
#
# Methods passivate categories carried by fewer than 5% of suppliers, citing
# Le Roux and Rouanet. The evidence for that decision was measured on the
# pre-29-Aug three-variable battery and never re-measured after C10 replaced
# it with six actives, so the manuscript states a rule whose effect on the
# solution it publishes is unrecorded.
#
# Greenacre (2013) sets the standard the decision has to meet: rare objects
# usually contribute little, because low mass offsets a distant position, so
# deletion is unnecessary "with very few exceptions that can be easily
# identified using the tools described here". This script runs that tool on
# the six-active space: it fits the same battery WITHOUT passivation and reads
# the contributions the four rare categories take.
#
# No decision is coded here. The numbers decide what methods and the SI say.
#
# Run: Rscript 50_pasivacion_contribuciones.R

source("theme_house.R", chdir = TRUE)
suppressMessages(library(GDAtools))

master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
m <- build_supplier_space(adj, master)
stopifnot(nrow(m) == 10580)

X <- active_matrix(m)
excl <- rare_categories(X)
n_tot <- nrow(X)

# The published solution and the same battery with nothing passivated.
pub  <- speMCA(X, excl = excl_index(X, excl), ncp = 5)
full <- speMCA(X, excl = NULL, ncp = 5)

frec <- unlist(lapply(names(X), function(v)
  setNames(as.numeric(table(X[[v]])), paste0(v, ".", levels(X[[v]])))))

# 1. The four rare categories in the unpassivated fit: what they carry.
raras <- data.frame(
  categoria = excl,
  n = frec[excl],
  pct = round(100 * frec[excl] / n_tot, 2),
  round(full$var$coord[excl, 1:3], 2),
  round(full$var$contrib[excl, 1:3], 2),
  check.names = FALSE)
names(raras)[4:9] <- c(paste0("coord", 1:3), paste0("ctr", 1:3))
raras <- raras[order(-raras$ctr1), ]
write_tab(raras, "tab_pasivacion_raras.csv")

# The share of each axis the rule removes, against the share a category of
# average weight would carry.
q <- ncol(X)
k_full <- nrow(full$var$coord)
ctr_medio <- 100 / k_full
resumen_ctr <- data.frame(
  eje = 1:3,
  ctr_raras = round(colSums(full$var$contrib[excl, 1:3]), 2),
  ctr_esperada = round(length(excl) * ctr_medio, 2),
  razon = round(colSums(full$var$contrib[excl, 1:3]) /
                  (length(excl) * ctr_medio), 2))
write_tab(resumen_ctr, "tab_pasivacion_ctr_ejes.csv")

# 2. Eigenvalue profile of both solutions.
mr_pub  <- modif.rate(pub)$modif$mrate
mr_full <- modif.rate(full)$modif$mrate
autov <- data.frame(
  dim = 1:5,
  eigen_publicada = round(pub$eig$eigen[1:5], 4),
  benzecri_publicada = round(mr_pub[1:5], 1),
  eigen_sin_pasivar = round(full$eig$eigen[1:5], 4),
  benzecri_sin_pasivar = round(mr_full[1:5], 1))
write_tab(autov, "tab_pasivacion_autovalores.csv")

# 3. Do the two solutions describe the same space? Correlate the individuals'
#    coordinates and take, for each published axis, its best match.
cc <- abs(cor(pub$ind$coord[, 1:3], full$ind$coord[, 1:5]))
enlace <- data.frame(
  eje_publicado = 1:3,
  mejor_eje_sin_pasivar = apply(cc, 1, which.max),
  r = round(apply(cc, 1, max), 3))
write_tab(enlace, "tab_pasivacion_congruencia.csv")

cat("\n== Categorias que la regla del 5% pasiva ==\n")
print(raras, row.names = FALSE)
cat("\n== Contribucion conjunta de las cuatro, por eje ==\n")
cat("(ctr_esperada = lo que llevarian cuatro categorias de peso medio)\n")
print(resumen_ctr, row.names = FALSE)
cat("\n== Autovalores y tasas modificadas ==\n")
print(autov, row.names = FALSE)
cat("\n== Congruencia entre las dos soluciones (individuos) ==\n")
print(enlace, row.names = FALSE)
