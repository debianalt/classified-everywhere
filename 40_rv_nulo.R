# 40 — What an RV of 0.5 means: the null distribution of the coefficient under
# identical structure (round 24, 30 Aug 2026).
#
# The reviewer's objection is exact. The manuscript treats overlapping
# intervals as compatibility with equality, but never says what the
# coefficient would look like IF the structures were identical and only
# sampling noise separated them. The survival null is stated as an equivalence
# (Table S17b, two one-sided tests inside a declared margin); the RV null is
# not, and the power calculation of script 22 is about the replicate
# distributions, not about the distance between two configurations.
#
# Two reference distributions, both built so that structural equality holds by
# construction:
#
#   (a) the NULL: draw 150 individuals at random from the whole cloud —
#       ignoring province, so the draw has the national structure by
#       construction — refit the specific analysis with the same passivated
#       categories, and compute the RV against the national configuration.
#       Whatever a province of 150 could reach if it were a miniature of the
#       country, this distribution contains.
#
#   (b) the CEILING: split the cloud into two halves and compute the RV
#       between the two solutions. This is the coefficient's test-retest
#       reliability at half the national size, and it answers the second half
#       of the objection: what counts as a high RV here.
#
# The comparison then reads off whether each province sits inside the null
# band. A province below it differs structurally; a province inside it is
# indistinguishable from a random slice of the country.
#
# Run: Rscript 40_rv_nulo.R

source("theme_house.R", chdir = TRUE)
suppressPackageStartupMessages({library(GDAtools); library(FactoMineR)})

set.seed(42)
N_MIN <- rv_n_min()
N_SUB <- N_MIN
B <- 400
options(tab_suffix = rv_suffix(N_MIN))

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
m <- build_supplier_space(adj, master)
X <- active_matrix(m)

RARE <- rare_categories(X)
nacional <- speMCA(X, excl = excl_index(X, RARE), ncp = 3)
mr <- modif.rate(nacional)$modif$mrate
stopifnot(abs(mr[1] - 43.6) < 0.15, abs(mr[2] - 33.0) < 0.15)
cc_nac <- as.data.frame(nacional$var$coord)[, 1:3]
rownames(cc_nac) <- rownames(nacional$var$coord)

fit_rows <- function(rows) {
  Xi <- droplevels(X[rows, , drop = FALSE])
  if (any(sapply(Xi, nlevels) < 2)) return(NULL)
  f <- try(speMCA(Xi, excl = excl_index(Xi, RARE), ncp = 3), silent = TRUE)
  if (inherits(f, "try-error")) return(NULL)
  cc <- as.data.frame(f$var$coord)[, 1:3]
  rownames(cc) <- rownames(f$var$coord)
  cc
}

rv_vs <- function(cc_a, cc_b) {
  common <- intersect(rownames(cc_a), rownames(cc_b))
  if (length(common) < 3) return(NULL)
  r <- try(coeffRV(as.matrix(cc_a[common, ]), as.matrix(cc_b[common, ])),
           silent = TRUE)
  if (inherits(r, "try-error")) return(NULL)
  c(rv = r$rv, comunes = length(common))
}

# ── (a) the null: random slices of the country, at provincial size ───────────
nulo <- do.call(rbind, lapply(seq_len(B), function(b) {
  cc <- fit_rows(sample(nrow(X), N_SUB))
  if (is.null(cc)) return(NULL)
  rv_vs(cc_nac, cc)
}))
nulo <- as.data.frame(nulo)
q <- quantile(nulo$rv, c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975))
cat(sprintf("\nNULO (n = %d, %d replicas validas): mediana %.3f, IC95 [%.3f, %.3f]\n",
            N_SUB, nrow(nulo), q[["50%"]], q[["2.5%"]], q[["97.5%"]]))

# ── (b) the ceiling: two halves of the same cloud against each other ─────────
techo <- do.call(rbind, lapply(seq_len(B %/% 2), function(b) {
  i <- sample(nrow(X))
  h1 <- fit_rows(i[seq_len(floor(nrow(X) / 2))])
  h2 <- fit_rows(i[-seq_len(floor(nrow(X) / 2))])
  if (is.null(h1) || is.null(h2)) return(NULL)
  rv_vs(h1, h2)
}))
techo <- as.data.frame(techo)
qt <- quantile(techo$rv, c(0.025, 0.5, 0.975))
cat(sprintf("TECHO (dos mitades del pais): mediana %.3f, IC95 [%.3f, %.3f]\n",
            qt[["50%"]], qt[["2.5%"]], qt[["97.5%"]]))

referencia <- tibble(
  # the label must follow N_SUB, or a replication run writes the canonical
  # threshold into its own table
  distribucion = c(sprintf("Random slices of the country at n = %d", N_SUB),
                   "Two halves of the country against each other"),
  replicas = c(nrow(nulo), nrow(techo)),
  p2.5 = round(c(q[["2.5%"]], qt[["2.5%"]]), 3),
  mediana = round(c(q[["50%"]], qt[["50%"]]), 3),
  p97.5 = round(c(q[["97.5%"]], qt[["97.5%"]]), 3),
  comunes_medianas = c(median(nulo$comunes), median(techo$comunes)))
write_tab(referencia, "tab_rv_nulo_referencia.csv")

# ── each province against the null band ──────────────────────────────────────
obs <- read_tab("tab_rv_size_control.csv")
lo <- q[["2.5%"]]; hi <- q[["97.5%"]]
comp <- obs |>
  transmute(provincia, estrato,
            n_proveedores,
            rv_observado,
            rv_mediano_n150,
            dentro_banda_nula = rv_mediano_n150 >= lo & rv_mediano_n150 <= hi,
            posicion = case_when(rv_mediano_n150 < lo ~ "below",
                                 rv_mediano_n150 > hi ~ "above",
                                 TRUE ~ "inside"),
            percentil_en_el_nulo = round(
              sapply(rv_mediano_n150, function(v) mean(nulo$rv <= v)), 3)) |>
  arrange(rv_mediano_n150)
write_tab(comp, "tab_rv_nulo_provincias.csv")

resumen <- comp |> group_by(estrato) |>
  summarise(provincias = n(),
            dentro = sum(dentro_banda_nula),
            percentil_mediano = round(median(percentil_en_el_nulo), 3),
            .groups = "drop")
write_tab(resumen, "tab_rv_nulo_estratos.csv")

cat("\nProvincias contra la banda nula:\n")
print(as.data.frame(comp), row.names = FALSE)
cat("\nPor estrato (cuantas caen dentro de la banda):\n")
print(as.data.frame(resumen), row.names = FALSE)
cat(sprintf("\nDentro de la banda nula: %d de %d provincias\n",
            sum(comp$dentro_banda_nula), nrow(comp)))
