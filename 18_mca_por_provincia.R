# 18 — Descriptive MCA province by province (external review 1.3): the
# stratum-level fracture test is run on three groups of provinces, so the
# wording "regionalised sub-spaces" needs the province-level counterpart.
# Each province with enough adjudicated suppliers is analysed on its own and
# its first axis is compared with the axis of the pooled national space.
#
# Run: Rscript 18_mca_por_provincia.R

source("theme_house.R", chdir = TRUE)
suppressPackageStartupMessages({library(GDAtools); library(FactoMineR)})

set.seed(42)
N_MIN <- rv_n_min()
options(tab_suffix = rv_suffix(N_MIN))

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
m <- build_supplier_space(adj, master)
X <- active_matrix(m)

RARE <- rare_categories(X)
nacional <- speMCA(X, excl = excl_index(X, RARE), ncp = 3)
cc_nac <- as.data.frame(nacional$var$coord)[, 1:3]
rownames(cc_nac) <- rownames(nacional$var$coord)

estrato_of <- function(p) m$estrato[match(p, m$provincia)]

provs <- m |> count(provincia) |> filter(n >= N_MIN) |> arrange(desc(n))
cat(sprintf("Provincias con n >= %d: %d de %d\n", N_MIN, nrow(provs),
            n_distinct(m$provincia)))

res <- bind_rows(lapply(provs$provincia, function(pv) {
  Xi <- droplevels(X[m$provincia == pv, , drop = FALSE])
  if (any(sapply(Xi, nlevels) < 2)) return(NULL)
  fit <- try(speMCA(Xi, excl = excl_index(Xi, RARE), ncp = 3), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)
  mr <- modif.rate(fit)$modif$mrate
  cc <- as.data.frame(fit$var$coord)[, 1:3]
  rownames(cc) <- rownames(fit$var$coord)
  common <- intersect(rownames(cc_nac), rownames(cc))
  cmat <- suppressWarnings(abs(cor(cc_nac[common, ], cc[common, ])))
  # axis-by-axis congruence depends on which axis matches which, and the match
  # can flip between solutions; the RV coefficient compares the two category
  # configurations as wholes and does not have that instability
  rvres <- coeffRV(as.matrix(cc_nac[common, ]), as.matrix(cc[common, ]))
  tibble(provincia = pv,
         n_proveedores = sum(m$provincia == pv),
         estrato = estrato_of(pv),
         nea = pv %in% NEA,
         categorias_comunes = length(common),
         benzecri_dim1 = round(mr[1], 1),
         benzecri_dim2 = round(mr[2], 1),
         congruencia_dim1 = round(max(cmat[1, ], na.rm = TRUE), 3),
         dim_que_matchea = which.max(cmat[1, ]),
         congruencia_dim1_vs_dim1 = round(cmat[1, 1], 3),
         rv = round(rvres$rv, 3),
         rv_p = signif(rvres$p.value, 3))
}))

res <- res |> arrange(congruencia_dim1)
write_tab(res, "tab_mca_por_provincia.csv")

resumen <- res |> group_by(estrato) |>
  summarise(provincias = n(),
            congruencia_media = round(mean(congruencia_dim1), 3),
            congruencia_min = min(congruencia_dim1),
            congruencia_max = max(congruencia_dim1),
            rv_medio = round(mean(rv), 3),
            rv_min = min(rv), rv_max = max(rv),
            n_min = min(n_proveedores), n_max = max(n_proveedores),
            .groups = "drop")
write_tab(resumen, "tab_mca_por_provincia_resumen.csv")

kw <- suppressWarnings(list(
  cong = kruskal.test(congruencia_dim1 ~ factor(estrato), data = res),
  rv = kruskal.test(rv ~ factor(estrato), data = res)))
pruebas <- tibble(
  medida = c("congruencia dim1", "coeficiente RV"),
  chisq = round(c(kw$cong$statistic, kw$rv$statistic), 2),
  gl = c(kw$cong$parameter, kw$rv$parameter),
  p = signif(c(kw$cong$p.value, kw$rv$p.value), 3),
  prueba = paste0("Kruskal-Wallis entre estratos, ", nrow(res), " provincias"))
write_tab(pruebas, "tab_mca_por_provincia_kw.csv")

# A smaller province yields a noisier solution, which would depress the
# comparison on its own. The rank correlation has to be run against the
# statistic the argument actually rests on: it is the RV coefficient, not the
# axis-by-axis congruence, that orders the strata, and the two answer
# differently. Script 22 settles the question by equalising the samples.
tam <- bind_rows(lapply(
  list(c(medida = "congruencia dim1", col = "congruencia_dim1"),
       c(medida = "coeficiente RV", col = "rv")), function(x) {
    ct <- suppressWarnings(cor.test(res$n_proveedores, res[[x[["col"]]]],
                                    method = "spearman"))
    tibble(prueba = paste0("Spearman: n de proveedores vs ", x[["medida"]]),
           rho = round(unname(ct$estimate), 3),
           p = signif(ct$p.value, 3), provincias = nrow(res))
  }))
write_tab(tam, "tab_mca_por_provincia_tamano.csv")

p <- ggplot(res |> mutate(estrato = factor(STRATUM_LABELS[estrato],
                                           levels = STRATUM_LABELS)),
            aes(reorder(provincia, congruencia_dim1), congruencia_dim1,
                fill = estrato)) +
  geom_col(width = 0.72) +
  coord_flip() +
  scale_fill_manual(values = stratum_pal(), name = NULL) +
  labs(x = NULL,
       y = "Congruence of the jurisdiction's first axis with the national space") +
  theme_house()
pub(p, "S4", 3.6)

cat("\nMCA por provincia (ordenadas por congruencia):\n"); print(res, n = 30)
cat("\nResumen por estrato:\n"); print(resumen)
cat("\nKruskal-Wallis entre estratos:\n"); print(pruebas)
cat("\nConfusión por tamaño:\n"); print(tam)
