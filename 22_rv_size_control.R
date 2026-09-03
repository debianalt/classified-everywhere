# 22 — Size control for the per-province RV result.
#
# Script 18 compares each province's own supplier space with the national one
# through the RV coefficient, and the stratum means order core > middle > outer.
# That ordering is also the ordering of the supplier populations: the core
# provinces hold between 325 and 3,954 suppliers and the outer ones between 157
# and 228. The rank correlation between size and RV is 0.308 (p = 0.331) on
# the C10 battery; it was 0.734 on the three-variable battery this comment
# was written for, so the correlation no longer carries the motivation on
# its own and the test below is what settles the question.
# A smaller cloud yields a noisier solution, which depresses the RV on its own,
# so the observed gradient cannot be read until the samples are equalised.
#
# The test: draw N_SUB suppliers from every province, refit the specific MCA
# with the same passivated categories, and recompute the RV against the national
# configuration. If the core provinces still exceed the outer ones at a common
# sample size, the finding survives. If they converge, the gradient is an
# artefact of how many suppliers each province has.
#
# Run: Rscript 22_rv_size_control.R

source("theme_house.R", chdir = TRUE)
suppressPackageStartupMessages({library(GDAtools); library(FactoMineR)})

set.seed(42)
N_MIN <- rv_n_min()   # same inclusion threshold as script 18
N_SUB <- N_MIN        # every province is drawn down to this
options(tab_suffix = rv_suffix(N_MIN))
B     <- 300

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
m <- build_supplier_space(adj, master)
X <- active_matrix(m)

RARE <- rare_categories(X)
nacional <- speMCA(X, excl = excl_index(X, RARE), ncp = 3)
cc_nac <- as.data.frame(nacional$var$coord)[, 1:3]
rownames(cc_nac) <- rownames(nacional$var$coord)

provs <- m |> count(provincia) |> filter(n >= N_MIN) |> arrange(desc(n))
cat(sprintf("Provincias con n >= %d: %d; submuestreo a n = %d, B = %d\n",
            N_MIN, nrow(provs), N_SUB, B))

# one RV against the national configuration, for an arbitrary set of rows
rv_of <- function(rows) {
  Xi <- droplevels(X[rows, , drop = FALSE])
  if (any(sapply(Xi, nlevels) < 2)) return(NULL)
  fit <- try(speMCA(Xi, excl = excl_index(Xi, RARE), ncp = 3), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)
  cc <- as.data.frame(fit$var$coord)[, 1:3]
  rownames(cc) <- rownames(fit$var$coord)
  common <- intersect(rownames(cc_nac), rownames(cc))
  if (length(common) < 3) return(NULL)
  r <- try(coeffRV(as.matrix(cc_nac[common, ]), as.matrix(cc[common, ])),
           silent = TRUE)
  if (inherits(r, "try-error")) return(NULL)
  c(rv = r$rv, comunes = length(common))
}

replicas <- bind_rows(lapply(provs$provincia, function(pv) {
  idx <- which(m$provincia == pv)
  obs <- rv_of(idx)
  draws <- lapply(seq_len(B), function(b) rv_of(sample(idx, N_SUB)))
  ok <- Filter(Negate(is.null), draws)
  cat(sprintf("  %-22s n = %5d  RV obs = %.3f  replicas validas = %d/%d\n",
              pv, length(idx), obs[["rv"]], length(ok), B))
  tibble(provincia = pv,
         estrato = m$estrato[idx[1]],
         n_proveedores = length(idx),
         rv_observado = round(unname(obs[["rv"]]), 3),
         rv = sapply(ok, function(x) x[["rv"]]),
         comunes = sapply(ok, function(x) x[["comunes"]]))
}))

por_prov <- replicas |> group_by(provincia, estrato, n_proveedores,
                                 rv_observado) |>
  summarise(replicas = n(),
            rv_mediano_n150 = round(median(rv), 3),
            ic_lo = round(quantile(rv, 0.025), 3),
            ic_hi = round(quantile(rv, 0.975), 3),
            comunes_medianas = median(comunes),
            .groups = "drop") |>
  arrange(desc(rv_mediano_n150))
write_tab(por_prov, "tab_rv_size_control.csv")

# stratum comparison at equal n: the replicates of every province in a stratum
# pooled, so the interval carries both the sampling noise and the spread between
# provinces of the same stratum
por_estrato <- replicas |> group_by(estrato) |>
  summarise(provincias = n_distinct(provincia), replicas = n(),
            rv_mediano_n150 = round(median(rv), 3),
            ic_lo = round(quantile(rv, 0.025), 3),
            ic_hi = round(quantile(rv, 0.975), 3),
            .groups = "drop") |>
  left_join(por_prov |> group_by(estrato) |>
              summarise(rv_observado_medio = round(mean(rv_observado), 3),
                        n_min = min(n_proveedores), n_max = max(n_proveedores),
                        .groups = "drop"), by = "estrato") |>
  arrange(desc(rv_mediano_n150))
write_tab(por_estrato, "tab_rv_size_control_estrato.csv")

# the same Kruskal-Wallis as script 18, now on size-equalised values, plus the
# two rank correlations that motivate the test
kw_obs <- suppressWarnings(kruskal.test(rv_observado ~ factor(estrato),
                                        data = por_prov))
kw_eq <- suppressWarnings(kruskal.test(rv_mediano_n150 ~ factor(estrato),
                                       data = por_prov))
ct_obs <- suppressWarnings(cor.test(por_prov$n_proveedores,
                                    por_prov$rv_observado, method = "spearman"))
ct_eq <- suppressWarnings(cor.test(por_prov$n_proveedores,
                                   por_prov$rv_mediano_n150,
                                   method = "spearman"))
pruebas <- tibble(
  prueba = c("Kruskal-Wallis entre estratos, RV observado",
             paste0("Kruskal-Wallis entre estratos, RV a n = ", N_SUB),
             "Spearman: n vs RV observado",
             paste0("Spearman: n vs RV a n = ", N_SUB)),
  estadistico = round(c(unname(kw_obs$statistic), unname(kw_eq$statistic),
                        unname(ct_obs$estimate), unname(ct_eq$estimate)), 3),
  p = signif(c(kw_obs$p.value, kw_eq$p.value, ct_obs$p.value, ct_eq$p.value), 3),
  provincias = nrow(por_prov))
write_tab(pruebas, "tab_rv_size_control_kw.csv")

# raw draws, so the minimum detectable effect below is recomputable
write_tab(replicas |> mutate(rv = round(rv, 4)) |>
            select(provincia, estrato, rv), "tab_rv_replicas.csv")

# minimum detectable effect of the equalised Kruskal-Wallis (round-12 review):
# a null that cannot exclude moderate differences is not evidence of
# homogeneity. Shift the outer provinces' replicate distributions down by
# delta, resample each province's median, and find the shift the test detects
# with power 0.8 at alpha 0.05.
draws_l <- split(replicas$rv, replicas$provincia)
estr_l <- replicas |> distinct(provincia, estrato)
estr_v <- setNames(as.character(estr_l$estrato), estr_l$provincia)
SIM <- 1000
mde_power <- function(delta) {
  mean(replicate(SIM, {
    med <- vapply(names(draws_l), function(pv) {
      v <- draws_l[[pv]]
      median(sample(v, length(v), replace = TRUE)) -
        if (estr_v[[pv]] == "outer") delta else 0
    }, numeric(1))
    suppressWarnings(
      kruskal.test(med, factor(estr_v[names(draws_l)]))$p.value) < 0.05
  }))
}
deltas <- seq(0, 0.30, 0.05)
potencia <- vapply(deltas, mde_power, numeric(1))
write_tab(tibble(delta_rv_outer = deltas, potencia = round(potencia, 3)),
          "tab_rv_mde.csv")
cat("\nMDE del Kruskal-Wallis igualado (potencia por delta):\n")
print(tibble(delta = deltas, potencia = round(potencia, 3)), n = 20)

p <- ggplot(por_prov |> mutate(estrato = factor(STRATUM_LABELS[estrato],
                                                levels = STRATUM_LABELS)),
            aes(reorder(provincia, rv_mediano_n150), rv_mediano_n150,
                colour = estrato)) +
  geom_linerange(aes(ymin = ic_lo, ymax = ic_hi), linewidth = 0.4) +
  geom_point(size = 1.5) +
  geom_point(aes(y = rv_observado), shape = 4, size = 1.5, stroke = 0.6,
             show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = 0.06)) +
  scale_colour_manual(values = stratum_pal(), name = NULL) +
  labs(x = NULL,
       y = sprintf("RV against the national configuration at n = %d (crosses: observed)",
                   N_SUB)) +
  theme(panel.grid.major.y = element_blank()) +
  theme_house()
pub(p, "S5", 3.8)

cat("\nRV por provincia, observado y a n comun:\n"); print(por_prov, n = 20)
cat("\nRV por estrato a n comun:\n"); print(por_estrato)
cat("\nPruebas:\n"); print(pruebas)
