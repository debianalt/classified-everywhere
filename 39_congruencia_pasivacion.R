# 39 — What the per-province comparison hides: the second axis, and the price
# of a national passivation threshold (round 23, 30 Aug 2026).
#
# Two objections arrived together from the Bourdieusian review, and they are
# the same question asked twice.
#
#   (a) Script 18 reports the congruence of the province's FIRST axis with the
#       national one and nothing about the second — which is the axis the
#       argument rests on. The supplement promised "the axis-by-axis
#       congruence" and delivered one axis.
#   (b) The 5% rule is applied at the national level and the same categories
#       are passivated in every province (theme_house.R::RARE_THRESH, and the
#       comment above it says so). A category that is rare nationally but
#       common in one province is therefore absent from that province's
#       solution although the province has it in abundance. Five of the twelve
#       provinces have at least one such category; in Chaco, electrical and
#       telecoms reaches 6.2 per cent of suppliers against a national 5 per
#       cent floor.
#
# This script answers both without touching the published space: it adds the
# second axis to the per-province comparison, and refits every province a
# second time letting it passivate on its OWN threshold, so the RV can be read
# with and without the national rule. The size control of script 22 is
# repeated under provincial passivation, because a province that passivates
# locally keeps more categories and a bigger configuration flatters the RV.
#
# Run: Rscript 39_congruencia_pasivacion.R

source("theme_house.R", chdir = TRUE)
suppressPackageStartupMessages({library(GDAtools); library(FactoMineR)})

set.seed(42)
N_MIN <- 150
N_SUB <- 150
B <- 300

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
m <- build_supplier_space(adj, master)
X <- active_matrix(m)

RARE <- rare_categories(X)
nacional <- speMCA(X, excl = excl_index(X, RARE), ncp = 3)
mr_nac <- modif.rate(nacional)$modif$mrate
stopifnot(abs(mr_nac[1] - 43.6) < 0.15, abs(mr_nac[2] - 33.0) < 0.15)
cc_nac <- as.data.frame(nacional$var$coord)[, 1:3]
rownames(cc_nac) <- rownames(nacional$var$coord)

provs <- m |> count(provincia) |> filter(n >= N_MIN) |> arrange(desc(n))
estrato_of <- function(p) m$estrato[match(p, m$provincia)]

# ── which categories the national threshold takes away from each province ────
locales <- bind_rows(lapply(provs$provincia, function(pv) {
  Xi <- X[m$provincia == pv, , drop = FALSE]
  propias <- rare_categories(Xi)          # what the province would passivate
  # nationally passivated yet common enough locally to have stayed active
  perdidas <- setdiff(RARE, propias)
  tibble(provincia = pv, n_proveedores = nrow(Xi),
         pasivadas_nacionales = length(RARE),
         pasivadas_propias = length(propias),
         activas_localmente_perdidas = length(perdidas),
         cuales = if (length(perdidas)) paste(perdidas, collapse = "; ") else "-")
}))
write_tab(locales, "tab_pasivacion_provincial.csv")

# ── one fit, both passivation rules, both axes ───────────────────────────────
compara <- function(rows, excl_labels) {
  Xi <- droplevels(X[rows, , drop = FALSE])
  if (any(sapply(Xi, nlevels) < 2)) return(NULL)
  fit <- try(speMCA(Xi, excl = excl_index(Xi, excl_labels), ncp = 3),
             silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)
  cc <- as.data.frame(fit$var$coord)[, 1:3]
  rownames(cc) <- rownames(fit$var$coord)
  common <- intersect(rownames(cc_nac), rownames(cc))
  if (length(common) < 3) return(NULL)
  cmat <- suppressWarnings(abs(cor(cc_nac[common, ], cc[common, ])))
  if (any(is.na(cmat))) return(NULL)
  rv <- try(coeffRV(as.matrix(cc_nac[common, ]), as.matrix(cc[common, ])),
            silent = TRUE)
  if (inherits(rv, "try-error")) return(NULL)
  mr <- modif.rate(fit)$modif$mrate
  list(comunes = length(common), rv = rv$rv, rv_p = rv$p.value,
       mrate1 = mr[1], mrate2 = mr[2],
       # row 1 = the national first axis against the province's three axes
       c1_best = max(cmat[1, ]), c1_dim = which.max(cmat[1, ]),
       c1_vs_1 = cmat[1, 1],
       # row 2 = the national SECOND axis, the axis of access
       c2_best = max(cmat[2, ]), c2_dim = which.max(cmat[2, ]),
       c2_vs_2 = cmat[2, 2])
}

res <- bind_rows(lapply(provs$provincia, function(pv) {
  rows <- which(m$provincia == pv)
  nac <- compara(rows, RARE)
  prop <- compara(rows, rare_categories(X[rows, , drop = FALSE]))
  if (is.null(nac)) return(NULL)
  tibble(provincia = pv, estrato = estrato_of(pv),
         n_proveedores = length(rows),
         comunes = nac$comunes,
         rv_nacional = round(nac$rv, 3),
         congr_eje1 = round(nac$c1_best, 3), eje1_matchea = nac$c1_dim,
         congr_eje1_vs_1 = round(nac$c1_vs_1, 3),
         congr_eje2 = round(nac$c2_best, 3), eje2_matchea = nac$c2_dim,
         congr_eje2_vs_2 = round(nac$c2_vs_2, 3),
         comunes_prop = if (is.null(prop)) NA_integer_ else prop$comunes,
         rv_provincial = if (is.null(prop)) NA_real_ else round(prop$rv, 3),
         congr_eje2_prop = if (is.null(prop)) NA_real_ else round(prop$c2_best, 3))
}))
res <- res |> arrange(desc(n_proveedores))
write_tab(res, "tab_congruencia_eje2.csv")

# ── size control, this time under provincial passivation ─────────────────────
reps <- bind_rows(lapply(provs$provincia, function(pv) {
  idx <- which(m$provincia == pv)
  draws <- lapply(seq_len(B), function(b) {
    s <- sample(idx, N_SUB)
    compara(s, rare_categories(X[s, , drop = FALSE]))
  })
  ok <- Filter(Negate(is.null), draws)
  if (!length(ok)) return(NULL)
  tibble(provincia = pv, estrato = estrato_of(pv),
         replicas = length(ok),
         rv_mediano = round(median(sapply(ok, `[[`, "rv")), 3),
         ic_lo = round(quantile(sapply(ok, `[[`, "rv"), 0.025), 3),
         ic_hi = round(quantile(sapply(ok, `[[`, "rv"), 0.975), 3),
         eje2_mediano = round(median(sapply(ok, `[[`, "c2_best")), 3))
}))
write_tab(reps, "tab_pasivacion_size_control.csv")

por_estrato <- reps |> group_by(estrato) |>
  summarise(provincias = n(),
            rv_mediano = round(median(rv_mediano), 3),
            eje2_mediano = round(median(eje2_mediano), 3), .groups = "drop")
kw <- suppressWarnings(kruskal.test(rv_mediano ~ factor(estrato), data = reps))
kw2 <- suppressWarnings(kruskal.test(congr_eje2 ~ factor(estrato), data = res))
pruebas <- tibble(
  prueba = c("Kruskal-Wallis entre estratos, RV con pasivacion provincial a n = 150",
             "Kruskal-Wallis entre estratos, congruencia del eje 2 observada"),
  estadistico = round(c(kw$statistic, kw2$statistic), 3),
  p = signif(c(kw$p.value, kw2$p.value), 3),
  provincias = c(nrow(reps), nrow(res)))
write_tab(bind_rows(por_estrato |>
                      rename(grupo = estrato) |>
                      mutate(across(everything(), as.character))),
          "tab_pasivacion_por_estrato.csv")
write_tab(pruebas, "tab_pasivacion_kw.csv")

cat("\nCategorias activas localmente que el umbral nacional pasiva:\n")
print(as.data.frame(locales[, c("provincia", "n_proveedores",
                                "activas_localmente_perdidas", "cuales")]),
      row.names = FALSE)
cat("\nCongruencia por eje y RV bajo las dos reglas de pasivacion:\n")
print(as.data.frame(res), row.names = FALSE)
cat("\nControl de tamano con pasivacion provincial:\n")
print(as.data.frame(reps), row.names = FALSE)
cat("\nPor estrato:\n"); print(as.data.frame(por_estrato), row.names = FALSE)
cat("\nPruebas:\n"); print(as.data.frame(pruebas), row.names = FALSE)
