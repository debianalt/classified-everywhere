# 16 — Bootstrap confidence intervals for the two claims that are stated as
# trends: the rise of the between-province share of supplier inequality, and
# the congruence gradient across province strata (external review 2.3).
#
# The congruence instrument recomputed here is the SEPARATE-MCA one (each
# stratum analysed on its own, category coordinates correlated against the
# metropolitan solution). It answers a different question from the csMCA of
# 11_mca_v2.R, which projects a subcloud into the reference space; both are
# reported, each for what it measures (GAN round 2).
#
# Run: Rscript 16_bootstrap_ci.R   (~5 min)

source("theme_house.R", chdir = TRUE)
suppressPackageStartupMessages({library(GDAtools); library(FactoMineR)})

set.seed(42)
B_THEIL <- 1000
B_CONG <- 200

# ── Theil between-province share, resampling suppliers within the year ───────
theil_between_share <- function(firm) {
  d <- firm[firm$monto > 0 & !is.na(firm$monto), ]
  if (nrow(d) < 2) return(NA_real_)
  x <- d$monto; mu <- mean(x); n <- length(x)
  total <- mean((x / mu) * log(x / mu))
  parts <- d |> group_by(provincia) |>
    summarise(ng = n(), mug = mean(monto), .groups = "drop")
  between <- sum((parts$ng / n) * (parts$mug / mu) * log(parts$mug / mu))
  between / total
}

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia), moneda == "ARS")

years <- WIN0:WIN1
firms_by_year <- lapply(years, function(yr) {
  adj |> filter(ejercicio == yr) |>
    group_by(cuit, provincia) |>
    summarise(monto = sum(monto, na.rm = TRUE), .groups = "drop")
})
names(firms_by_year) <- as.character(years)

reps_by_year <- lapply(firms_by_year, function(firm) {
  replicate(B_THEIL, {
    theil_between_share(firm[sample.int(nrow(firm), replace = TRUE), ])
  })
})

boot_theil <- bind_rows(lapply(seq_along(years), function(i) {
  r <- reps_by_year[[i]]
  tibble(ejercicio = years[i],
         n_firmas = nrow(firms_by_year[[i]]),
         share_between = theil_between_share(firms_by_year[[i]]),
         ic_lo = unname(quantile(r, 0.025, na.rm = TRUE)),
         ic_hi = unname(quantile(r, 0.975, na.rm = TRUE)),
         B = B_THEIL)
}))
write_tab(boot_theil |> mutate(across(where(is.numeric), ~round(.x, 4))),
          "tab_boot_theil_ci.csv")

# the headline is a change, so it gets its own interval: replicate differences
# between the closing and opening year of the publication window
delta <- reps_by_year[[as.character(WIN1)]] - reps_by_year[[as.character(WIN0)]]
boot_delta <- tibble(
  comparacion = paste(WIN1, "menos", WIN0),
  delta_observado = theil_between_share(firms_by_year[[as.character(WIN1)]]) -
    theil_between_share(firms_by_year[[as.character(WIN0)]]),
  ic_lo = unname(quantile(delta, 0.025, na.rm = TRUE)),
  ic_hi = unname(quantile(delta, 0.975, na.rm = TRUE)),
  p_delta_menor_igual_0 = mean(delta <= 0, na.rm = TRUE),
  B = B_THEIL)
write_tab(boot_delta |> mutate(across(where(is.numeric), ~round(.x, 4))),
          "tab_boot_theil_delta.csv")

# ── Congruence of separate per-stratum MCAs, with a bootstrap CI ─────────────
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
m <- build_supplier_space(
  read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")), master)
X <- active_matrix(m)

# category coordinates of an MCA fitted on one stratum only
RARE <- rare_categories(X)

mca_coords <- function(rows) {
  Xi <- droplevels(X[rows, , drop = FALSE])
  if (any(sapply(Xi, nlevels) < 2)) return(NULL)
  res <- try(speMCA(Xi, excl = excl_index(Xi, RARE), ncp = 3), silent = TRUE)
  if (inherits(res, "try-error")) return(NULL)
  cc <- as.data.frame(res$var$coord)[, 1:3, drop = FALSE]
  rownames(cc) <- rownames(res$var$coord)
  cc
}

# two measures of how alike two solutions are. The axis-by-axis congruence
# depends on which axis of one solution matches which of the other, and that
# match can flip; the RV coefficient compares the configurations as wholes and
# does not have that instability, so both are reported.
similarity <- function(base_cc, cc) {
  out <- c(cong = NA_real_, rv = NA_real_)
  if (is.null(base_cc) || is.null(cc)) return(out)
  common <- intersect(rownames(base_cc), rownames(cc))
  if (length(common) < 6) return(out)
  cmat <- suppressWarnings(abs(cor(base_cc[common, ], cc[common, ])))
  if (!all(is.na(cmat))) out["cong"] <- max(cmat[1, ], na.rm = TRUE)
  r <- try(coeffRV(as.matrix(base_cc[common, ]), as.matrix(cc[common, ])),
           silent = TRUE)
  if (!inherits(r, "try-error")) out["rv"] <- r$rv
  out
}

idx <- split(seq_len(nrow(m)), m$estrato)
base_cc <- mca_coords(idx$core)

obs <- sapply(c("core", "middle", "outer"),
              function(s) similarity(base_cc, mca_coords(idx[[s]])))

boot_cong <- bind_rows(lapply(c("middle", "outer"), function(s) {
  reps <- replicate(B_CONG, {
    b_base <- mca_coords(sample(idx$core, replace = TRUE))
    similarity(b_base, mca_coords(sample(idx[[s]], replace = TRUE)))
  })
  tibble(estrato = s, n = length(idx[[s]]),
         congruencia_dim1 = obs["cong", s],
         cong_ic_lo = unname(quantile(reps["cong", ], 0.025, na.rm = TRUE)),
         cong_ic_hi = unname(quantile(reps["cong", ], 0.975, na.rm = TRUE)),
         rv = obs["rv", s],
         rv_ic_lo = unname(quantile(reps["rv", ], 0.025, na.rm = TRUE)),
         rv_ic_hi = unname(quantile(reps["rv", ], 0.975, na.rm = TRUE)),
         replicas_validas = sum(!is.na(reps["cong", ])), B = B_CONG)
}))
write_tab(boot_cong |> mutate(across(where(is.numeric), ~round(.x, 3))),
          "tab_boot_congruencia_ci.csv")

cat("\nTheil between-share con IC 95% (bootstrap de proveedores):\n")
print(boot_theil |> mutate(across(c(share_between, ic_lo, ic_hi),
                                  ~round(100 * .x, 1))), n = 12)
cat("\nCambio 2017-2025:\n"); print(boot_delta)
cat("\nSimilitud con el sub-espacio core (MCAs separadas), IC 95%:\n")
print(boot_cong, width = 120)
