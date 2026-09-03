# 41 — The RV comparison on a fixed common set of active categories.
#
# The open objection to the structural null (external review, 30 Aug): the RV
# coefficient and the class-specific analysis compare only the categories two
# configurations share, so an opposition carried by a category a province does
# not have is outside what either instrument can see. Worse, the shared set is
# not the same from one comparison to the next: script 18 intersects each
# province's own solution with the national one, which gives 25 categories in
# ten provinces and 24 in Misiones and Chaco. Different provinces are therefore
# compared on different category sets, and that is a dependence on the
# instrument rather than on the market.
#
# The test: fix one set of active categories ex ante — the national active
# categories that every compared province actually carries — refit the national
# reference and each province on that same set, and recompute the RV over all of
# it rather than over a per-province intersection. Run twice: on the observed
# provincial samples, and with every province drawn down to a common n, since it
# is the size-equalised version that the paper's reading rests on.
#
# Run: Rscript 41_rv_conjunto_fijo.R

source("theme_house.R", chdir = TRUE)
suppressPackageStartupMessages({library(GDAtools); library(FactoMineR)})

set.seed(42)
N_MIN <- rv_n_min()   # same inclusion threshold as scripts 18 and 22
N_SUB <- N_MIN        # same equalising draw as script 22
options(tab_suffix = rv_suffix(N_MIN))
B     <- 300

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
m <- build_supplier_space(adj, master)
X <- active_matrix(m)

RARE <- rare_categories(X)
ACTIVE_NAC <- setdiff(category_names(X), RARE)

provs <- m |> count(provincia) |> filter(n >= N_MIN) |> arrange(desc(n))
cat(sprintf("Provincias con n >= %d: %d de %d\n", N_MIN, nrow(provs),
            n_distinct(m$provincia)))

# ---- which national active categories are not carried everywhere ------------
# a category with no individuals in a province has no position there, so it is
# what the per-province intersection was silently dropping
conteo <- function(rows) {
  Xi <- X[rows, , drop = FALSE]
  unlist(lapply(names(Xi), function(v) {
    setNames(as.numeric(table(factor(Xi[[v]], levels = levels(X[[v]])))),
             paste0(v, ".", levels(X[[v]])))
  }))
}

cuentas <- sapply(provs$provincia, function(pv) conteo(which(m$provincia == pv)))
cuentas <- cuentas[ACTIVE_NAC, , drop = FALSE]
ausentes <- rownames(cuentas)[apply(cuentas, 1, function(r) any(r == 0))]

frec_nac <- conteo(seq_len(nrow(X)))
detalle <- tibble(
  categoria = ausentes,
  suppliers_nacional = as.integer(frec_nac[ausentes]),
  pct_nacional = round(100 * frec_nac[ausentes] / nrow(X), 2),
  provincias_sin = sapply(ausentes, function(k)
    paste(colnames(cuentas)[cuentas[k, ] == 0], collapse = "; ")))
write_tab(detalle, "tab_rv_categorias_ausentes.csv")
cat("\nCategorias activas nacionales que alguna provincia no lleva:\n")
if (nrow(detalle) == 0) cat("  ninguna\n") else print(detalle, n = 20, width = 200)

FIJO <- setdiff(ACTIVE_NAC, ausentes)
EXCL_FIJO <- union(RARE, ausentes)
cat(sprintf("\nConjunto fijo: %d categorias activas (nacional: %d; retiradas: %d)\n",
            length(FIJO), length(ACTIVE_NAC), length(ausentes)))

# ---- the national reference, refitted on the fixed set ----------------------
nac_fijo <- speMCA(X, excl = excl_index(X, EXCL_FIJO), ncp = 3)
cc_fijo <- as.data.frame(nac_fijo$var$coord)[, 1:3]
rownames(cc_fijo) <- rownames(nac_fijo$var$coord)
stopifnot(setequal(rownames(cc_fijo), FIJO))

# the variable-set reference of scripts 18 and 22, kept so the two readings sit
# in one table
nac_var <- speMCA(X, excl = excl_index(X, RARE), ncp = 3)
cc_var <- as.data.frame(nac_var$var$coord)[, 1:3]
rownames(cc_var) <- rownames(nac_var$var$coord)

# ---- one RV, on either protocol ---------------------------------------------
rv_of <- function(rows, fijo = TRUE) {
  Xi <- droplevels(X[rows, , drop = FALSE])
  if (any(sapply(Xi, nlevels) < 2)) return(NULL)
  excl <- if (fijo) EXCL_FIJO else RARE
  ref <- if (fijo) cc_fijo else cc_var
  fit <- try(speMCA(Xi, excl = excl_index(Xi, excl), ncp = 3), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)
  cc <- as.data.frame(fit$var$coord)[, 1:3]
  rownames(cc) <- rownames(fit$var$coord)
  cats <- if (fijo) {
    # on the fixed protocol the two sets must coincide; a draw that loses a
    # category is dropped rather than silently compared on fewer
    if (!setequal(rownames(cc), rownames(ref))) return(NULL)
    rownames(ref)
  } else intersect(rownames(ref), rownames(cc))
  if (length(cats) < 3) return(NULL)
  r <- try(coeffRV(as.matrix(ref[cats, ]), as.matrix(cc[cats, ])), silent = TRUE)
  if (inherits(r, "try-error")) return(NULL)
  c(rv = r$rv, cats = length(cats))
}

# ---- observed samples --------------------------------------------------------
obs <- bind_rows(lapply(provs$provincia, function(pv) {
  idx <- which(m$provincia == pv)
  a <- rv_of(idx, fijo = TRUE)
  b <- rv_of(idx, fijo = FALSE)
  tibble(provincia = pv,
         estrato = m$estrato[idx[1]],
         n_proveedores = length(idx),
         rv_conjunto_variable = round(unname(b[["rv"]]), 3),
         categorias_variable = unname(b[["cats"]]),
         rv_conjunto_fijo = round(unname(a[["rv"]]), 3),
         categorias_fijo = unname(a[["cats"]]))
})) |> arrange(desc(rv_conjunto_fijo))

# ---- the same, with every province drawn down to a common n ------------------
cat(sprintf("\nSubmuestreo a n = %d, B = %d, sobre el conjunto fijo\n", N_SUB, B))
replicas <- bind_rows(lapply(provs$provincia, function(pv) {
  idx <- which(m$provincia == pv)
  draws <- lapply(seq_len(B), function(b) rv_of(sample(idx, N_SUB), fijo = TRUE))
  ok <- Filter(Negate(is.null), draws)
  cat(sprintf("  %-22s n = %5d  replicas validas = %d/%d\n",
              pv, length(idx), length(ok), B))
  tibble(provincia = pv, estrato = m$estrato[idx[1]],
         rv = sapply(ok, function(x) x[["rv"]]))
}))

igualado <- replicas |> group_by(provincia, estrato) |>
  summarise(replicas = n(),
            rv_fijo_n150 = round(median(rv), 3),
            ic_lo = round(quantile(rv, 0.025), 3),
            ic_hi = round(quantile(rv, 0.975), 3), .groups = "drop")

por_prov <- obs |> left_join(igualado, by = c("provincia", "estrato")) |>
  arrange(desc(rv_fijo_n150))
write_tab(por_prov, "tab_rv_conjunto_fijo.csv")

por_estrato <- replicas |> group_by(estrato) |>
  summarise(provincias = n_distinct(provincia), replicas = n(),
            rv_fijo_n150 = round(median(rv), 3),
            ic_lo = round(quantile(rv, 0.025), 3),
            ic_hi = round(quantile(rv, 0.975), 3), .groups = "drop") |>
  left_join(obs |> group_by(estrato) |>
              summarise(rv_variable_medio = round(mean(rv_conjunto_variable), 3),
                        rv_fijo_medio = round(mean(rv_conjunto_fijo), 3),
                        .groups = "drop"), by = "estrato") |>
  arrange(desc(rv_fijo_n150))
write_tab(por_estrato, "tab_rv_conjunto_fijo_estrato.csv")

# ---- the tests ---------------------------------------------------------------
kw_var <- suppressWarnings(kruskal.test(rv_conjunto_variable ~ factor(estrato),
                                        data = por_prov))
kw_fijo <- suppressWarnings(kruskal.test(rv_conjunto_fijo ~ factor(estrato),
                                         data = por_prov))
kw_eq <- suppressWarnings(kruskal.test(rv_fijo_n150 ~ factor(estrato),
                                       data = por_prov))
ct_fijo <- suppressWarnings(cor.test(por_prov$n_proveedores,
                                     por_prov$rv_conjunto_fijo,
                                     method = "spearman"))
ct_eq <- suppressWarnings(cor.test(por_prov$n_proveedores, por_prov$rv_fijo_n150,
                                   method = "spearman"))
ct_par <- suppressWarnings(cor.test(por_prov$rv_conjunto_variable,
                                    por_prov$rv_conjunto_fijo,
                                    method = "spearman"))
pruebas <- tibble(
  prueba = c("Kruskal-Wallis entre estratos, RV conjunto variable",
             "Kruskal-Wallis entre estratos, RV conjunto fijo",
             paste0("Kruskal-Wallis entre estratos, RV conjunto fijo a n = ", N_SUB),
             "Spearman: n vs RV conjunto fijo",
             paste0("Spearman: n vs RV conjunto fijo a n = ", N_SUB),
             "Spearman: RV variable vs RV fijo"),
  estadistico = round(c(unname(kw_var$statistic), unname(kw_fijo$statistic),
                        unname(kw_eq$statistic), unname(ct_fijo$estimate),
                        unname(ct_eq$estimate), unname(ct_par$estimate)), 3),
  p = signif(c(kw_var$p.value, kw_fijo$p.value, kw_eq$p.value,
               ct_fijo$p.value, ct_eq$p.value, ct_par$p.value), 3),
  provincias = nrow(por_prov))
write_tab(pruebas, "tab_rv_conjunto_fijo_kw.csv")

cat("\nRV por provincia, conjunto variable y conjunto fijo:\n")
print(por_prov, n = 20, width = 200)
cat("\nRV por estrato:\n"); print(por_estrato, width = 200)
cat("\nPruebas:\n"); print(pruebas, width = 200)
