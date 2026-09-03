# 15 — Outcome-leakage check for the MCA (external review 2.1 / 4.1).
# Model E (endowments only: personería + rubro + cohorte de registro) vs
# Model F (the canonical six-variable battery of theme_house::active_matrix).
# If Model E reproduces the axis structure and the relational categories
# project strongly as supplementary, the geometry is not an artefact of
# including the buyer relation and the procedure among actives.
# Run: Rscript 15_mca_endowments_check.R

source("theme_house.R", chdir = TRUE)
suppressPackageStartupMessages(library(GDAtools))
set.seed(42)

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
m <- build_supplier_space(adj, master)
RARE <- rare_categories(active_matrix(m))

XE <- m |> select(A_personeria, A_rubro, A_registro) |>
  mutate(across(everything(), as.factor)) |> as.data.frame()
XF <- active_matrix(m)

me <- speMCA(XE, excl = excl_index(XE, RARE), ncp = 3)
mf <- speMCA(XF, excl = excl_index(XF, RARE), ncp = 3)
bze <- modif.rate(me)$modif$mrate
bzf <- modif.rate(mf)$modif$mrate

ce <- as.data.frame(me$ind$coord)[, 1:2]
cf <- as.data.frame(mf$ind$coord)[, 1:2]
cors <- abs(cor(ce, cf))

sv_cli <- suppressWarnings(varsup(me, factor(m$A_cliente)))
sv_est <- suppressWarnings(varsup(me, factor(m$estrato)))

res <- tibble(
  metrica = c("benzecri_dim1", "benzecri_dim2",
              "cor_dim1_E_vs_F_bestmatch", "cor_dim2_E_vs_F_bestmatch",
              "typic_seguridad_dim1_en_E", "typic_seguridad_dim2_en_E",
              "typic_outer_dim1_en_E", "typic_outer_dim2_en_E"),
  modelo_E = c(bze[1], bze[2], max(cors[1, ]), max(cors[2, ]),
               as.data.frame(sv_cli$typic)["seguridad_defensa", 1],
               as.data.frame(sv_cli$typic)["seguridad_defensa", 2],
               as.data.frame(sv_est$typic)["outer", 1],
               as.data.frame(sv_est$typic)["outer", 2]),
  modelo_F = c(bzf[1], bzf[2], NA, NA, NA, NA, NA, NA))
write_tab(res |> mutate(across(where(is.numeric), ~round(.x, 3))),
          "tab_mca_endowments_check.csv")

cat("\nModelo E (solo dotaciones): Benzécri", round(bze[1:2], 1), "\n")
cat("Modelo F (con cliente):      Benzécri", round(bzf[1:2], 1), "\n")
cat("Correlación |dims E vs F| (matriz):\n"); print(round(cors, 3))
cat("\nCliente como suplementaria en E (coordenadas dim1/dim2):\n")
print(round(as.data.frame(sv_cli$coord)[, 1:2], 2))
cat("\nTypicality estrato outer en E:",
    unlist(round(as.data.frame(sv_est$typic)["outer", 1:2], 2)), "\n")
