# 99 — Cross-validation: R canonical tables vs frozen Python first pass.
# Deterministic statistics must match within tolerance; geometric outputs
# (CA/MCA) are compared structurally in the scripts themselves, not here.
# Run: Rscript 99_crossval.R

source("theme_house.R", chdir = TRUE)

# pass2 is the Python freeze taken after the hand-check of the buyer typology
# (28-08-2026); pass1 is kept as the record of the first pass, against the
# regex-only classification, and is no longer the comparison set
PY <- "validation/python_pass2"
TOL <- 1e-6

EXACT <- c("tab_theil_provincia_anual.csv", "tab_theil_anidado_era.csv",
           "tab_shannon_rubros_provincia_era.csv", "tab_cohort_survival.csv",
           "tab_entry_exit_region.csv", "tab_procedimientos_region_era.csv",
           "tab_directa_provincia.csv", "tab_robustez_theil.csv",
           "tab_robustez_supervivencia.csv", "tab_empleo_publico_provincia.csv",
           "tab_contratar_provincia_ejecucion.csv", "tab_exit_ratio_v2.csv")
DOCUMENTED <- c("tab_legibilidad_exit.csv")  # sipro_sociedades: R excludes
# NA personería (Python 07 counted it as sociedad); intentional correction.
STRUCTURAL <- c("tab_ca_coords_provincia_tipo.csv", "tab_mca2_benzecri.csv",
                "tab_mca2_congruencia.csv",
                "tab_mca2_centroides_provincia_era.csv")

norm_keys <- function(df) {
  df |> mutate(across(where(is.character), ~tolower(trimws(.x))),
               across(where(is.logical), ~tolower(as.character(.x))))
}

compare <- function(f) {
  a <- tryCatch(read.csv(file.path(PY, f)), error = function(e) NULL)
  b <- tryCatch(read.csv(file.path(DIR_TAB, f)), error = function(e) NULL)
  if (is.null(a) || is.null(b))
    return(tibble(tabla = f, filas = NA, max_delta_rel = NA,
                  status = "FALTA ARCHIVO"))
  a <- norm_keys(a); b <- norm_keys(b)
  KEY_NUM <- c("ejercicio", "anio", "k", "dim")  # id columns, not values
  is_key <- function(df) !sapply(df, is.numeric) | names(df) %in% KEY_NUM
  keys <- intersect(names(a)[is_key(a)], names(b)[is_key(b)])
  nums <- setdiff(intersect(names(a)[sapply(a, is.numeric)],
                            names(b)[sapply(b, is.numeric)]), KEY_NUM)
  mrg <- merge(a[, c(keys, nums)], b[, c(keys, nums)],
               by = keys, suffixes = c("_py", "_r"))
  if (!nrow(mrg))
    return(tibble(tabla = f, filas = 0, max_delta_rel = NA,
                  status = "SIN ALINEACION"))
  deltas <- sapply(nums, function(v) {
    x <- mrg[[paste0(v, "_py")]]; y <- mrg[[paste0(v, "_r")]]
    both_na <- is.na(x) & is.na(y)
    d <- abs(x - y) / pmax(1, abs(x))
    max(d[!both_na], na.rm = TRUE)
  })
  md <- suppressWarnings(max(deltas, na.rm = TRUE))
  tibble(tabla = f, filas = nrow(mrg), max_delta_rel = md,
         status = ifelse(md <= TOL, "OK", "EXCEDE TOL"))
}

rep <- bind_rows(
  bind_rows(lapply(EXACT, compare)),
  bind_rows(lapply(DOCUMENTED, compare)) |>
    mutate(status = paste0(status, " (DIVERGENCIA DOCUMENTADA)")),
  tibble(tabla = STRUCTURAL, filas = NA, max_delta_rel = NA,
         status = "ESTRUCTURAL (no comparable numericamente)"))
write.csv(rep, "validation/crossval_report.csv", row.names = FALSE)
print(rep, n = 30)
fallas <- rep |> filter(status == "EXCEDE TOL")
cat("\n", ifelse(nrow(fallas) == 0,
                 "CROSSVAL OK: todas las tablas deterministas coinciden.",
                 paste("REVISAR:", nrow(fallas), "tablas exceden tolerancia.")),
    "\n")
