# 46 — De-identified release for the replication repository (3 Sep 2026).
#
# The award file and the supplier register are public downloads, but they carry
# taxpayer identifiers and names, and roughly two in five suppliers in this
# population are natural persons. Redistributing them as a curated dataset is a
# different act from citing the source, so the repository ships this instead:
# the two analysis matrices with a surrogate key in place of the identifier and
# no name in any column.
#
# What it buys: the whole geometric analysis, the gradient, the survival model
# and the exemption analysis run end to end from these two files, without the
# 244 MB of raw downloads and without a name leaving the project.
#
# What it does not claim: this is de-identified, not anonymous in the strict
# sense. The sources are public, so a reader holding them can re-link the rows.
# The surrogate key is a random permutation under a fixed seed, so the file
# itself leaks no order; it is not a defence against someone with the source.
#
# Run: Rscript 46_release_anonimizada.R   (~1 min)

source("theme_house.R", chdir = TRUE)

OUT <- file.path("data", "anonymised")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
apart <- read_parquet(file.path(DIR_PROC, "adjudicaciones_apartado.parquet")) |>
  select(doc_contractual, familia)
m <- build_supplier_space(adj, master)

# Surrogate key: a random permutation under a fixed seed, shared by the two
# files so they join. It is built over the 10,602 suppliers of the window, not
# the 10,580 of the geometry, so the award file carries the whole window and
# the chain of the paper is visible in the release: 163,126 awards to 10,602
# suppliers, of which 10,580 are recorded on all six active variables.
win <- adj |> filter(es_nuevo, !is.na(provincia),
                     between(ejercicio, WIN0, WIN1))
set.seed(20260903)
key <- tibble(cuit = sample(unique(win$cuit)), supplier_id = seq_along(cuit))

sup <- m |>
  inner_join(key, by = "cuit") |>
  transmute(supplier_id, provincia, estrato,
            A_personeria, A_rubro, A_cliente, A_escala, A_directa, A_registro,
            S_intensidad, S_tenure, S_cohorte, S_fundamento,
            n_adjudicaciones, primer_ejercicio, ultimo_ejercicio) |>
  arrange(supplier_id)
write.csv(sup, file.path(OUT, "supplier_space.csv"), row.names = FALSE,
          fileEncoding = "UTF-8")

# award level: everything the analyses use, nothing that identifies. The buyer
# is kept, because it is an organ of the state and the paper is about it; the
# supplier is the surrogate key. Amounts stay nominal, with the deflator in
# data/processed/ipc_anual.csv, so a replication can rebuild constant pesos.
aw <- win |>
  inner_join(key, by = "cuit") |>
  left_join(apart, by = "doc_contractual") |>
  transmute(supplier_id, ejercicio, era, organismo, tipo_organismo,
            anclaje_territorial, procedimiento, modalidad,
            rubro_principal, monto, moneda,
            provincia_domicilio_fiscal = provincia,
            personeria,
            fundamento = ifelse(is.na(familia), "competitivo", familia)) |>
  arrange(ejercicio, supplier_id)
# gzipped: 34 MB of plain CSV is paid by every clone, and both read.csv() and
# pandas.read_csv() open a .gz without ceremony
gz <- gzfile(file.path(OUT, "awards.csv.gz"), "w", encoding = "UTF-8")
write.csv(aw, gz, row.names = FALSE)
close(gz)

# the assertions the paper's numbers depend on
stopifnot(nrow(sup) == 10580, nrow(aw) == 163126,
          n_distinct(aw$supplier_id) == 10602,
          !any(grepl("cuit|proveedor|razon", names(sup), ignore.case = TRUE)),
          !any(grepl("cuit|proveedor|razon", names(aw), ignore.case = TRUE)))
cat(sprintf("\nsupplier_space.csv: %d proveedores, %d columnas\n", nrow(sup),
            ncol(sup)))
cat(sprintf("awards.csv: %d adjudicaciones, %d columnas\n", nrow(aw), ncol(aw)))
cat(sprintf("tamaño: %.1f MB\n",
            sum(file.size(file.path(OUT, c("supplier_space.csv",
                                           "awards.csv.gz")))) / 1048576))
