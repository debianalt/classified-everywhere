# 29 — The declared offer: a second cloud, and the volume battery it answers.
#
# The author's question was whether the active battery could reach the ten or
# so variables usual in survey-based social spaces. Two enlarged batteries
# settle it on the data:
#   (a) eleven variables of the REALISED relation (breadth of clients, buyers
#       and sectors, deflated scale, intensity, tenure, procedure profile)
#       collapse onto a single volume axis — the classic size effect;
#   (b) eleven CONSTITUTIVE variables — legal form, modal sector, modal client
#       plus eight binary families of capacity declared in SIPRO's Rubros
#       field at registration (100% coverage) — keep a multidimensional
#       structure in which the provisioning opposition reappears and the
#       territorial displacement almost vanishes. Suppliers declare similar
#       capacities everywhere; the gradient enters with the realised relation.
# The published three-variable space stays canonical (asserted below).
#
# Run: Rscript 29_oferta_declarada.R

source("theme_house.R", chdir = TRUE)
suppressMessages(library(GDAtools))

master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
m <- build_supplier_space(adj, master)

Xp <- active_matrix(m)
mcap <- speMCA(Xp, excl = excl_index(Xp, rare_categories(Xp)), ncp = 2)
mrp <- modif.rate(mcap)$modif$mrate
stopifnot(abs(mrp[1] - 43.6) < 0.15, abs(mrp[2] - 33.0) < 0.15)  # C10 canon intact
estrato <- factor(m$estrato, levels = c("core", "middle", "outer"))

resumen_espacio <- function(mca, etiqueta) {
  sv <- supvar(mca, estrato)
  cc <- abs(cor(mcap$ind$coord[, 1:2], mca$ind$coord[, 1:3]))
  list(
    benzecri = data.frame(bateria = etiqueta, dim = 1:3,
                          mrate = round(modif.rate(mca)$modif$mrate[1:3], 1)),
    estratos = data.frame(bateria = etiqueta, estrato = rownames(sv$coord),
                          round(sv$coord[, 1:3], 3),
                          typic = round(sv$typic[, 1:3], 1)),
    r = data.frame(bateria = etiqueta, eje_pub = c(1, 2),
                   mejor_eje = apply(cc, 1, which.max),
                   r_abs = round(apply(cc, 1, max), 2)))
}

# ── (a) the volume battery: the realised relation as active ──────────────────
ipc <- read.csv(file.path(DIR_PROC, "ipc_anual.csv"))
w <- adj |> filter(es_nuevo, !is.na(provincia), between(ejercicio, WIN0, WIN1)) |>
  left_join(ipc |> select(ejercicio = anio, deflactor_2024), by = "ejercicio") |>
  mutate(real = ifelse(moneda == "ARS", monto * deflactor_2024, NA))
extra <- w |> group_by(cuit) |> summarise(
  n_rubros = n_distinct(rubro_principal), n_tipos = n_distinct(tipo_organismo),
  n_orgs = n_distinct(organismo), total_real = sum(real, na.rm = TRUE),
  .groups = "drop")   # med_real ya viene de build_supplier_space
mv <- m |> left_join(extra, by = "cuit")
Xv <- mv |> transmute(
  A_personeria, A_rubro, A_cliente,
  A_nrubros = cut(n_rubros, c(0, 1, 2, Inf),
                  labels = c("1rubro", "2rubros", "3+rubros")),
  A_ntipos = cut(n_tipos, c(0, 1, Inf),
                 labels = c("mono_cliente", "multi_cliente")),
  A_norgs = cut(n_orgs, c(0, 1, 3, Inf), labels = c("1org", "2-3orgs", "4+orgs")),
  A_total = cut(total_real, quantile(total_real, c(0, .25, .5, .75, 1),
                                     na.rm = TRUE),
                labels = c("Q1total", "Q2total", "Q3total", "Q4total"),
                include.lowest = TRUE),
  A_medadj = cut(med_real, quantile(med_real, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                 labels = c("adj_chica", "adj_media", "adj_grande"),
                 include.lowest = TRUE),
  A_directa = S_directa, A_intensidad = S_intensidad, A_tenure = S_tenure) |>
  mutate(across(everything(), as.factor)) |> as.data.frame()
okv <- complete.cases(Xv)
mcav <- speMCA(Xv[okv, ], excl = excl_index(Xv[okv, ], rare_categories(Xv[okv, ])),
               ncp = 3)
svv <- supvar(mcav, estrato[okv])
ccv <- abs(cor(mcap$ind$coord[okv, 1:2], mcav$ind$coord[, 1:3]))
vol <- list(
  benzecri = data.frame(bateria = "volumen", dim = 1:3,
                        mrate = round(modif.rate(mcav)$modif$mrate[1:3], 1)),
  estratos = data.frame(bateria = "volumen", estrato = rownames(svv$coord),
                        round(svv$coord[, 1:3], 3),
                        typic = round(svv$typic[, 1:3], 1)),
  r = data.frame(bateria = "volumen", eje_pub = c(1, 2),
                 mejor_eje = apply(ccv, 1, which.max),
                 r_abs = round(apply(ccv, 1, max), 2)))
write_tab(bind_rows(vol$benzecri, vol$r |> rename(dim = eje_pub) |>
                      mutate(mrate = NA)), "tab_bateria_volumen.csv")

# ── (b) the declared-offer battery ───────────────────────────────────────────
sip <- read.csv(list.files(DIR_RAW, pattern = "sipro_proveedores.csv",
                           recursive = TRUE, full.names = TRUE)[1])
sip$cuit <- as.character(sip$CUIT_NIT)
dup <- sum(duplicated(sip$cuit))
sip <- sip[order(-nchar(sip$Rubros)), ]
sip <- sip[!duplicated(sip$cuit), ]   # keep the fullest Rubros per CUIT
cat("SIPRO: filas duplicadas por CUIT deduplicadas:", dup, "\n")
md <- m |> mutate(cuit = as.character(cuit)) |>
  left_join(sip |> select(cuit, Rubros), by = "cuit")
stopifnot(nrow(md) == nrow(m))
fam <- function(pat) factor(ifelse(grepl(pat, md$Rubros, fixed = TRUE),
                                   "si", "no"))
Xd <- data.frame(
  A_personeria = factor(md$A_personeria), A_rubro = factor(md$A_rubro),
  A_cliente = factor(md$A_cliente),
  D_profesional = fam("SERV. PROFESIONAL"),
  D_mantenimiento = fam("MANT. REPARACION"), D_equipos = fam("EQUIPOS"),
  D_repuestos = fam("REPUESTOS"), D_informatica = fam("INFORMATICA"),
  D_alquiler = fam("ALQUILER"), D_construccion = fam("CONSTRUCCION"),
  D_alimentos = fam("ALIMENTOS"))
mcad <- speMCA(Xd, excl = excl_index(Xd, rare_categories(Xd)), ncp = 3)
dec <- resumen_espacio(mcad, "declarada")
write_tab(dec$benzecri, "tab_oferta_declarada_benzecri.csv")
write_tab(data.frame(cat = rownames(mcad$var$contrib),
                     round(mcad$var$contrib[, 1:3], 1)) |>
            arrange(desc(dim.1)), "tab_oferta_declarada_contrib.csv")
write_tab(dec$estratos, "tab_oferta_declarada_estratos.csv")
write_tab(dec$r, "tab_oferta_declarada_r.csv")

cat("\n== Bateria de volumen ==\nBenzecri:", vol$benzecri$mrate, "\n")
print(vol$r, row.names = FALSE); print(vol$estratos, row.names = FALSE)
cat("\n== Bateria declarada ==\nBenzecri:", dec$benzecri$mrate, "\n")
print(dec$r, row.names = FALSE); print(dec$estratos, row.names = FALSE)
cat("\nAmplitud declarada (rubros por proveedor):\n")
n_decl <- lengths(regmatches(md$Rubros, gregexpr("[^;]+", md$Rubros)))
print(table(cut(n_decl, c(0, 1, 2, 4, 8, Inf),
                labels = c("1", "2", "3-4", "5-8", "9+")), useNA = "ifany"))
