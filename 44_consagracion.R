# 44 — The register of justification: counting, naming, competing.
#
# COMPR.AR records the legal ground a buying organ invoked whenever it departed
# from open competition (script 43). Two grounds do opposite symbolic work:
#
#   arithmetic  the award is small enough that no competition is required.
#               The threshold decides and nobody is named.
#   qualitative the state declares this supplier the only possible one
#               (exclusivity) or its competence the one required (specialty).
#               Here the state names, and the naming is the ground.
#
# The claim tested is NOT that the state names arbitrarily: exclusivity often
# records a genuine technical monopoly. It is that the FORM of the exception
# differs by position — arithmetic and impersonal at one pole of the space,
# qualitative and nominative at the other — whatever its technical warrant.
# The within-sector control is what makes that reading defensible: if the
# difference were only that some sectors have single suppliers, it would
# disappear once the sector is held fixed.
#
# Definitions follow the manuscript: a natural person is
# personeria == "Persona Fisica" (as in scripts 21 and 32), and the shares are
# computed over awards that carry a ground at all, since the field exists only
# where competition was departed from.
#
# Run: Rscript 44_consagracion.R   (~2 min)

source("theme_house.R", chdir = TRUE)

set.seed(42)
B <- 1000
N_MIN_RUBRO <- 300   # sectors with fewer grounded awards are not read on their own

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia), between(ejercicio, WIN0, WIN1))
apart <- read_parquet(file.path(DIR_PROC, "adjudicaciones_apartado.parquet"))

d <- adj |>
  left_join(apart |> select(doc_contractual, familia), by = "doc_contractual") |>
  mutate(familia = ifelse(is.na(familia), "competitivo", familia),
         pf = personeria == "Persona Fisica",
         cual = familia == "cualitativo")

cat(sprintf("ventana: %d adjudicaciones, %d proveedores\n",
            nrow(d), n_distinct(d$cuit)))
cat(sprintf("con fundamento: %d (%.1f%%) | cualitativo: %d | aritmético: %d\n",
            sum(d$familia != "competitivo"),
            100 * mean(d$familia != "competitivo"),
            sum(d$cual), sum(d$familia == "cuantitativo")))

g <- d |> filter(familia != "competitivo", !is.na(pf))

# ── The 2x2: anchoring of the buyer x legal form of the supplier ─────────────
# Cluster bootstrap on suppliers, since awards are nested in them and the cells
# are dominated by a few high-volume suppliers.
cuits <- unique(g$cuit)
idx <- split(seq_len(nrow(g)), g$cuit)
cell_share <- function(rows) {
  h <- g[rows, ]
  h |> group_by(anclaje_territorial, pf) |>
    summarise(p = mean(cual), .groups = "drop")
}
obs <- g |> group_by(anclaje_territorial, pf) |>
  summarise(n = n(), p = mean(cual), .groups = "drop")
boot <- lapply(seq_len(B), function(i) {
  rows <- unlist(idx[sample(cuits, length(cuits), replace = TRUE)],
                 use.names = FALSE)
  cell_share(rows)
}) |> bind_rows()
ci <- boot |> group_by(anclaje_territorial, pf) |>
  summarise(lo = quantile(p, 0.025), hi = quantile(p, 0.975), .groups = "drop")
dos_x_dos <- obs |> left_join(ci, by = c("anclaje_territorial", "pf")) |>
  mutate(comprador = ifelse(anclaje_territorial, "anclado", "programatico"),
         proveedor = ifelse(pf, "persona_fisica", "sociedad"),
         across(c(p, lo, hi), ~round(100 * .x, 2))) |>
  select(comprador, proveedor, n, pct_cualitativo = p, lo, hi) |>
  arrange(comprador, proveedor)
write_tab(dos_x_dos, "tab_consagracion_2x2.csv")
cat("\nFundamento cualitativo, % de las adjudicaciones con fundamento:\n")
print(as.data.frame(dos_x_dos))

# ── The full split by family, by anchoring (Table S28b) ─────────────────────
# The body quotes the share settled on the amount alone (91.7 anchored, 67.7
# programmatic); it has to come from a table, not from a console print.
por_familia <- g |> group_by(anclaje_territorial, familia) |>
  summarise(n = n(), .groups = "drop_last") |>
  mutate(pct = round(100 * n / sum(n), 2)) |> ungroup() |>
  mutate(comprador = ifelse(anclaje_territorial, "anclado", "programatico")) |>
  select(comprador, familia, n, pct) |> arrange(comprador, desc(n))
write_tab(por_familia, "tab_consagracion_familia_anclaje.csv")
cat("\nFamilias del fundamento por anclaje:\n")
print(as.data.frame(por_familia))

# ── Within-sector control ────────────────────────────────────────────────────
por_rubro <- g |> group_by(rubro_principal) |>
  filter(n() >= N_MIN_RUBRO) |>
  summarise(n = n(),
            pct_pf = round(100 * mean(cual[pf]), 2),
            pct_soc = round(100 * mean(cual[!pf]), 2),
            n_pf = sum(pf), n_soc = sum(!pf), .groups = "drop") |>
  mutate(mayor_en_sociedades = pct_soc > pct_pf) |>
  arrange(desc(n))
write_tab(por_rubro, "tab_consagracion_rubro.csv")
cat(sprintf("\nControl por rubro: la sociedad recibe más fundamento cualitativo en %d de %d sectores con %d+ adjudicaciones\n",
            sum(por_rubro$mayor_en_sociedades), nrow(por_rubro), N_MIN_RUBRO))
print(as.data.frame(por_rubro |> select(rubro_principal, n, pct_pf, pct_soc)))

# ── Jurisdiction level: the share of suppliers the state ever named ──────────
cons <- d |> group_by(cuit, provincia) |>
  summarise(consagrado = any(cual), .groups = "drop")
anclado <- d |> group_by(provincia) |>
  summarise(anclado = mean(anclaje_territorial), .groups = "drop")
por_prov <- cons |> group_by(provincia) |>
  summarise(proveedores = n(), pct_consagrados = 100 * mean(consagrado),
            .groups = "drop") |>
  left_join(anclado, by = "provincia") |>
  mutate(across(c(pct_consagrados, anclado), ~round(.x, 3))) |>
  arrange(anclado)
write_tab(por_prov, "tab_consagracion_jurisdiccion.csv")
rho <- cor.test(por_prov$pct_consagrados, por_prov$anclado, method = "spearman")
cat(sprintf("\nrho(consagrados, demanda anclada) sobre %d jurisdicciones: %.3f, p = %.4f\n",
            nrow(por_prov), rho$estimate, rho$p.value))
print(as.data.frame(por_prov))

# Robustness: drop each jurisdiction's leading supplier, the protocol the
# manuscript already applies to the rho of 0.637.
lider <- d |> count(provincia, cuit) |> group_by(provincia) |>
  slice_max(n, n = 1, with_ties = FALSE) |> ungroup() |> select(provincia, cuit)
sin_lider <- cons |> anti_join(lider, by = c("provincia", "cuit")) |>
  group_by(provincia) |>
  summarise(pct = 100 * mean(consagrado), .groups = "drop") |>
  left_join(anclado, by = "provincia")
rho_sl <- cor.test(sin_lider$pct, sin_lider$anclado, method = "spearman")
cat(sprintf("sin el proveedor líder de cada jurisdicción: %.3f, p = %.4f\n",
            rho_sl$estimate, rho_sl$p.value))

# ── Composition counterfactual, the procedure of script 32 ───────────────────
# Does the gradient survive holding the local mix of buyer types at the
# national one? If it does, what varies is the response within each type of
# demand, not which types are present.
w_nac <- g |> count(tipo_organismo) |> mutate(w_nac = n / sum(n)) |>
  select(tipo_organismo, w_nac)
celda <- g |> group_by(provincia, tipo_organismo) |>
  summarise(n = n(), pi = mean(cual), .groups = "drop") |>
  group_by(provincia) |> mutate(w = n / sum(n)) |> ungroup() |>
  left_join(w_nac, by = "tipo_organismo")
cf <- celda |> group_by(provincia) |>
  summarise(p_obs = 100 * sum(w * pi),
            p_cf = 100 * sum(w_nac * pi) / sum(w_nac),
            tipos_presentes = n(), .groups = "drop") |>
  left_join(anclado, by = "provincia") |>
  mutate(across(c(p_obs, p_cf), ~round(.x, 2))) |> arrange(anclado)
write_tab(cf, "tab_consagracion_contrafactual.csv")
cat(sprintf("\ncontrafactual de composición: rango observado %.1f-%.1f, contrafactual %.1f-%.1f\n",
            min(cf$p_obs), max(cf$p_obs), min(cf$p_cf), max(cf$p_cf)))
cat(sprintf("rho observado %.3f | contrafactual %.3f\n",
            cor(cf$p_obs, cf$anclado, method = "spearman"),
            cor(cf$p_cf, cf$anclado, method = "spearman")))

# ── The register of justification across the three presidencies ──────────────
por_era <- g |> mutate(era = factor(era, levels = ERAS)) |>
  group_by(era) |>
  summarise(n = n(),
            pct_cualitativo = round(100 * mean(cual), 2),
            pct_aritmetico = round(100 * mean(familia == "cuantitativo"), 2),
            pct_interadmin = round(100 * mean(familia == "interadministrativo"), 2),
            .groups = "drop")
share_fund <- d |> mutate(era = factor(era, levels = ERAS)) |> group_by(era) |>
  summarise(pct_con_fundamento = round(100 * mean(familia != "competitivo"), 2),
            .groups = "drop")
por_era <- por_era |> left_join(share_fund, by = "era")
write_tab(por_era, "tab_consagracion_era.csv")
cat("\nEl registro de la justificación por presidencia:\n")
print(as.data.frame(por_era))
