# 47 — What the class-specific axes carry (round 34, 4 Sep 2026)
#
# Table S12 reports the class-specific MCA by stratum: the core's first
# opposition reappears as the SECOND axis of the middle and outer subclouds
# (0.54, 0.65), first against first is 0.13 and 0.08, and the core's second
# axis, the axis of access, is barely recovered (0.21, 0.15). Read at face
# value that is a structural difference, and Table S34 says at the same time
# that the separately fitted provinces keep the national rank. The body owed
# the reconciliation and, by the rule of 30 Aug, it has to be measured before
# it is written.
#
# Two measurements. (a) Which national opposition each class-specific axis
# carries: the correlation, over the INDIVIDUALS of the subcloud, between
# their class-specific coordinates and their coordinates in the national
# space. Script 11 matched category coordinates across two class-specific
# solutions, which says whether the axes look alike, not what they carry.
# (b) Whether the peripheral subclouds are compressed on the axis of access:
# the standard deviation of each subcloud on the national axes 1 and 2, the
# ratio to the core's, and the share of the subcloud's variance in the
# national plane that lies on axis 2. A subcloud gathered at one pole of an
# axis has little variance left on it, and a class-specific analysis, which
# seeks the principal directions of the subcloud, will rank that opposition
# low: composition, not structure. Whether that is the case here is what
# the tables decide.
#
# Also emits the size of every supplementary category (Table S10b): the
# share of single-award suppliers quoted in the methods lived only in the
# de-identified release until now.
#
# Same construction as 11_mca_v2.R and 28_geometria_robustez.R; asserts the
# published Benzécri rates before doing anything.
#
# Run: Rscript 47_csmca_reconciliacion.R

source("theme_house.R", chdir = TRUE)
suppressMessages(library(GDAtools))

master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
m <- build_supplier_space(adj, master)

X <- active_matrix(m)
RARE <- rare_categories(X)
mca <- speMCA(X, excl = excl_index(X, RARE), ncp = 5)
mr <- modif.rate(mca)$modif$mrate
stopifnot(abs(mr[1] - 43.6) < 0.15, abs(mr[2] - 33.0) < 0.15)  # C10 canon intact

# five national axes, so that a peripheral axis that is a minor national
# opposition promoted to the front is recognised as such and not read as a
# direction the national space does not contain
nat <- as.data.frame(mca$ind$coord)[, 1:5]
names(nat) <- paste0("nac", 1:5)
stopifnot(nrow(nat) == nrow(m))

# ── (a) what each class-specific axis carries, on the individuals ───────────
ejes <- list(); disp <- list(); contrib <- list()
for (s in c("core", "middle", "outer")) {
  sel <- m$estrato == s
  res <- csMCA(X, subcloud = sel, excl = excl_index(X, RARE), ncp = 3)
  cs <- as.data.frame(res$ind$coord)[, 1:3]
  stopifnot(nrow(cs) == sum(sel))
  mrs <- modif.rate(res)$modif$mrate
  sub <- nat[sel, ]
  for (k in 1:3) {
    r <- sapply(1:5, function(j) cor(cs[, k], sub[, j]))
    ejes[[length(ejes) + 1]] <- tibble(
      estrato = s, eje_cs = k, tasa_modif_cs = round(mrs[k], 1),
      r_nac1 = round(r[1], 3), r_nac2 = round(r[2], 3), r_nac3 = round(r[3], 3),
      r_nac4 = round(r[4], 3), r_nac5 = round(r[5], 3),
      eje_nacional_que_carga = which.max(abs(r)), r_max = round(max(abs(r)), 3))
  }
  # the categories that build each class-specific axis: what the direction is
  ct <- as.data.frame(res$var$contrib)[, 1:2]
  cc <- as.data.frame(res$var$coord)[, 1:2]
  for (k in 1:2) {
    o <- order(-ct[[k]])[1:8]
    contrib[[length(contrib) + 1]] <- tibble(
      estrato = s, eje_cs = k, categoria = rownames(res$var$contrib)[o],
      contrib = round(ct[[k]][o], 1), coord = round(cc[[k]][o], 2))
  }
  # ── (b) dispersion of the subcloud on the national axes ──────────────────
  v1 <- var(sub$nac1); v2 <- var(sub$nac2)
  disp[[length(disp) + 1]] <- tibble(
    estrato = s, n = sum(sel),
    media_nac2 = round(mean(sub$nac2), 3),
    de_nac1 = round(sd(sub$nac1), 3), de_nac2 = round(sd(sub$nac2), 3),
    share_var_eje2 = round(v2 / (v1 + v2), 3))
}
ejes <- bind_rows(ejes)
disp <- bind_rows(disp)
core_de <- disp |> filter(estrato == "core")
disp <- disp |>
  mutate(cociente_de1_core = round(de_nac1 / core_de$de_nac1, 3),
         cociente_de2_core = round(de_nac2 / core_de$de_nac2, 3))
contrib <- bind_rows(contrib)
write_tab(ejes, "tab_csmca_ejes_individuos.csv")
write_tab(disp, "tab_csmca_dispersion.csv")
write_tab(contrib, "tab_csmca_contribuciones.csv")

# ── (d) the same, passivating on the subcloud's own threshold ───────────────
# The first run showed the peripheral class-specific first axes built by ONE
# category each (infrastructure client, 52-55% of the axis; rental supply,
# 46-49% of the second): categories rare inside the subcloud but passivated
# only on the national count. That is the artefact the 5% rule exists to
# prevent, and Table S11b already applies the own-count rule to the
# per-province refits. Here it is applied to the class-specific solutions.
ejes_p <- list(); contrib_p <- list(); raras <- list()
for (s in c("core", "middle", "outer")) {
  sel <- m$estrato == s
  own <- rare_categories(X[sel, , drop = FALSE])
  fr <- unlist(lapply(names(X), function(v) {
    setNames(as.numeric(table(X[[v]][sel])) / sum(sel),
             paste0(v, ".", levels(X[[v]])))
  }))
  raras[[length(raras) + 1]] <- tibble(
    estrato = s, categoria = names(fr), share_subnube = round(100 * fr, 1),
    pasivada_nacional = names(fr) %in% RARE,
    pasivada_propia = names(fr) %in% own) |>
    filter(pasivada_nacional | pasivada_propia)
  res <- csMCA(X, subcloud = sel, excl = excl_index(X, union(RARE, own)), ncp = 3)
  cs <- as.data.frame(res$ind$coord)[, 1:3]
  mrs <- modif.rate(res)$modif$mrate
  sub <- nat[sel, ]
  for (k in 1:3) {
    r <- sapply(1:5, function(j) cor(cs[, k], sub[, j]))
    ejes_p[[length(ejes_p) + 1]] <- tibble(
      estrato = s, eje_cs = k, tasa_modif_cs = round(mrs[k], 1),
      r_nac1 = round(r[1], 3), r_nac2 = round(r[2], 3), r_nac3 = round(r[3], 3),
      r_nac4 = round(r[4], 3), r_nac5 = round(r[5], 3),
      eje_nacional_que_carga = which.max(abs(r)), r_max = round(max(abs(r)), 3))
  }
  ct <- as.data.frame(res$var$contrib)[, 1:2]
  cc <- as.data.frame(res$var$coord)[, 1:2]
  for (k in 1:2) {
    o <- order(-ct[[k]])[1:8]
    contrib_p[[length(contrib_p) + 1]] <- tibble(
      estrato = s, eje_cs = k, categoria = rownames(res$var$contrib)[o],
      contrib = round(ct[[k]][o], 1), coord = round(cc[[k]][o], 2))
  }
}
ejes_p <- bind_rows(ejes_p); contrib_p <- bind_rows(contrib_p)
raras <- bind_rows(raras)
write_tab(ejes_p, "tab_csmca_ejes_individuos_propio.csv")
write_tab(contrib_p, "tab_csmca_contribuciones_propio.csv")
write_tab(raras, "tab_csmca_raras_subnube.csv")
# ── (e) what the infrastructure client is, stratum by stratum ───────────────
# The peripheral class-specific first axis is built by the infrastructure
# client (52-55% of the axis). Whether that client is the road district, an
# anchored organ, or a programmatic works agency decides how the axis reads.
adj_win <- adj |> filter(es_nuevo, !is.na(provincia),
                         between(ejercicio, WIN0, WIN1))
infra <- adj_win |>
  inner_join(m |> select(cuit, estrato), by = "cuit") |>
  filter(tipo_organismo == "infraestructura") |>
  group_by(estrato) |>
  summarise(n_adj = n(),
            share_anclado = round(100 * mean(anclaje_territorial), 1),
            .groups = "drop")
write_tab(infra, "tab_csmca_infra_anclado.csv")
cat("\nAdjudicaciones de clientes de infraestructura: parte anclada por estrato:\n")
print(as.data.frame(infra))

cat("\nCategorías pasivadas por subnube (nacional / propia):\n")
print(as.data.frame(raras))
cat("\nEjes class-specific con pasivación propia:\n")
print(as.data.frame(ejes_p))
cat("\nContribuciones con pasivación propia:\n")
print(as.data.frame(contrib_p))

# ── (c) size of the supplementary categories (Table S10b) ───────────────────
sup_n <- bind_rows(
  m |> count(categoria = S_intensidad) |> mutate(variable = "intensidad"),
  m |> count(categoria = S_tenure)     |> mutate(variable = "tenure"),
  m |> count(categoria = S_cohorte)    |> mutate(variable = "cohorte_arca"),
  m |> count(categoria = S_fundamento) |> mutate(variable = "fundamento"),
  m |> count(categoria = estrato)      |> mutate(variable = "estrato")) |>
  mutate(categoria = as.character(categoria),
         share = round(100 * n / nrow(m), 1)) |>
  select(variable, categoria, n, share)
write_tab(sup_n, "tab_c10_supvars_n.csv")

cat("\nEjes class-specific contra los ejes nacionales (individuos):\n")
print(as.data.frame(ejes))
cat("\nDispersión de las subnubes sobre los ejes nacionales:\n")
print(as.data.frame(disp))
cat("\nCategorías que construyen los ejes class-specific 1 y 2:\n")
print(as.data.frame(contrib))
cat("\nTamaño de las categorías suplementarias:\n")
print(as.data.frame(sup_n))
