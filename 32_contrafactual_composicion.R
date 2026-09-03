# 32 — Demand-composition counterfactual (V26, author's decision 29 Aug 2026).
#
# The claim "the state varies the composition of the positions it offers"
# is a description until the composition of demand is actually held fixed.
# For each jurisdiction j and functional buyer type t:
#   pi_tj = share of type-t awards in j going to natural persons,
#   w_tj  = share of j's awards issued by type t,
#   observed  p_j  = sum_t w_tj  * pi_tj
#   counterf. p*_j = sum_t w_t,nac * pi_tj   (national type mix, local response)
# If p* flattens the CABA-Formosa range, local demand composition carries the
# gradient of personal supply; what remains is the within-type difference.
# Unit note: award shares, not supplier shares — Table 2 counts suppliers, so
# levels differ from its natural-person column by construction.
#
# Run: Rscript 32_contrafactual_composicion.R

source("theme_house.R", chdir = TRUE)

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia), between(ejercicio, WIN0, WIN1),
         !is.na(personeria)) |>
  mutate(pf = personeria == "Persona Fisica")

w_nac <- adj |> count(tipo_organismo) |> mutate(w_nac = n / sum(n)) |>
  select(tipo_organismo, w_nac)

celda <- adj |> group_by(provincia, tipo_organismo) |>
  summarise(n = n(), pi = mean(pf), .groups = "drop") |>
  group_by(provincia) |> mutate(w = n / sum(n)) |> ungroup() |>
  left_join(w_nac, by = "tipo_organismo")

# national weights renormalised over the types present in each jurisdiction,
# so a jurisdiction never receives weight on a type it has no awards from
cf <- celda |> group_by(provincia) |>
  summarise(p_obs = sum(w * pi),
            p_cf = sum(w_nac * pi) / sum(w_nac),
            tipos_presentes = n(), .groups = "drop") |>
  mutate(across(c(p_obs, p_cf), ~round(.x, 3)))

anclado <- adj |> group_by(provincia) |>
  summarise(anclado = mean(anclaje_territorial), .groups = "drop")
cf <- cf |> left_join(anclado, by = "provincia") |>
  mutate(anclado = round(anclado, 3)) |> arrange(desc(anclado))
write_tab(cf, "tab_contrafactual_composicion.csv")

r_obs <- diff(range(cf$p_obs)); r_cf <- diff(range(cf$p_cf))
sd_obs <- sd(cf$p_obs); sd_cf <- sd(cf$p_cf)
rho_obs <- cor(cf$anclado, cf$p_obs, method = "spearman")
rho_cf <- cor(cf$anclado, cf$p_cf, method = "spearman")
caba <- cf |> filter(provincia == "CABA")
formosa <- cf |> filter(provincia == "Formosa")
gap_obs <- formosa$p_obs - caba$p_obs
gap_cf <- formosa$p_cf - caba$p_cf

resumen <- tibble(
  medida = c("rango observado", "rango contrafactual",
             "desvio observado", "desvio contrafactual",
             "gap Formosa-CABA observado", "gap Formosa-CABA contrafactual",
             "gap cerrado por composicion (%)",
             "rho con anclado, observado", "rho con anclado, contrafactual"),
  valor = round(c(r_obs, r_cf, sd_obs, sd_cf, gap_obs, gap_cf,
                  100 * (1 - gap_cf / gap_obs), rho_obs, rho_cf), 3))
write_tab(resumen, "tab_contrafactual_resumen.csv")

# robustness: the same counterfactual at the anchored/non-anchored binary and
# at type x anchoring, plus the local response rates that carry the result
adj2 <- adj |> mutate(tipo2 = ifelse(anclaje_territorial, "anclado",
                                     "no_anclado"),
                      tipo12 = paste(tipo_organismo, tipo2))
run_cf <- function(d, tipovar) {
  wn <- d |> count(.data[[tipovar]]) |> mutate(w_nac = n / sum(n))
  d |> group_by(provincia, .data[[tipovar]]) |>
    summarise(n = n(), pi = mean(pf), .groups = "drop") |>
    group_by(provincia) |> mutate(w = n / sum(n)) |> ungroup() |>
    left_join(wn |> select(-n), by = tipovar) |>
    group_by(provincia) |>
    summarise(p_obs = sum(w * pi), p_cf = sum(w_nac * pi) / sum(w_nac),
              .groups = "drop")
}
rob <- bind_rows(lapply(c("tipo2", "tipo12"), function(tv) {
  x <- run_cf(adj2, tv)
  tibble(resolucion = tv,
         gap_obs = x$p_obs[x$provincia == "Formosa"] -
           x$p_obs[x$provincia == "CABA"],
         gap_cf = x$p_cf[x$provincia == "Formosa"] -
           x$p_cf[x$provincia == "CABA"]) |>
    mutate(cerrado_pct = round(100 * (1 - gap_cf / gap_obs), 1),
           across(c(gap_obs, gap_cf), ~round(.x, 3)))
}))
write_tab(rob, "tab_contrafactual_robustez.csv")

pi_anclaje <- adj2 |> group_by(provincia, tipo2) |>
  summarise(n = n(), pi = round(mean(pf), 3), .groups = "drop") |>
  tidyr::pivot_wider(names_from = tipo2, values_from = c(n, pi)) |>
  left_join(anclado, by = "provincia") |>
  mutate(anclado = round(anclado, 3)) |> arrange(desc(anclado))
write_tab(pi_anclaje, "tab_contrafactual_pi_anclaje.csv")

cat("\nContrafactual de composicion (share de adjudicaciones a personas fisicas):\n")
print(as.data.frame(resumen))
cat("\nExtremos:\n")
print(as.data.frame(cf |> filter(provincia %in%
  c("CABA", "Buenos Aires", "Chaco", "Formosa", "Misiones", "Corrientes"))))
