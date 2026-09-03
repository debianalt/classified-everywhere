# 42 — Does registration carry the gradient? (round 27, 1 Sep 2026)
#
# Section 6 claims that being readable to the state is the one step of the
# sequence that carries no territorial gradient, and it illustrated the claim
# with a single pair: Misiones registers with the national system slightly
# above the federal capital. That pair is the only favourable one of the four
# north-eastern provinces — Chaco registers at less than half CABA's rate, and
# Corrientes and Formosa below it too — so the illustration was a selection
# rather than the evidence, and the rates themselves appeared in no
# supplementary table.
#
# This states the claim over all 24 jurisdictions instead: the rank
# correlation between anchored demand and the rate at which a jurisdiction's
# companies appear in the national supplier register. Two rates are reported,
# because the numerator can be read two ways and they disagree — every
# registrant (natural persons included) over the company stock, and registered
# companies only over the same stock. The denominator is the company stock in
# both, since the tax register carries no natural persons.
#
# Reads the canonical tables and recomputes no analysis of its own.
#
# Run: Rscript 42_legibilidad_gradiente.R

source("theme_house.R", chdir = TRUE)

leg <- read.csv(file.path(DIR_TAB, "tab_legibilidad_exit.csv"),
                encoding = "UTF-8")
grad <- read.csv(file.path(DIR_TAB, "tab_gradiente_provincias.csv"),
                 encoding = "UTF-8")

d <- grad |>
  select(provincia, grupo, share_anclado, share_persona_fisica) |>
  inner_join(leg |> select(provincia, sipro_total, sipro_sociedades,
                           stock_sociedades, legibilidad_total_x1000,
                           legibilidad_soc_x1000),
             by = "provincia")

stopifnot(nrow(d) == 24)
# the published pair, so a change in the underlying table fails here rather
# than silently moving a number the manuscript quotes
stopifnot(abs(d$legibilidad_soc_x1000[d$provincia == "Misiones"] - 33.4) < 0.1,
          abs(d$legibilidad_soc_x1000[d$provincia == "CABA"] - 32.5) < 0.1)

tab <- d |>
  transmute(provincia, grupo,
            share_anclado = round(share_anclado, 4),
            registrados = sipro_total,
            registrados_sociedades = sipro_sociedades,
            stock_sociedades,
            legibilidad_total_x1000 = round(legibilidad_total_x1000, 1),
            legibilidad_soc_x1000 = round(legibilidad_soc_x1000, 1)) |>
  arrange(desc(share_anclado))
write_tab(tab, "tab_legibilidad_gradiente.csv")

sp <- function(x, y) {
  h <- suppressWarnings(cor.test(x, y, method = "spearman"))
  c(rho = unname(h$estimate), p = unname(h$p.value))
}

pruebas <- rbind(
  data.frame(prueba = "Anchored demand against registrants per 1,000 companies",
             t(sp(d$share_anclado, d$legibilidad_total_x1000))),
  data.frame(prueba = "Anchored demand against registered companies per 1,000",
             t(sp(d$share_anclado, d$legibilidad_soc_x1000))),
  data.frame(prueba = "Natural-person share against registered companies per 1,000",
             t(sp(d$share_persona_fisica, d$legibilidad_soc_x1000))))
pruebas$rho <- round(pruebas$rho, 3)
pruebas$p <- round(pruebas$p, 3)
write_tab(pruebas, "tab_legibilidad_cor.csv")

cat("\nLegibilidad por jurisdiccion (orden de demanda anclada):\n")
print(as.data.frame(tab), row.names = FALSE)
cat("\nCorrelaciones:\n")
print(pruebas, row.names = FALSE)
