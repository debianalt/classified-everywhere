# 45 — The differential return of the legal form.
#
# §2 calls juridical form and seniority in the register an organisational
# capital. The warrant cannot be that they differentiate positions: every
# active variable of a geometric analysis does that by construction, so the
# criterion would make any coordinate a capital and empty the word.
#
# A return has to be measured outside the construction of the space. This
# script compares the median deflated award of companies against sole
# proprietors WITHIN the same sector and the same territorial stratum, so
# neither the sectoral composition of supply nor the territorial gradient can
# produce the difference. Cells need at least fifteen suppliers of each form.
#
# Run: Rscript 45_retorno_forma.R

source("theme_house.R", chdir = TRUE)

N_MIN_CELDA <- 15

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
m <- build_supplier_space(adj, master)

celdas <- m |>
  filter(!is.na(med_real), med_real > 0) |>
  mutate(soc = A_personeria != "PersFisica") |>
  group_by(A_rubro, estrato) |>
  filter(sum(soc) >= N_MIN_CELDA, sum(!soc) >= N_MIN_CELDA) |>
  summarise(n_soc = sum(soc), n_pf = sum(!soc),
            med_soc = median(med_real[soc]),
            med_pf = median(med_real[!soc]),
            .groups = "drop") |>
  mutate(cociente = med_soc / med_pf) |>
  arrange(desc(n_soc + n_pf))

write_tab(celdas |> mutate(across(c(med_soc, med_pf), round),
                           cociente = round(cociente, 2)),
          "tab_retorno_forma.csv")

por_estrato <- celdas |> group_by(estrato) |>
  summarise(celdas = n(), a_favor_de_la_sociedad = sum(cociente > 1),
            cociente_mediano = round(median(cociente), 2), .groups = "drop")
write_tab(por_estrato, "tab_retorno_forma_estrato.csv")

cat(sprintf("celdas sector x estrato con >= %d de cada forma: %d\n",
            N_MIN_CELDA, nrow(celdas)))
cat(sprintf("la sociedad transa por encima de la persona fisica en %d de %d\n",
            sum(celdas$cociente > 1), nrow(celdas)))
cat(sprintf("cociente mediano %.2f, rango %.2f - %.2f\n",
            median(celdas$cociente), min(celdas$cociente),
            max(celdas$cociente)))
print(as.data.frame(por_estrato))
