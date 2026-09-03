# 06 — Procedure mix: discretionality index (canonical R port of
# 06_procedure_mix.py; same outputs).
# Run: Rscript 06_procedure_mix.R

source("theme_house.R", chdir = TRUE)

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_clean.parquet")) |>
  filter(es_nuevo, !is.na(provincia)) |>
  mutate(region = region_of(provincia),
         directa = procedimiento == "Contratación Directa",
         abierta = procedimiento %in% c("Licitacion Pública", "Subasta Pública",
                                        "Concurso Público"))

t1 <- adj |>
  group_by(region, era) |>
  summarise(n = n(), share_directa_n = mean(directa),
            share_abierta_n = mean(abierta), .groups = "drop") |>
  left_join(
    adj |> filter(moneda == "ARS") |> group_by(region, era) |>
      summarise(share_directa_monto =
                  sum(monto[directa], na.rm = TRUE) / sum(monto, na.rm = TRUE),
                .groups = "drop"),
    by = c("region", "era"))
write_tab(t1, "tab_procedimientos_region_era.csv")

t2 <- adj |>
  group_by(provincia, era) |>
  summarise(n = n(), share_directa = mean(directa), .groups = "drop") |>
  mutate(nea = provincia %in% NEA)
write_tab(t2, "tab_directa_provincia.csv")

d <- t1 |>
  pivot_longer(c(share_directa_n, share_directa_monto),
               names_to = "metrica", values_to = "share") |>
  mutate(metrica = recode(metrica,
                          share_directa_n = "por cantidad",
                          share_directa_monto = "por monto ARS"),
         era = factor(era, levels = ERAS),
         region = factor(region, levels = REGION_LEVELS))
p <- ggplot(d, aes(era, share, group = region, colour = region)) +
  geom_line() + geom_point() +
  scale_colour_manual(values = REGION_COLS) +
  facet_wrap(~metrica) +
  expand_limits(y = 0) +
  labs(x = NULL, y = "Share de contratación directa",
       title = "Contratación directa por región y era") +
  theme_house()
save_fig(p, "fig_directa_nea.png", width = 11, height = 4.2)

cat("\nProcedimientos por región y era:\n")
print(t1 |> mutate(across(where(is.numeric), ~round(.x, 3))))
cat("\nShare directa NEA:\n")
print(t2 |> filter(nea) |> select(provincia, era, share_directa) |>
        pivot_wider(names_from = era, values_from = share_directa) |>
        mutate(across(where(is.numeric), ~round(.x, 3))))
