# 03 — Theil decomposition of the national supplier market (canonical R port
# of 03_theil_decomposition.py; same outputs, same filenames).
# Run: Rscript 03_theil_decomposition.R   (from the project root)

source("theme_house.R", chdir = TRUE)

theil_t <- function(x) {
  x <- x[x > 0 & !is.na(x)]
  if (length(x) < 2) return(NA_real_)
  mu <- mean(x)
  mean((x / mu) * log(x / mu))
}

theil_decompose <- function(df, value, group) {
  d <- df[df[[value]] > 0 & !is.na(df[[value]]), ]
  if (nrow(d) < 2) return(c(total = NA, between = NA, within = NA))
  mu <- mean(d[[value]]); n <- nrow(d)
  total <- theil_t(d[[value]])
  parts <- d |>
    group_by(.data[[group]]) |>
    summarise(ng = n(), mug = mean(.data[[value]]),
              tg = theil_t(.data[[value]]), .groups = "drop")
  between <- sum((parts$ng / n) * (parts$mug / mu) * log(parts$mug / mu))
  within <- sum((parts$ng / n) * (parts$mug / mu) *
                  ifelse(is.na(parts$tg), 0, parts$tg))
  c(total = total, between = between, within = within)
}

shannon <- function(counts) {
  p <- counts / sum(counts)
  p <- p[p > 0]
  -sum(p * log(p))
}

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_clean.parquet")) |>
  filter(es_nuevo, !is.na(provincia))
ars <- adj |> filter(moneda == "ARS")

# ── Cut (a): yearly Theil across suppliers, between/within province ─────────
ta <- bind_rows(lapply(sort(unique(ars$ejercicio)), function(yr) {
  firm <- ars |> filter(ejercicio == yr) |>
    group_by(cuit, provincia) |>
    summarise(monto = sum(monto, na.rm = TRUE), n = n(), .groups = "drop")
  bind_rows(lapply(c("monto", "n"), function(v) {
    r <- theil_decompose(firm, v, "provincia")
    tibble(ejercicio = yr, variable = v, theil_total = r["total"],
           theil_between = r["between"], theil_within = r["within"],
           share_between = r["between"] / r["total"], n_firmas = nrow(firm))
  }))
}))
write_tab(ta, "tab_theil_provincia_anual.csv")

# ── Cut (b): rubro Shannon H per province x era ──────────────────────────────
k_rubros <- n_distinct(adj$rubro_principal, na.rm = TRUE)
tb <- adj |>
  filter(!is.na(rubro_principal)) |>
  count(provincia, era, rubro_principal) |>
  group_by(provincia, era) |>
  summarise(n_adj = sum(n), n_rubros = n(), shannon_h = shannon(n),
            .groups = "drop") |>
  mutate(shannon_norm = shannon_h / log(k_rubros), nea = provincia %in% NEA)
write_tab(tb, "tab_shannon_rubros_provincia_era.csv")

# ── Cut (c): nested Theil by era ─────────────────────────────────────────────
tc <- bind_rows(lapply(ERAS, function(e) {
  firm <- ars |> filter(era == e) |>
    group_by(cuit, provincia) |>
    summarise(monto = sum(monto, na.rm = TRUE), .groups = "drop") |>
    mutate(region = ifelse(provincia %in% NEA, "NEA", "Resto"))
  r_reg <- theil_decompose(firm, "monto", "region")
  r_prov <- theil_decompose(firm, "monto", "provincia")
  tibble(era = e, theil_total = r_reg["total"],
         between_region = r_reg["between"],
         between_provincia = r_prov["between"],
         between_prov_within_region = r_prov["between"] - r_reg["between"],
         within_provincia = r_prov["within"])
}))
write_tab(tc, "tab_theil_anidado_era.csv")

# ── Figures ──────────────────────────────────────────────────────────────────
d_a <- ta |> mutate(variable = recode(variable, monto = "Montos ARS",
                                      n = "Cantidad de adjudicaciones"))
p1 <- ggplot(d_a, aes(ejercicio)) +
  geom_col(aes(y = share_between * max(theil_total, na.rm = TRUE)),
           fill = "#1f77b4", alpha = 0.15) +
  geom_line(aes(y = theil_total, colour = "Theil total")) +
  geom_point(aes(y = theil_total, colour = "Theil total")) +
  geom_line(aes(y = theil_between, colour = "Between provincia"),
            linetype = "dashed") +
  geom_point(aes(y = theil_between, colour = "Between provincia"), shape = 15) +
  geom_vline(xintercept = c(2019.5, 2023.5), linetype = "dotted",
             colour = "grey50", linewidth = 0.4) +
  scale_colour_manual(values = c("Theil total" = "#333333",
                                 "Between provincia" = "#d62728")) +
  facet_wrap(~variable, nrow = 1) +
  labs(x = "Ejercicio", y = "Theil T",
       title = "Theil T entre proveedores, between/within provincia") +
  theme_house()
save_fig(p1, "fig_theil_temporal.png", width = 11, height = 4.2)

rest_w <- tb |> filter(!nea) |> group_by(era) |>
  summarise(shannon_norm = weighted.mean(shannon_norm, n_adj), .groups = "drop") |>
  mutate(provincia = "Resto (prom. ponderado)")
d_b <- bind_rows(tb |> filter(nea) |> select(provincia, era, shannon_norm),
                 rest_w) |>
  mutate(era = factor(era, levels = ERAS))
p2 <- ggplot(d_b, aes(era, shannon_norm, group = provincia,
                      colour = provincia)) +
  geom_line() + geom_point() +
  scale_colour_manual(values = c(Chaco = "#d62728", Corrientes = "#ff7f0e",
                                 Formosa = "#9467bd", Misiones = "#8c564b",
                                 `Resto (prom. ponderado)` = "black")) +
  labs(x = NULL, y = "Shannon H normalizado (rubros)",
       title = "Diversidad de rubros de proveedores por era, NEA vs resto") +
  theme_house()
save_fig(p2, "fig_shannon_nea.png")

cat("\nCut (a) share between (montos):\n")
print(ta |> filter(variable == "monto") |>
        transmute(ejercicio, theil_total = round(theil_total, 3),
                  share_between = round(share_between, 3)), n = 20)
cat("\nCut (c):\n")
print(tc |> mutate(across(where(is.numeric), ~round(.x, 4))))
