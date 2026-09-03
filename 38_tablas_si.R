# 38 — Supplementary tables that had no canonical output.
#
# Section S1 of the supplement describes the record linkage and promises a
# Table S1 with match rates by year. No script wrote it: the rates were
# printed by 01_build_supplier_panel.py and never persisted, so the supplement
# named a table that did not exist. This writes it from the same repaired
# identifiers the rest of the analysis uses, against the raw supplier register.
#
# The identifier repair is the rule of 01_build_supplier_panel.py::norm_cuit:
# framework-agreement rows carry twelve digits with the check digit duplicated
# inside the middle block, and removing the duplicate restores eleven. The
# awards parquet already holds the repaired identifier; only the register has
# to be normalised the same way here.
#
# Run: Rscript 38_tablas_si.R

source("theme_house.R", chdir = TRUE)

norm_cuit <- function(x) {
  d <- gsub("[^0-9]", "", as.character(x))
  rep12 <- nchar(d) == 12 & substr(d, 11, 11) == substr(d, 12, 12)
  d[rep12] <- paste0(substr(d[rep12], 1, 10), substr(d[rep12], 12, 12))
  ifelse(nchar(d) == 11, d, NA_character_)
}

# modulus-11 check digit, the algorithm the tax authority uses
mod11_ok <- function(cuit) {
  w <- c(5, 4, 3, 2, 7, 6, 5, 4, 3, 2)
  dig <- do.call(rbind, lapply(strsplit(cuit, ""), as.integer))
  s <- as.vector(dig[, 1:10, drop = FALSE] %*% w)
  r <- 11 - (s %% 11)
  r[r == 11] <- 0
  r[r == 10] <- 9
  r == dig[, 11]
}

adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_clean.parquet"))
sip_raw <- read.csv(file.path(DIR_RAW, "sipro_proveedores.csv"),
                    encoding = "UTF-8", colClasses = "character")
sip_cuits <- unique(na.omit(norm_cuit(sip_raw$CUIT_NIT)))

# the canonical population of the manuscript: new awards inside the window
# that carry a province of fiscal domicile
a <- adj |>
  filter(between(ejercicio, WIN0, WIN1)) |>
  mutate(cuit = as.character(cuit),
         en_sipro = cuit %in% sip_cuits)

stopifnot(sum(a$es_nuevo & !is.na(a$provincia)) == 163126)

s1 <- a |>
  group_by(Year = ejercicio) |>
  summarise(
    Awards = n(),
    `New awards` = sum(es_nuevo),
    Suppliers = n_distinct(cuit),
    `Suppliers in the register (%)` =
      round(100 * mean(unique(cuit) %in% sip_cuits), 1),
    `Awards matched (%)` = round(100 * mean(en_sipro), 1),
    `Awards carrying a province (%)` =
      round(100 * mean(!is.na(provincia)), 1),
    .groups = "drop")

total <- tibble(
  Year = NA_integer_, Awards = nrow(a), `New awards` = sum(a$es_nuevo),
  Suppliers = n_distinct(a$cuit),
  `Suppliers in the register (%)` =
    round(100 * mean(unique(a$cuit) %in% sip_cuits), 1),
  `Awards matched (%)` = round(100 * mean(a$en_sipro), 1),
  `Awards carrying a province (%)` =
    round(100 * mean(!is.na(a$provincia)), 1))
s1 <- bind_rows(s1, total)

write_tab(s1, "tab_linkage_por_anio.csv")
print(as.data.frame(s1), row.names = FALSE)

# the repair, quantified: how many rows it recovers and how the recovered
# identifiers behave under the check-digit algorithm, against those that were
# eleven digits to begin with
raw_adj <- read.csv(file.path(DIR_RAW, "comprar_adjudicaciones_2016_2026.csv"),
                    encoding = "UTF-8", colClasses = "character",
                    nrows = -1)
d <- gsub("[^0-9]", "", raw_adj$CUIT)
es12 <- nchar(d) == 12 & substr(d, 11, 11) == substr(d, 12, 12)
rep_ok  <- mod11_ok(na.omit(norm_cuit(raw_adj$CUIT[es12])))
nat_ok  <- mod11_ok(na.omit(norm_cuit(raw_adj$CUIT[nchar(d) == 11])))

s1b <- tibble(
  Identifier = c("Twelve digits, repaired", "Eleven digits as recorded"),
  Rows = c(sum(es12), sum(nchar(d) == 11)),
  `Passing the check digit (%)` = round(100 * c(mean(rep_ok), mean(nat_ok)), 2))
write_tab(s1b, "tab_linkage_reparacion.csv")
print(as.data.frame(s1b), row.names = FALSE)
