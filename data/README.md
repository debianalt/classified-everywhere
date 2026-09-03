# Data

## What is in `raw/`

These inputs are public, small and carry no personal data, so they travel with
the code.

| File | Source | Used for |
|---|---|---|
| `georef_provincias.geojson` | infra.datos.gob.ar, georef distribution | the base map of the figures |
| `georef_localidades.json` | infra.datos.gob.ar, georef distribution | locality coordinates for the gazetteer and the flows |
| `uoc_gazetteer.csv` | own construction, script 33 | the seat of each of the 539 purchasing units |
| `uoc_sedes_deepseek.json`, `uoc_sedes_nominatim.json` | archived caches of the two lookups script 33 makes | so the gazetteer rebuilds without calling either service |
| `covariates/uoc_sedes_manual.csv` | hand verification, each row citing its source | the corrections applied on top of the automated lookup |
| `tipo_cambio_bna_anual.csv` | national series API, series 168.1_T_CAMBIOR_D_0_0_26 | annual average official exchange rate, for the dollar figures |
| `covid_emergencia_cgonc9y10.csv` | Jefatura de Gabinete de Ministros | the 450 emergency procedures that ran outside the ordinary regime in 2020 |
| `dnap_ocupacion_salarios_1987_2024.xlsx` | Dirección Nacional de Análisis Presupuestario | public employment by province, a control |
| `covariates/` | hand-coded, see `covariates/SOURCES.md` | buyer-typology overrides, provincial regimes, transparency indices |

## `anonymised/`

The two analysis matrices, with a surrogate key in place of the taxpayer
identifier and no name in any column. They are what makes this repository
runnable without the downloads below: `awards.csv.gz` holds the 163,126 awards
of the window and `supplier_space.csv` the 10,580 suppliers of the geometric
analysis, joining on `supplier_id`. Built by `46_release_anonimizada.R`.

## What has to be downloaded

Four sources carry taxpayer identifiers and supplier names and are not
redistributed. `00_download_snapshots.py` fetches all of them into `raw/`.
The snapshots behind the published results were taken on 27 August 2026; the
publishers overwrite these files, so a later download will not reproduce the
counts exactly.

| File | Where it comes from |
|---|---|
| `comprar_adjudicaciones_2016_2026.csv` | COMPR.AR awards, datos.gob.ar dataset 4, distribution 4.22 |
| `comprar_convocatorias_2016_2026.csv` | COMPR.AR calls, distribution 4.21 |
| `sipro_proveedores.csv` | Sistema de Información de Proveedores, distribution 4.23 |
| `arca_padron_denominacion.zip` | ARCA, *apellido, nombre y denominación* file |

Two extracts from CONTRAT.AR, the separate national platform for public works,
are read by script 07 and are also not redistributed, for the same reason: they
carry `contratista_cuit` and `contratista_razon_social`. They are published by
the Oficina Nacional de Contrataciones on datos.gob.ar. Nothing reported in the
paper depends on them.

## `processed/`

The parquets of the data layer are written here by scripts 00 to 02, 09, 33, 34
and 43. They are derived from the supplier microdata and are not distributed.

## A note on the exchange rate

Each peso amount is converted on its **nominal** value at the average official
rate of the year of its own award, not at a single rate for the window. Annual
inflation ran in three figures over part of the period, so one rate applied
across years would say nothing about what an award of 2019 was worth. A dollar
figure pooled across years is therefore in current dollars of mixed years, and
is a different quantity from the constant-peso series used for the inequality
decompositions.
