# Classified everywhere, named in few places

Replication material for a study of the social space of the suppliers of the
Argentine national state, built from the award records of the COMPR.AR
procurement platform, 2019–2025: 163,126 awards to 10,602 suppliers from 152
buying organisms.

The analysis maps supplier positions by specific multiple correspondence
analysis on six coordinates the state itself records — legal form, modal
sector, modal type of client, scale of the exchange, procedure profile and
registration cohort — and compares sub-national configurations of that space.

## What is here, and what is not

| Included | |
|---|---|
| `*.R`, `*.py` | every script of the pipeline, 58 files |
| `tables/` | the 150 canonical outputs every number in the paper is quoted from |
| `figures/` | the twelve submission figures as PNG (TIFF and EPS on request) |
| `data/raw/` | the public inputs that are small and carry no personal data |
| `data/raw/covariates/` | the hand-coded covariates, with `SOURCES.md` |
| `data/anonymised/` | the two analysis matrices, de-identified: every award and every supplier the paper uses |
| `validation/` | the frozen first-pass Python tables (40 CSVs) and the R/Python cross-validation report |

The folder is assembled by `build_replication_folder.py`, which lives with
the project sources and refuses to write a file carrying a taxpayer
identifier.

## The de-identified release

`data/anonymised/` carries the analysis as data, with a surrogate key in place
of the taxpayer identifier and no name in any column:

- `awards.csv.gz` — the 163,126 awards of the window to 10,602 suppliers:
  year, presidency, buying organism and its functional type, whether the organ
  is territorially anchored, procedure, sector, nominal amount and currency,
  the supplier's province of fiscal domicile and legal form, and the legal
  ground of the exemption where the award was made outside open competition.
- `supplier_space.csv` — the 10,580 suppliers recorded on all six active
  variables, with the supplementary battery, the province and the stratum.

The two join on `supplier_id`, and between them they reproduce the paper's
chain: 163,126 awards, 10,602 suppliers, 10,580 in the geometry. Amounts are
nominal; `02_denominators_covariates.py` writes the price index and the
exchange rate that turn them into constant pesos and dollars.

This is de-identified, not anonymous in the strict sense. The sources are
public, so a reader holding them can re-link the rows. The surrogate key is a
random permutation under a fixed seed, so the files leak no order of their own;
that is all it is. `46_release_anonimizada.R` builds them and asserts the
counts and the absence of identifying columns before writing.

**Not included: the supplier microdata.** The COMPR.AR award file, the SIPRO
supplier register and the taxpayer register carry taxpayer identifiers and
names, and roughly two in five suppliers in this population are natural
persons. They are public downloads, so `data/README.md` says where each one
comes from and `00_download_snapshots.py` fetches them; they are not
redistributed here. The derived parquets in `data/processed/` are rebuilt from
them by the data layer and are excluded for the same reason.

No output in `tables/` or `figures/` carries a taxpayer identifier or a
supplier name; the build of this folder refuses to write one.

The manuscript and its supporting information are not included: the paper is
under review.

## Software

Analysis and every published figure run in R; the data layer runs in Python.
The division is stated in the paper's methods.

R 4.5.1 with FactoMineR 2.14, GDAtools 2.3, ggplot2 4.0.3, dplyr 1.1.4,
tidyr 1.3.2, arrow 23.0.1.2, sf 1.0.24, survival 3.8.3, patchwork 1.3.2,
ggrepel 0.9.8, readxl 1.4.5, broom, cluster, stringi.

Python 3.14 with the packages pinned in `requirements.txt`.

## Running it

Every script resolves its paths relative to this directory, so run them from
here and do not move them into subfolders. Each carries a header saying what it
does; the R scripts also state their run time.

**Data layer, Python.** `00_download_snapshots.py` fetches the public sources.
Then `01_build_supplier_panel.py` links awards to the supplier register by
taxpayer identifier and repairs a duplicated check digit in the rows that
originate in framework agreements; `02_denominators_covariates.py` builds the
company denominators, the consumer price index and the exchange-rate series;
`09_buyer_typology.py` classifies the buying organisms; `33_uoc_gazetteer.py`
places the 539 purchasing units at their seats; `34_flujos_intercambio.py`
joins each award's unit seat to its supplier's locality; `43_apartado.py`
extracts the legal ground of each award made outside open competition.

**Analysis layer, R.** `theme_house.R` holds the analysis window, the palette,
the category labels and the shared space builder, and every script sources it.
`30_espacio_c10.R` is the canonical diagnostic of the published space;
`13_publication_figures.R`, `17`, `18`, `22`, `28`, `35` and `36` write the
figures; `38_tablas_si.R` writes the supplementary tables; `99_crossval.R`
compares the R results against the frozen Python pass in `validation/`.

`build_si_tables.py` writes the curated supplementary tables into the
manuscript's markdown, which is not distributed here; it is included because it
documents how each supplementary table is drawn from `tables/`. The script that
assembles the submission documents is not in this repository.

## Guards

Three assertions are built into the chain and will stop it rather than let a
number drift silently:

- `28_geometria_robustez.R` asserts the published modified rates, 43.6 and 33.0
  per cent, before doing anything else.
- `37_robustez_final.R` asserts the rank correlation of 0.637 between anchored
  demand and the share of suppliers that are natural persons, after reading the
  gradient table. It exists because that table was once written rounded to
  three decimals and four scripts recomputed statistics on the rounded values.
- `theme_house.R::read_tab()` routes every cross-script table read, so a
  replication run at a different threshold cannot silently read the canonical
  run's output.

## Anonymity

The population includes natural persons. Outputs report ranks and proportions
only. Where a single supplier holds a large share of the awards a jurisdiction
receives, it is described by its position and never named, in the paper, in the
tables and in the code comments.

## Licence

Code under the MIT licence (`LICENSE`). Data, tables, figures and documentation
under CC BY 4.0 (`LICENSE-DATA.md`). The underlying administrative records
belong to their publishers, named in `data/README.md`.
