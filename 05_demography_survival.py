"""
05 — Supplier demography and survival across political transitions
==================================================================
Project: Who Sells to the State (2026_2)

Entry (first award), exit (last award), tenure; annual entry rates by
region; cohort persistence across presidencies (P(era-X entrant still
winning under era Y)) by province/region; discrete survival curves
(pandas, no lifelines).

Censoring: exit is right-censored at 2025+ (a supplier whose last award
is 2025-2026 may still be active); survival curves use awards observed
through Aug-2026.

Input:  data/processed/adjudicaciones_clean.parquet
Output: tables/tab_cohort_survival.csv
        tables/tab_entry_exit_region.csv
        figures/fig_survival_by_region.png

Usage:  python 05_demography_survival.py
"""
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

PROJECT = Path(__file__).parent
PROC = PROJECT / "data" / "processed"
TAB = PROJECT / "tables"
FIG = PROJECT / "figures"

NEA = {"Misiones", "Corrientes", "Chaco", "Formosa"}
METRO = {"CABA", "Buenos Aires"}
ERA_OF_YEAR = {**{y: "macri" for y in range(2016, 2020)},
               **{y: "fernandez" for y in range(2020, 2024)},
               **{y: "milei" for y in range(2024, 2027)}}

adj = pd.read_parquet(PROC / "adjudicaciones_clean.parquet")
adj = adj[adj["es_nuevo"] & adj["provincia"].notna()]

firm_years = (adj.groupby(["cuit", "provincia", "ejercicio"])
              .size().rename("n").reset_index())
firm_years["region"] = np.select(
    [firm_years["provincia"].isin(NEA), firm_years["provincia"].isin(METRO)],
    ["NEA", "Metro"], default="Resto")

life = (firm_years.groupby(["cuit", "provincia", "region"])
        .agg(entrada=("ejercicio", "min"), salida=("ejercicio", "max"),
             anios_activo=("ejercicio", "nunique"))
        .reset_index())
life["cohorte_era"] = life["entrada"].map(ERA_OF_YEAR)

# ── Entry/exit rates by region-year ──────────────────────────────────────────
active = firm_years.groupby(["region", "ejercicio"])["cuit"].nunique().rename("activos")
entries = life.groupby(["region", "entrada"])["cuit"].size().rename("entradas")
ee = pd.concat([active, entries.rename_axis(["region", "ejercicio"])], axis=1)
ee["tasa_entrada"] = ee["entradas"] / ee["activos"]
ee.reset_index().to_csv(TAB / "tab_entry_exit_region.csv", index=False)

# ── Cohort persistence across eras ───────────────────────────────────────────
active_by_era = (firm_years.assign(era=firm_years["ejercicio"].map(ERA_OF_YEAR))
                 .groupby(["cuit", "era"]).size().unstack().notna())
rows = []
for scope, sel in [("NEA", life["region"] == "NEA"),
                   ("Metro", life["region"] == "Metro"),
                   ("Resto", life["region"] == "Resto")]:
    for cohorte, target in [("macri", "fernandez"), ("macri", "milei"),
                            ("fernandez", "milei")]:
        cuits = life[sel & (life["cohorte_era"] == cohorte)]["cuit"]
        if len(cuits) == 0:
            continue
        surv = active_by_era.reindex(cuits)[target].fillna(False).mean()
        rows.append({"region": scope, "cohorte": cohorte, "sobrevive_en": target,
                     "n_cohorte": len(cuits), "tasa_supervivencia": round(surv, 3)})
cs = pd.DataFrame(rows)
# per NEA province detail, macri -> milei
for prov in sorted(NEA):
    cuits = life[(life["provincia"] == prov) & (life["cohorte_era"] == "macri")]["cuit"]
    if len(cuits):
        surv = active_by_era.reindex(cuits)["milei"].fillna(False).mean()
        cs = pd.concat([cs, pd.DataFrame([{
            "region": prov, "cohorte": "macri", "sobrevive_en": "milei",
            "n_cohorte": len(cuits), "tasa_supervivencia": round(surv, 3)}])])
cs.to_csv(TAB / "tab_cohort_survival.csv", index=False)

# ── Discrete survival curves (age = years since entry, entrants <= 2021) ─────
fig, ax = plt.subplots(figsize=(8, 4.5))
for reg, color in [("NEA", "#d62728"), ("Metro", "#555555"), ("Resto", "#1f77b4")]:
    sub = life[(life["region"] == reg) & (life["entrada"] <= 2021)]
    ages = np.arange(0, 6)
    surv = []
    for k in ages:
        at_risk = sub[sub["entrada"] + k <= 2026]
        alive = (at_risk["salida"] >= at_risk["entrada"] + k).mean()
        surv.append(alive)
    ax.plot(ages, surv, "o-", color=color, label=f"{reg} (n={len(sub)})")
ax.set_xlabel("Años desde la primera adjudicación")
ax.set_ylabel("P(sigue adjudicando en el año k o después)")
ax.set_title("Supervivencia de proveedores en el mercado nacional (entrantes 2016-2021)")
ax.legend()
fig.tight_layout()
fig.savefig(FIG / "fig_survival_by_region.png", dpi=200)
plt.close(fig)

print("Persistencia de cohortes entre presidencias:")
print(cs.to_string(index=False))
print("\nEntradas y activos por región (últimos años):")
print(ee.reset_index().query("ejercicio >= 2022").round(3).to_string(index=False))
print("OK -> tables/, figures/")
