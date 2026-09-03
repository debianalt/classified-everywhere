"""
06 — Procedure mix: a discretionality index
===========================================
Project: Who Sells to the State (2026_2)

Share of Contratación Directa (count and ARS amount) by era, by supplier
province/region, and by top buying organisms. Weberian discretionality
index, free in the CSV.

Input:  data/processed/adjudicaciones_clean.parquet
Output: tables/tab_procedimientos_region_era.csv
        tables/tab_directa_provincia.csv
        figures/fig_directa_nea.png

Usage:  python 06_procedure_mix.py
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
ERAS = ["macri", "fernandez", "milei"]

adj = pd.read_parquet(PROC / "adjudicaciones_clean.parquet")
adj = adj[adj["es_nuevo"] & adj["provincia"].notna()].copy()
adj["region"] = np.select([adj["provincia"].isin(NEA), adj["provincia"].isin(METRO)],
                          ["NEA", "Metro"], default="Resto")
adj["directa"] = adj["procedimiento"] == "Contratación Directa"
adj["abierta"] = adj["procedimiento"].isin(["Licitacion Pública", "Subasta Pública",
                                            "Concurso Público"])
ars = adj[adj["moneda"] == "ARS"]

t1 = (adj.groupby(["region", "era"])
      .agg(n=("directa", "size"), share_directa_n=("directa", "mean"),
           share_abierta_n=("abierta", "mean"))
      .join(ars.groupby(["region", "era"])
            .apply(lambda g: g.loc[g["directa"], "monto"].sum() / g["monto"].sum(),
                   include_groups=False).rename("share_directa_monto"))
      .reset_index())
t1.to_csv(TAB / "tab_procedimientos_region_era.csv", index=False)

t2 = (adj.groupby(["provincia", "era"])
      .agg(n=("directa", "size"), share_directa=("directa", "mean"))
      .reset_index())
t2["nea"] = t2["provincia"].isin(NEA)
t2.to_csv(TAB / "tab_directa_provincia.csv", index=False)

fig, axes = plt.subplots(1, 2, figsize=(11, 4.2), sharey=True)
for ax, metric, lab in [(axes[0], "share_directa_n", "por cantidad"),
                        (axes[1], "share_directa_monto", "por monto ARS")]:
    for reg, color in [("NEA", "#d62728"), ("Metro", "#555555"), ("Resto", "#1f77b4")]:
        d = t1[t1["region"] == reg].set_index("era").reindex(ERAS)
        ax.plot(ERAS, d[metric], "o-", color=color, label=reg)
    ax.set_title(f"Share de contratación directa ({lab})")
    ax.set_ylim(0, None)
    ax.legend(fontsize=8)
fig.tight_layout()
fig.savefig(FIG / "fig_directa_nea.png", dpi=200)
plt.close(fig)

print("Procedimientos por región y era:")
print(t1.round(3).to_string(index=False))
print("\nShare directa NEA por provincia y era:")
print(t2[t2["nea"]].pivot_table(index="provincia", columns="era",
                                values="share_directa").reindex(sorted(NEA))
      [ERAS].round(3).to_string())
print("OK -> tables/, figures/")
