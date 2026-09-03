"""
10 — Correspondence analysis: territorial supply x state demand
===============================================================
Project: Who Sells to the State (2026_2). GAN V3 remedy — the central
relational instrument: provinces (supply positions) and buyer types (demand
poles) in one geometric space, per era; plus rubro x buyer-type.

Input:  data/processed/adjudicaciones_tipo.parquet
Output: tables/tab_ca_coords_provincia_tipo.csv
        figures/fig_ca_provincia_tipo.png
        figures/fig_ca_rubro_tipo.png

Usage:  python 10_ca_provincia_organismo.py
"""
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import prince

PROJECT = Path(__file__).parent
PROC = PROJECT / "data" / "processed"
TAB = PROJECT / "tables"
FIG = PROJECT / "figures"

NEA = {"Misiones", "Corrientes", "Chaco", "Formosa"}
ERAS = ["macri", "fernandez", "milei"]

adj = pd.read_parquet(PROC / "adjudicaciones_tipo.parquet")
adj = adj[adj["es_nuevo"] & adj["provincia"].notna()]

# ── CA provincia x tipo_organismo, pooled and per era ────────────────────────
def run_ca(table):
    ca = prince.CA(n_components=2, random_state=42).fit(table)
    return (ca, ca.row_coordinates(table), ca.column_coordinates(table))


coords_all = []
fig, axes = plt.subplots(1, 3, figsize=(16, 5.2))
for ax, era in zip(axes, ERAS):
    xt = pd.crosstab(adj.loc[adj["era"] == era, "provincia"],
                     adj.loc[adj["era"] == era, "tipo_organismo"])
    xt = xt[xt.sum(axis=1) >= 30]  # provinces with enough mass
    ca, rows, cols = run_ca(xt)
    inertia = ca.percentage_of_variance_
    for prov, r in rows.iterrows():
        is_nea = prov in NEA
        ax.scatter(r[0], r[1], s=60 if is_nea else 25,
                   color="#d62728" if is_nea else "#7fb3d5", zorder=3)
        ax.annotate(prov, (r[0], r[1]), fontsize=6.5,
                    color="#a00" if is_nea else "#333",
                    xytext=(3, 3), textcoords="offset points")
        coords_all.append({"era": era, "punto": prov, "clase": "provincia",
                           "dim1": r[0], "dim2": r[1]})
    for tipo, r in cols.iterrows():
        ax.scatter(r[0], r[1], marker="^", s=120, color="#2ca02c", zorder=4)
        ax.annotate(tipo, (r[0], r[1]), fontsize=8, weight="bold",
                    color="#155d15", xytext=(4, -8), textcoords="offset points")
        coords_all.append({"era": era, "punto": tipo, "clase": "tipo_organismo",
                           "dim1": r[0], "dim2": r[1]})
    ax.axhline(0, color="grey", lw=0.5)
    ax.axvline(0, color="grey", lw=0.5)
    ax.set_title(f"{era} (dim1 {inertia[0]:.0f}%, dim2 {inertia[1]:.0f}%)")
fig.suptitle("CA: provincias (oferta territorial) × tipos de organismo (demanda estatal)")
fig.tight_layout()
fig.savefig(FIG / "fig_ca_provincia_tipo.png", dpi=200)
plt.close(fig)
pd.DataFrame(coords_all).to_csv(TAB / "tab_ca_coords_provincia_tipo.csv", index=False)

# ── CA rubro x tipo_organismo (pooled) ───────────────────────────────────────
top_rubros = adj["rubro_principal"].value_counts().head(20).index
xt2 = pd.crosstab(adj.loc[adj["rubro_principal"].isin(top_rubros), "rubro_principal"],
                  adj.loc[adj["rubro_principal"].isin(top_rubros), "tipo_organismo"])
ca2, rows2, cols2 = run_ca(xt2)
fig, ax = plt.subplots(figsize=(9, 7))
for rub, r in rows2.iterrows():
    ax.scatter(r[0], r[1], s=20, color="#7f7f7f")
    ax.annotate(str(rub)[:22], (r[0], r[1]), fontsize=6.5, color="#444",
                xytext=(3, 3), textcoords="offset points")
for tipo, r in cols2.iterrows():
    ax.scatter(r[0], r[1], marker="^", s=140, color="#2ca02c", zorder=4)
    ax.annotate(tipo, (r[0], r[1]), fontsize=9, weight="bold", color="#155d15")
ax.axhline(0, color="grey", lw=0.5)
ax.axvline(0, color="grey", lw=0.5)
ax.set_title(f"CA: rubros × tipos de organismo "
             f"(dim1 {ca2.percentage_of_variance_[0]:.0f}%, "
             f"dim2 {ca2.percentage_of_variance_[1]:.0f}%)")
fig.tight_layout()
fig.savefig(FIG / "fig_ca_rubro_tipo.png", dpi=200)
plt.close(fig)

# print NEA positions per era vs the seguridad pole
print("Posición NEA vs polo seguridad_defensa (dim1, por era):")
cdf = pd.DataFrame(coords_all)
for era in ERAS:
    d = cdf[cdf["era"] == era].set_index("punto")
    if "seguridad_defensa" not in d.index:
        continue
    pole = d.loc["seguridad_defensa", "dim1"]
    neap = d[d.index.isin(NEA)]["dim1"]
    print(f"  {era:10s} polo_seg={pole:+.2f} | " +
          " ".join(f"{p}={v:+.2f}" for p, v in neap.items()))
print("OK -> tables/, figures/")
