"""
04 — MCA of the supplier space
==============================
Project: Who Sells to the State (2026_2). Toolkit of 2026_11/2026_12.

MCA (prince) on categorical attributes of adjudicated suppliers; KMeans
clusters on row coordinates; region centroids (NEA / CABA-BsAs / Resto)
and cluster x region composition.

Input:  data/processed/supplier_master.parquet
Output: tables/tab_mca_cluster_profiles.csv
        tables/tab_cluster_por_region.csv
        figures/fig_mca_biplot.png

Usage:  python 04_supplier_space_mca.py
"""
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import prince
from sklearn.cluster import KMeans

PROJECT = Path(__file__).parent
PROC = PROJECT / "data" / "processed"
TAB = PROJECT / "tables"
FIG = PROJECT / "figures"

NEA = {"Misiones", "Corrientes", "Chaco", "Formosa"}
METRO = {"CABA", "Buenos Aires"}

m = pd.read_parquet(PROC / "supplier_master.parquet")
m = m[m["adjudicado"] & m["provincia"].notna()].copy()

m["region"] = np.select([m["provincia"].isin(NEA), m["provincia"].isin(METRO)],
                        ["NEA", "Metro"], default="Resto")

def band_personeria(p):
    if p == "Persona Fisica":
        return "PersFisica"
    if p == "Sociedad Anonima":
        return "SA"
    if p == "Sociedad Responsabilidad Limitada":
        return "SRL"
    return "OtraForma"

top_rubros = m["rubro_modal"].value_counts().head(8).index
m["A_personeria"] = m["personeria"].map(band_personeria).fillna("OtraForma")
m["A_rubro"] = np.where(m["rubro_modal"].isin(top_rubros),
                        m["rubro_modal"].str.slice(0, 18), "OTROS").astype(str)
m["A_intensidad"] = pd.cut(m["n_adjudicaciones"], [0, 1, 5, 20, np.inf],
                           labels=["1adj", "2-5adj", "6-20adj", "20+adj"]).astype(str)
m["A_tenure"] = pd.cut(m["ultimo_ejercicio"] - m["primer_ejercicio"],
                       [-1, 0, 3, 7, np.inf],
                       labels=["1solo_anio", "2-4anios", "5-8anios", "9+anios"]).astype(str)
m["A_directa"] = pd.cut(m["share_directa"], [-0.01, 0.001, 0.5, 1.0],
                        labels=["sin_directa", "mixto", "mayoria_directa"]).astype(str)
m["A_organismos"] = pd.cut(m["n_organismos"], [0, 1, 4, np.inf],
                           labels=["1org", "2-4org", "5+org"]).astype(str)

ACTIVE = ["A_personeria", "A_rubro", "A_intensidad", "A_tenure",
          "A_directa", "A_organismos"]
X = m[ACTIVE]

mca = prince.MCA(n_components=5, random_state=42).fit(X)
coords = mca.row_coordinates(X)
coords.columns = [f"dim{i+1}" for i in range(coords.shape[1])]
eig = mca.eigenvalues_summary
print(eig.head(3).to_string())

km = KMeans(n_clusters=5, n_init=10, random_state=42).fit(coords[["dim1", "dim2"]])
m["cluster"] = km.labels_

prof = (pd.concat([m[a].groupby(m["cluster"]).value_counts(normalize=True)
                   .rename("share").reset_index()
                   .assign(variable=a).rename(columns={a: "categoria"})
                   for a in ACTIVE])
        .query("share > 0.25"))
prof.to_csv(TAB / "tab_mca_cluster_profiles.csv", index=False)

ct = pd.crosstab(m["cluster"], m["region"], normalize="columns").round(3)
ct.to_csv(TAB / "tab_cluster_por_region.csv")

col_coords = mca.column_coordinates(X)
fig, ax = plt.subplots(figsize=(9, 7))
for reg, color in [("Metro", "#bbbbbb"), ("Resto", "#7fb3d5"), ("NEA", "#d62728")]:
    d = coords[m["region"].values == reg]
    ax.scatter(d["dim1"], d["dim2"], s=6, alpha=0.25 if reg != "NEA" else 0.6,
               color=color, label=f"{reg} (n={len(d)})")
    ax.scatter(d["dim1"].mean(), d["dim2"].mean(), s=180, marker="X",
               color=color, edgecolor="k", zorder=5)
for cat, row in col_coords.iterrows():
    if abs(row[0]) + abs(row[1]) > 0.8:
        ax.annotate(str(cat)[:24], (row[0], row[1]), fontsize=7, color="#333")
ax.axhline(0, color="grey", lw=0.5)
ax.axvline(0, color="grey", lw=0.5)
ax.set_xlabel("Dim 1")
ax.set_ylabel("Dim 2")
ax.set_title("Espacio de proveedores del Estado nacional (MCA) — centroides por región")
ax.legend()
fig.tight_layout()
fig.savefig(FIG / "fig_mca_biplot.png", dpi=200)
plt.close(fig)

print("\nComposición de clusters por región (share dentro de región):")
print(ct.to_string())
print("\nCentroides región (dim1, dim2):")
print(coords.assign(region=m["region"].values).groupby("region")[["dim1", "dim2"]]
      .mean().round(3).to_string())
print("OK -> tables/, figures/")
