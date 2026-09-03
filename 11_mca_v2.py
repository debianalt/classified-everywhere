"""
11 — MCA v2: respecified geometry of the supplier space
=======================================================
Project: Who Sells to the State (2026_2). GAN V2 remedy.

- Actives = endowments only: personería, rubro modal, modal client type.
- Supplementary = trajectory categories (intensity, tenure, directa) and
  provinces, projected as barycenters of their suppliers.
- Benzécri-corrected inertia; silhouette scan for k.
- Fracture test: separate MCAs per province stratum (metro / intermediate /
  peripheral); congruence of dim-1 category structure across strata.
- Provincial centroid trajectories per era: dispersion of centroids as the
  geometric counterpart of the Theil between-share.

Input:  data/processed/adjudicaciones_tipo.parquet
        data/processed/supplier_master.parquet
Output: tables/tab_mca2_benzecri.csv, tab_mca2_congruencia.csv,
        tab_mca2_centroides_provincia_era.csv
        figures/fig_mca2_espacio.png

Usage:  python 11_mca_v2.py
"""
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import prince
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score

PROJECT = Path(__file__).parent
PROC = PROJECT / "data" / "processed"
TAB = PROJECT / "tables"
FIG = PROJECT / "figures"

NEA = {"Misiones", "Corrientes", "Chaco", "Formosa"}
METRO = {"CABA", "Buenos Aires", "Córdoba", "Santa Fe", "Mendoza"}
PERIF = NEA | {"Catamarca", "Jujuy", "La Rioja", "Salta",
               "Santiago del Estero", "Tucumán"}
ERAS = ["macri", "fernandez", "milei"]

adj = pd.read_parquet(PROC / "adjudicaciones_tipo.parquet")
adj = adj[adj["es_nuevo"] & adj["provincia"].notna()]
master = pd.read_parquet(PROC / "supplier_master.parquet")

cliente_modal = (adj.groupby("cuit")["tipo_organismo"]
                 .agg(lambda s: s.mode().iat[0]).rename("cliente_modal"))
m = (master[master["adjudicado"] & master["provincia"].notna()]
     .merge(cliente_modal, on="cuit", how="left").dropna(subset=["cliente_modal"]))

m["estrato"] = np.select([m["provincia"].isin(METRO), m["provincia"].isin(PERIF)],
                         ["metro", "periferica"], default="intermedia")


def band_personeria(p):
    return {"Persona Fisica": "PersFisica", "Sociedad Anonima": "SA",
            "Sociedad Responsabilidad Limitada": "SRL"}.get(p, "OtraForma")


top_rubros = m["rubro_modal"].value_counts().head(8).index
m["A_personeria"] = m["personeria"].map(band_personeria)
m["A_rubro"] = np.where(m["rubro_modal"].isin(top_rubros),
                        m["rubro_modal"].str.slice(0, 18), "OTROS").astype(str)
m["A_cliente"] = m["cliente_modal"]
ACTIVE = ["A_personeria", "A_rubro", "A_cliente"]

m["S_intensidad"] = pd.cut(m["n_adjudicaciones"], [0, 1, 5, 20, np.inf],
                           labels=["1adj", "2-5adj", "6-20adj", "20+adj"]).astype(str)
m["S_tenure"] = pd.cut(m["ultimo_ejercicio"] - m["primer_ejercicio"], [-1, 0, 3, np.inf],
                       labels=["1anio", "2-4anios", "5+anios"]).astype(str)
m["S_directa"] = pd.cut(m["share_directa"], [-0.01, 0.001, 0.5, 1.0],
                        labels=["sin_directa", "mixto", "mayoria_directa"]).astype(str)
SUPP = ["S_intensidad", "S_tenure", "S_directa"]


def benzecri(raw_eig, K):
    thr = 1.0 / K
    adj_e = [((K / (K - 1)) * (e - thr)) ** 2 for e in raw_eig if e > thr]
    total = sum(adj_e)
    return [e / total for e in adj_e]


def fit_mca(df):
    mca = prince.MCA(n_components=5, random_state=42).fit(df[ACTIVE])
    rows = mca.row_coordinates(df[ACTIVE])
    rows.columns = [f"dim{i+1}" for i in range(rows.shape[1])]
    return mca, rows


mca, coords = fit_mca(m)
raw_eig = list(mca.eigenvalues_)
bz = benzecri(raw_eig, K=len(ACTIVE))
pd.DataFrame({"dim": range(1, len(raw_eig) + 1), "eigenvalue": raw_eig,
              "pct_raw": [e / sum(raw_eig) for e in raw_eig],
              "pct_benzecri": bz + [np.nan] * (len(raw_eig) - len(bz))}
             ).to_csv(TAB / "tab_mca2_benzecri.csv", index=False)
print(f"Benzécri: dim1 {bz[0]:.1%}, dim2 {bz[1]:.1%}" if len(bz) > 1
      else f"Benzécri: dim1 {bz[0]:.1%}")

sil = {}
for k in range(3, 9):
    lab = KMeans(n_clusters=k, n_init=10, random_state=42).fit_predict(coords[["dim1", "dim2"]])
    sil[k] = silhouette_score(coords[["dim1", "dim2"]].sample(min(5000, len(coords)),
                                                              random_state=1),
                              pd.Series(lab, index=coords.index)
                              .loc[coords.sample(min(5000, len(coords)),
                                                 random_state=1).index])
best_k = max(sil, key=sil.get)
print("silhouette:", {k: round(v, 3) for k, v in sil.items()}, "| best k =", best_k)
m["cluster"] = KMeans(n_clusters=best_k, n_init=10, random_state=42) \
    .fit_predict(coords[["dim1", "dim2"]])

# ── Supplementary projections: barycenters ───────────────────────────────────
def barycenters(var):
    return (coords[["dim1", "dim2"]].groupby(m[var].values).mean()
            .assign(variable=var))


supp_pts = pd.concat([barycenters(v) for v in SUPP + ["provincia"]])

# provincial centroids per era (suppliers active in each era)
act = (adj.groupby(["cuit", "era"]).size().rename("n").reset_index())
act = act.merge(coords.assign(cuit=m["cuit"].values), on="cuit", how="inner") \
         .merge(m[["cuit", "provincia"]], on="cuit", how="left")
cent = (act.groupby(["provincia", "era"])[["dim1", "dim2"]].mean().reset_index())
disp = (cent.groupby("era")[["dim1", "dim2"]]
        .apply(lambda g: np.sqrt(g.var(ddof=0).sum())).rename("dispersion_centroides"))
cent.to_csv(TAB / "tab_mca2_centroides_provincia_era.csv", index=False)

# ── Fracture test: congruence of dim1 across strata ──────────────────────────
def cat_coords_dim1(df):
    mca_s = prince.MCA(n_components=3, random_state=42).fit(df[ACTIVE])
    cc = mca_s.column_coordinates(df[ACTIVE])[0]
    ev = benzecri(list(mca_s.eigenvalues_), K=len(ACTIVE))
    return cc, ev[0] if ev else np.nan


base_cc, _ = cat_coords_dim1(m[m["estrato"] == "metro"])
rows = []
for estrato in ["metro", "intermedia", "periferica"]:
    cc, bz1 = cat_coords_dim1(m[m["estrato"] == estrato])
    common = base_cc.index.intersection(cc.index)
    r = np.corrcoef(base_cc[common], cc[common])[0, 1]
    congr = abs(r)  # axis sign is arbitrary
    rows.append({"estrato": estrato, "n": (m["estrato"] == estrato).sum(),
                 "benzecri_dim1": round(bz1, 3),
                 "congruencia_dim1_vs_metro": round(congr, 3)})
cg = pd.DataFrame(rows)
cg.to_csv(TAB / "tab_mca2_congruencia.csv", index=False)

# ── Figure ───────────────────────────────────────────────────────────────────
fig, ax = plt.subplots(figsize=(10, 8))
ax.scatter(coords["dim1"], coords["dim2"], s=4, alpha=0.15, color="#bbbbbb")
cols = mca.column_coordinates(m[ACTIVE])
for cat, r in cols.iterrows():
    ax.annotate(str(cat)[:26], (r[0], r[1]), fontsize=7, color="#1f4e79", weight="bold")
for prov, r in supp_pts[supp_pts["variable"] == "provincia"].iterrows():
    is_nea = prov in NEA
    ax.scatter(r["dim1"], r["dim2"], marker="s",
               s=70 if is_nea else 28, color="#d62728" if is_nea else "#e8a0a0",
               zorder=5)
    if is_nea or prov in {"CABA", "Buenos Aires", "Córdoba"}:
        ax.annotate(prov, (r["dim1"], r["dim2"]), fontsize=8,
                    color="#a00", xytext=(4, 4), textcoords="offset points")
for era, marker in zip(ERAS, ["o", "D", "*"]):
    d = cent[cent["provincia"].isin(NEA) & (cent["era"] == era)]
    ax.scatter(d["dim1"], d["dim2"], marker=marker, s=60, facecolors="none",
               edgecolors="#7b241c", label=f"centroides NEA {era}")
ax.axhline(0, color="grey", lw=0.5)
ax.axvline(0, color="grey", lw=0.5)
ax.set_xlabel(f"Dim 1 ({bz[0]:.0%} Benzécri)")
ax.set_ylabel(f"Dim 2 ({bz[1]:.0%} Benzécri)" if len(bz) > 1 else "Dim 2")
ax.set_title("El espacio social de los proveedores del Estado — categorías activas,\n"
             "provincias como puntos suplementarios (NEA rojo)")
ax.legend(fontsize=7)
fig.tight_layout()
fig.savefig(FIG / "fig_mca2_espacio.png", dpi=200)
plt.close(fig)

print("\nDispersión de centroides provinciales por era (contraparte geométrica del Theil):")
print(disp.reindex(ERAS).round(4).to_string())
print("\nCongruencia dim1 vs metro (fracture test):")
print(cg.to_string(index=False))
print("\nCentroides NEA por era (dim1):")
print(cent[cent["provincia"].isin(NEA)].pivot_table(index="provincia",
      columns="era", values="dim1").reindex(sorted(NEA))[ERAS].round(3).to_string())
print("OK -> tables/, figures/")
