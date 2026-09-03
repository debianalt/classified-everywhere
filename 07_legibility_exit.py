"""
07 — Legibility rates and the Hirschman exit test
=================================================
Project: Who Sells to the State (2026_2)

(1) Legibility: SIPRO-registered suppliers per 1,000 companies, two
    numerators (all suppliers / companies only, to avoid mixing personas
    físicas into a company denominator).
(2) "Dead registry": share of registered suppliers that never won.
(3) Exit test: representation ratio = province share of national awards
    (count and ARS amount) / province share of company stock. >1 means
    over-representation in the national market relative to economy size.
(4) Correlations with ITPP (mean 2016-2024) and INTRA 2024.
(5) Annex: CONTRAT.AR public works by province of execution (2017-2023).

Input:  data/processed/*.parquet, data/raw/contratar_*.csv
Output: tables/tab_legibilidad_exit.csv
        tables/tab_contratar_provincia_ejecucion.csv
        figures/fig_exit_ratio.png

Usage:  python 07_legibility_exit.py
"""
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

PROJECT = Path(__file__).parent
RAW = PROJECT / "data" / "raw"
PROC = PROJECT / "data" / "processed"
TAB = PROJECT / "tables"
FIG = PROJECT / "figures"

NEA = {"Misiones", "Corrientes", "Chaco", "Formosa"}

master = pd.read_parquet(PROC / "supplier_master.parquet")
adj = pd.read_parquet(PROC / "adjudicaciones_clean.parquet")
adj = adj[adj["es_nuevo"] & adj["provincia"].notna()]
den = pd.read_parquet(PROC / "denominators_provincia_anio.parquet")
cov = pd.read_parquet(PROC / "covariates_provincia_anio.parquet")

stock = den[den["ejercicio"] == 2025].set_index("provincia")["stock_sociedades"]

reg = master[master["provincia"].notna()]
t = pd.DataFrame({
    "sipro_total": reg.groupby("provincia").size(),
    "sipro_sociedades": reg[reg["personeria"] != "Persona Fisica"]
        .groupby("provincia").size(),
    "adjudicados": reg[reg["adjudicado"]].groupby("provincia").size(),
})
t["stock_sociedades"] = stock
t["legibilidad_total_x1000"] = 1000 * t["sipro_total"] / t["stock_sociedades"]
t["legibilidad_soc_x1000"] = 1000 * t["sipro_sociedades"] / t["stock_sociedades"]
t["registro_muerto"] = 1 - t["adjudicados"] / t["sipro_total"]

ars = adj[adj["moneda"] == "ARS"]
t["share_adj_n"] = adj.groupby("provincia").size() / len(adj)
t["share_adj_monto"] = ars.groupby("provincia")["monto"].sum() / ars["monto"].sum()
t["share_stock"] = t["stock_sociedades"] / t["stock_sociedades"].sum()
t["exit_ratio_n"] = t["share_adj_n"] / t["share_stock"]
t["exit_ratio_monto"] = t["share_adj_monto"] / t["share_stock"]

itpp_mean = (cov[cov["ejercicio"].between(2016, 2024)]
             .groupby("provincia")["itpp"].mean())
t["itpp_media_2016_2024"] = itpp_mean
t = t.merge(pd.read_csv(RAW / "covariates" / "intra_2024.csv")[["provincia", "intra_2024"]],
            left_index=True, right_on="provincia", how="left").set_index("provincia")
t["nea"] = t.index.isin(NEA)
t.round(4).to_csv(TAB / "tab_legibilidad_exit.csv")

corr = t[["legibilidad_soc_x1000", "exit_ratio_n", "registro_muerto",
          "itpp_media_2016_2024"]].corr(method="spearman")

# ── CONTRAT.AR annex: works by province of execution ─────────────────────────
con = pd.read_csv(RAW / "contratar_onc-contratar-contratos.csv", dtype=str)
ubi = pd.read_csv(RAW / "contratar_onc-contratar-ubicacion-geografica.csv", dtype=str)
ubi1 = ubi.drop_duplicates("numero_obra")[["numero_obra", "provincia_nombre"]]
con = con.merge(ubi1, on="numero_obra", how="left")
con["monto"] = pd.to_numeric(con["contrato_monto"], errors="coerce")
ca = (con.groupby("provincia_nombre")
      .agg(n_contratos=("contrato_numero", "size"),
           n_contratistas=("contratista_cuit", "nunique"),
           monto_total=("monto", "sum"))
      .sort_values("n_contratos", ascending=False))
ca.to_csv(TAB / "tab_contratar_provincia_ejecucion.csv")

# ── Figure: exit ratio ───────────────────────────────────────────────────────
d = t.sort_values("exit_ratio_n")
fig, ax = plt.subplots(figsize=(8, 7))
colors = ["#d62728" if x else "#7fb3d5" for x in d["nea"]]
ax.barh(d.index, d["exit_ratio_n"], color=colors)
ax.axvline(1, color="k", lw=0.8, ls="--")
ax.set_xlabel("Share adjudicaciones / share stock de sociedades (2016-2026)")
ax.set_title("Ratio de representación en el mercado estatal nacional\n(>1 = sobre-representada; NEA en rojo)")
fig.tight_layout()
fig.savefig(FIG / "fig_exit_ratio.png", dpi=200)
plt.close(fig)

print("Legibilidad, registro muerto y exit ratio (NEA + extremos):")
sel = t[t["nea"]].index.tolist() + ["CABA", "Buenos Aires", "Córdoba", "Santa Fe"]
print(t.loc[sel, ["legibilidad_soc_x1000", "registro_muerto", "exit_ratio_n",
                  "exit_ratio_monto", "itpp_media_2016_2024"]].round(3).to_string())
print("\nSpearman:")
print(corr.round(3).to_string())
print("\nCONTRAT.AR por provincia de ejecución (NEA):")
print(ca.loc[[p for p in ca.index if str(p).upper() in
              {"MISIONES", "CORRIENTES", "CHACO", "FORMOSA"}]].to_string())
print("OK -> tables/, figures/")
