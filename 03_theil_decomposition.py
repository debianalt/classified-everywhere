"""
03 — Theil decomposition of the national supplier market
========================================================
Project: Who Sells to the State (2026_2). Adapts 2026_12_SER conventions.

Cut (a): Theil T of adjudicated ARS amounts across suppliers, per year,
         decomposed between/within province of supplier domicile.
         Also on award counts (inflation-free by construction).
Cut (b): Shannon H of rubro composition within each province x era
         (supplier-monoculture test), normalised by ln(K).
Cut (c): Nested Theil by era: between-region (NEA vs resto) ->
         between-province within region -> within-province.

Input:  data/processed/adjudicaciones_clean.parquet
Output: tables/tab_theil_provincia_anual.csv
        tables/tab_shannon_rubros_provincia_era.csv
        tables/tab_theil_anidado_era.csv
        figures/fig_theil_temporal.png
        figures/fig_shannon_nea.png

Usage:  python 03_theil_decomposition.py
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
TAB.mkdir(exist_ok=True)
FIG.mkdir(exist_ok=True)

NEA = {"Misiones", "Corrientes", "Chaco", "Formosa"}
ERAS = ["macri", "fernandez", "milei"]


def theil_t(x):
    x = np.asarray(x, dtype=float)
    x = x[x > 0]
    if len(x) < 2:
        return np.nan
    mu = x.mean()
    return float(np.mean((x / mu) * np.log(x / mu)))


def theil_decompose(df, value, group):
    """Return total, between, within Theil T of `value` across rows grouped by `group`."""
    d = df[df[value] > 0]
    if len(d) < 2:
        return np.nan, np.nan, np.nan
    mu = d[value].mean()
    n = len(d)
    total = theil_t(d[value])
    between = within = 0.0
    for _, g in d.groupby(group):
        ng, mug = len(g), g[value].mean()
        between += (ng / n) * (mug / mu) * np.log(mug / mu)
        tg = theil_t(g[value])
        if not np.isnan(tg):
            within += (ng / n) * (mug / mu) * tg
    return total, between, within


adj = pd.read_parquet(PROC / "adjudicaciones_clean.parquet")
adj = adj[adj["es_nuevo"] & adj["provincia"].notna()]
ars = adj[adj["moneda"] == "ARS"]

# ── Cut (a): yearly Theil across suppliers, between/within province ─────────
rows = []
for year, g in ars.groupby("ejercicio"):
    firm = (g.groupby(["cuit", "provincia"], as_index=False)
            .agg(monto=("monto", "sum"), n=("monto", "size")))
    for value in ["monto", "n"]:
        tot, bet, wit = theil_decompose(firm, value, "provincia")
        rows.append({"ejercicio": year, "variable": value, "theil_total": tot,
                     "theil_between": bet, "theil_within": wit,
                     "share_between": bet / tot if tot else np.nan,
                     "n_firmas": len(firm)})
ta = pd.DataFrame(rows)
ta.to_csv(TAB / "tab_theil_provincia_anual.csv", index=False)

# ── Cut (b): rubro Shannon H per province x era ──────────────────────────────
def shannon(counts):
    p = counts / counts.sum()
    p = p[p > 0]
    return float(-(p * np.log(p)).sum())


k_rubros = adj["rubro_principal"].nunique()
rows = []
for (prov, era), g in adj.groupby(["provincia", "era"]):
    counts = g["rubro_principal"].value_counts()
    h = shannon(counts)
    rows.append({"provincia": prov, "era": era, "n_adj": len(g),
                 "n_rubros": len(counts), "shannon_h": h,
                 "shannon_norm": h / np.log(k_rubros),
                 "nea": prov in NEA})
tb = pd.DataFrame(rows)
tb.to_csv(TAB / "tab_shannon_rubros_provincia_era.csv", index=False)

# ── Cut (c): nested Theil by era ─────────────────────────────────────────────
rows = []
for era, g in ars.groupby("era"):
    firm = (g.groupby(["cuit", "provincia"], as_index=False)
            .agg(monto=("monto", "sum")))
    firm["region"] = np.where(firm["provincia"].isin(NEA), "NEA", "Resto")
    tot, bet_reg, _ = theil_decompose(firm, "monto", "region")
    _, bet_prov, wit_prov = theil_decompose(firm, "monto", "provincia")
    rows.append({"era": era, "theil_total": tot,
                 "between_region": bet_reg,
                 "between_provincia": bet_prov,
                 "between_prov_within_region": bet_prov - bet_reg,
                 "within_provincia": wit_prov})
tc = pd.DataFrame(rows).set_index("era").reindex(ERAS).reset_index()
tc.to_csv(TAB / "tab_theil_anidado_era.csv", index=False)

# ── Figures ──────────────────────────────────────────────────────────────────
fig, axes = plt.subplots(1, 2, figsize=(11, 4.2))
for ax, var, lab in zip(axes, ["monto", "n"],
                        ["Montos ARS", "Cantidad de adjudicaciones"]):
    d = ta[ta["variable"] == var]
    ax.plot(d["ejercicio"], d["theil_total"], "o-", label="Theil total", color="#333")
    ax.plot(d["ejercicio"], d["theil_between"], "s--", label="Between provincia", color="#d62728")
    ax2 = ax.twinx()
    ax2.bar(d["ejercicio"], 100 * d["share_between"], alpha=0.15, color="#1f77b4")
    ax2.set_ylabel("share between (%)", color="#1f77b4")
    for x in [2019.5, 2023.5]:
        ax.axvline(x, color="grey", lw=0.8, ls=":")
    ax.set_title(f"Theil T entre proveedores — {lab}")
    ax.set_xlabel("Ejercicio")
    ax.legend(fontsize=8)
fig.tight_layout()
fig.savefig(FIG / "fig_theil_temporal.png", dpi=200)
plt.close(fig)

fig, ax = plt.subplots(figsize=(8, 4.5))
for prov in sorted(NEA):
    d = tb[tb["provincia"] == prov].set_index("era").reindex(ERAS)
    ax.plot(ERAS, d["shannon_norm"], "o-", label=prov)
rest = (tb[~tb["nea"]].groupby("era")
        .apply(lambda g: np.average(g["shannon_norm"], weights=g["n_adj"]),
               include_groups=False).reindex(ERAS))
ax.plot(ERAS, rest.values, "k--", lw=2, label="Resto (prom. ponderado)")
ax.set_ylabel("Shannon H normalizado (rubros)")
ax.set_title("Diversidad de rubros de proveedores por era — NEA vs resto")
ax.legend(fontsize=8)
fig.tight_layout()
fig.savefig(FIG / "fig_shannon_nea.png", dpi=200)
plt.close(fig)

print("Cut (a) — share between-provincia por año (montos):")
print(ta[ta["variable"] == "monto"][["ejercicio", "theil_total", "share_between"]]
      .round(3).to_string(index=False))
print("\nCut (b) — Shannon normalizado, NEA por era:")
print(tb[tb["nea"]].pivot_table(index="provincia", columns="era",
                                values="shannon_norm").reindex(sorted(NEA))
      .round(3).to_string())
print("\nCut (c) — Theil anidado por era:")
print(tc.round(4).to_string(index=False))
print("OK -> tables/, figures/")
