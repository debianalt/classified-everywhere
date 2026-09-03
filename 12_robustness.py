"""
12 — Robustness battery (GAN V4, V5, V6)
========================================
Project: Who Sells to the State (2026_2)

(1) Theil between-province share per year: baseline vs excluding
    security-defence buyers vs reweighted to the constant 2016-2019
    buyer-type mix (decides whether the fracture is structural or the
    militarisation of demand).
(2) Cohort survival stratified by modal client type (territorial vs not).
(3) Supplier-level exit ratio (unique winning firms / company stock).

Input:  data/processed/adjudicaciones_tipo.parquet, supplier_master.parquet,
        denominators_provincia_anio.parquet
Output: tables/tab_robustez_theil.csv, tab_robustez_supervivencia.csv,
        tab_exit_ratio_v2.csv
        figures/fig_robustez_theil.png

Usage:  python 12_robustness.py
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


def theil_between_share(firm, value="monto", group="provincia"):
    d = firm[firm[value] > 0]
    if len(d) < 2:
        return np.nan
    mu, n = d[value].mean(), len(d)
    x = d[value].to_numpy()
    total = float(np.mean((x / mu) * np.log(x / mu)))
    between = 0.0
    for _, g in d.groupby(group):
        ng, mug = len(g), g[value].mean()
        between += (ng / n) * (mug / mu) * np.log(mug / mu)
    return between / total if total else np.nan


adj = pd.read_parquet(PROC / "adjudicaciones_tipo.parquet")
adj = adj[adj["es_nuevo"] & adj["provincia"].notna() & (adj["moneda"] == "ARS")]

base_mix = (adj[adj["ejercicio"].between(2016, 2019)]["tipo_organismo"]
            .value_counts(normalize=True))
base_mix_org = (adj[adj["ejercicio"].between(2016, 2019)]["organismo"]
                .value_counts(normalize=True))

rows = []
for year, g in adj.groupby("ejercicio"):
    # baseline
    firm = g.groupby(["cuit", "provincia"], as_index=False)["monto"].sum()
    rows.append({"ejercicio": year, "variante": "baseline",
                 "share_between": theil_between_share(firm)})
    # excluding security-defence
    g2 = g[g["tipo_organismo"] != "seguridad_defensa"]
    firm2 = g2.groupby(["cuit", "provincia"], as_index=False)["monto"].sum()
    rows.append({"ejercicio": year, "variante": "sin_seguridad",
                 "share_between": theil_between_share(firm2)})
    # constant buyer mix (reweight amounts to 2016-2019 type shares)
    yr_mix = g["tipo_organismo"].value_counts(normalize=True)
    w = g["tipo_organismo"].map(base_mix / yr_mix).fillna(0)
    g3 = g.assign(monto_w=g["monto"] * w)
    firm3 = (g3.groupby(["cuit", "provincia"], as_index=False)["monto_w"].sum()
             .rename(columns={"monto_w": "monto"}))
    rows.append({"ejercicio": year, "variante": "mix_constante",
                 "share_between": theil_between_share(firm3)})
    # finer variant: constant mix at organism level (162 buyers)
    yr_org = g["organismo"].value_counts(normalize=True)
    w4 = g["organismo"].map(base_mix_org / yr_org).fillna(0)
    firm4 = (g.assign(monto_w=g["monto"] * w4)
             .groupby(["cuit", "provincia"], as_index=False)["monto_w"].sum()
             .rename(columns={"monto_w": "monto"}))
    rows.append({"ejercicio": year, "variante": "mix_constante_organismo",
                 "share_between": theil_between_share(firm4)})
rt = pd.DataFrame(rows)
rt.to_csv(TAB / "tab_robustez_theil.csv", index=False)

fig, ax = plt.subplots(figsize=(8, 4.5))
for var, style in [("baseline", "o-"), ("sin_seguridad", "s--"),
                   ("mix_constante", "^:")]:
    d = rt[(rt["variante"] == var) & rt["ejercicio"].between(2017, 2025)]
    ax.plot(d["ejercicio"], 100 * d["share_between"], style, label=var)
for x in [2019.5, 2023.5]:
    ax.axvline(x, color="grey", lw=0.8, ls=":")
ax.set_ylabel("Theil between-provincia (% del total)")
ax.set_title("Robustez de la fractura territorial (montos ARS, 2017-2025)")
ax.legend()
fig.tight_layout()
fig.savefig(FIG / "fig_robustez_theil.png", dpi=200)
plt.close(fig)

# ── (2) Survival stratified by client type ───────────────────────────────────
# Territorial-client position measured AT COHORT ENTRY (first-year awards
# only) to avoid future-data leakage (external review 2.6): a supplier must
# not be classified by clients it acquired in later eras.
first_yr = adj.groupby("cuit")["ejercicio"].transform("min")
entry_awards = adj[adj["ejercicio"] == first_yr]
cliente_modal = (entry_awards.groupby("cuit")
                 .agg(territorial=("anclaje_territorial", "mean"),
                      provincia=("provincia", "first"),
                      entrada=("ejercicio", "first")))
cliente_modal["cliente_territorial"] = cliente_modal["territorial"] > 0.5
cliente_modal["region"] = np.select(
    [cliente_modal["provincia"].isin(NEA), cliente_modal["provincia"].isin(METRO)],
    ["NEA", "Metro"], default="Resto")
cliente_modal["cohorte"] = cliente_modal["entrada"].map(ERA_OF_YEAR)

act_era = (adj.assign(era=adj["ejercicio"].map(ERA_OF_YEAR))
           .groupby(["cuit", "era"]).size().unstack().notna())
rows = []
for (reg, terr), grp in cliente_modal[cliente_modal["cohorte"] == "macri"] \
        .groupby(["region", "cliente_territorial"]):
    surv_f = act_era.reindex(grp.index)["fernandez"].fillna(False).mean()
    surv_m = act_era.reindex(grp.index)["milei"].fillna(False).mean()
    rows.append({"region": reg, "cliente_territorial": terr, "n": len(grp),
                 "surv_fernandez": round(surv_f, 3), "surv_milei": round(surv_m, 3)})
rs = pd.DataFrame(rows)
rs.to_csv(TAB / "tab_robustez_supervivencia.csv", index=False)

# ── (3) Supplier-level exit ratio ────────────────────────────────────────────
den = pd.read_parquet(PROC / "denominators_provincia_anio.parquet")
master = pd.read_parquet(PROC / "supplier_master.parquet")
stock = den[den["ejercicio"] == 2025].set_index("provincia")["stock_sociedades"]
soc_cuits = set(master.loc[master["personeria"].notna() &
                           (master["personeria"] != "Persona Fisica"), "cuit"])
firms = adj.groupby("provincia")["cuit"].nunique().rename("firmas_ganadoras")
firms_soc = (adj[adj["cuit"].isin(soc_cuits)]
             .groupby("provincia")["cuit"].nunique().rename("sociedades_ganadoras"))
ex = pd.DataFrame({"firmas_ganadoras": firms, "sociedades_ganadoras": firms_soc,
                   "stock": stock})
for col, num in [("exit_ratio_firmas", "firmas_ganadoras"),
                 ("exit_ratio_soc", "sociedades_ganadoras")]:
    ex[col] = (ex[num] / ex[num].sum()) / (ex["stock"] / ex["stock"].sum())
ex["nea"] = ex.index.isin(NEA)
ex.round(4).to_csv(TAB / "tab_exit_ratio_v2.csv")

print("Theil between-share (%): baseline vs robustez")
print(rt.pivot_table(index="ejercicio", columns="variante", values="share_between")
      .mul(100).round(1).to_string())
print("\nSupervivencia cohorte Macri por región x cliente territorial:")
print(rs.to_string(index=False))
print("\nExit ratio a nivel firma (NEA + referencia):")
sel = sorted(NEA) + ["CABA", "Buenos Aires", "Córdoba"]
print(ex.loc[sel, ["firmas_ganadoras", "sociedades_ganadoras",
                   "exit_ratio_firmas", "exit_ratio_soc"]].round(3).to_string())
print("OK -> tables/, figures/")
