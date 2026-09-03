"""
08 — Two tiers of state-mediated exchange (Auyero angle)
========================================================
Project: Who Sells to the State (2026_2)

Parses the 38 annual DNAP sheets (1987-2024) into a province-year panel of
provincial public employment; computes employment per 1,000 inhabitants
(currency-free, comparable across the whole series); contrasts it with
supplier density in the national procurement market (2016-2026):
employment-intensive vs contractor-intensive modes of state mediation.

Input:  data/raw/dnap_ocupacion_salarios_1987_2024.xlsx
        data/processed/supplier_master.parquet
        data/processed/denominators_provincia_anio.parquet
Output: data/processed/empleo_publico_panel.parquet
        tables/tab_empleo_publico_provincia.csv
        figures/fig_empleo_largo_nea.png
        figures/fig_two_tiers_scatter.png

Usage:  python 08_two_tiers.py
"""
import unicodedata
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

PROV_CANON = [
    "Buenos Aires", "CABA", "Catamarca", "Chaco", "Chubut", "Córdoba",
    "Corrientes", "Entre Ríos", "Formosa", "Jujuy", "La Pampa", "La Rioja",
    "Mendoza", "Misiones", "Neuquén", "Río Negro", "Salta", "San Juan",
    "San Luis", "Santa Cruz", "Santa Fe", "Santiago del Estero",
    "Tierra del Fuego", "Tucumán",
]


def _strip(s):
    return "".join(c for c in unicodedata.normalize("NFD", str(s))
                   if unicodedata.category(c) != "Mn").upper().strip()


_LOOKUP = {_strip(p): p for p in PROV_CANON}
_LOOKUP.update({"G.C.B.A.": "CABA", "GCBA": "CABA", "CAPITAL FEDERAL": "CABA",
                "TIERRA DEL FUEGO (**)": "Tierra del Fuego",
                "STGO. DEL ESTERO": "Santiago del Estero",
                "SGO. DEL ESTERO": "Santiago del Estero"})

# ── Parse the 38 sheets ──────────────────────────────────────────────────────
xl = pd.ExcelFile(RAW / "dnap_ocupacion_salarios_1987_2024.xlsx")
rows = []
for sheet in xl.sheet_names:
    try:
        year = int(sheet)
    except ValueError:
        continue
    df = xl.parse(sheet, header=None)
    hdr = df.index[df[1].astype(str).str.strip()
                   .isin(["PROVINCIAS", "JURISDICCIONES"])]
    if len(hdr) == 0:
        print(f"  WARN {sheet}: header no hallado, salto")
        continue
    for _, r in df.iloc[hdr[0] + 1:].iterrows():
        name = r[1]
        if pd.isna(name):
            continue
        prov = _LOOKUP.get(_strip(name))
        if prov is None:
            if _strip(name).startswith("TOTAL"):
                break
            continue
        rows.append({"provincia": prov, "anio": year,
                     "planta": pd.to_numeric(r[3], errors="coerce"),
                     "habitantes": pd.to_numeric(r[5], errors="coerce")})
emp = pd.DataFrame(rows).dropna(subset=["planta"])
emp["empleo_x1000hab"] = 1000 * emp["planta"] / emp["habitantes"]
emp.to_parquet(PROC / "empleo_publico_panel.parquet", index=False)
emp.to_csv(TAB / "tab_empleo_publico_provincia.csv", index=False)
print(f"panel empleo: {len(emp)} filas, {emp['anio'].min()}-{emp['anio'].max()}, "
      f"{emp['provincia'].nunique()} provincias")

# ── Long series figure: NEA vs national mean ─────────────────────────────────
fig, ax = plt.subplots(figsize=(10, 5))
for prov, color in zip(sorted(NEA), ["#d62728", "#ff7f0e", "#9467bd", "#8c564b"]):
    d = emp[emp["provincia"] == prov].sort_values("anio")
    ax.plot(d["anio"], d["empleo_x1000hab"], label=prov, color=color)
nat = (emp.groupby("anio").apply(
    lambda g: 1000 * g["planta"].sum() / g["habitantes"].sum(),
    include_groups=False))
ax.plot(nat.index, nat.values, "k--", lw=2, label="Total 24 jurisdicciones")
for x0, x1 in [(1989, 1999), (2003, 2015), (2015.9, 2019.9), (2023.9, 2024.5)]:
    ax.axvspan(x0, x1, alpha=0.06, color="grey")
ax.set_ylabel("Empleo público provincial por 1.000 habitantes")
ax.set_title("Planta pública provincial, 1987-2024 — NEA vs total nacional (DNAP)")
ax.legend(fontsize=8)
fig.tight_layout()
fig.savefig(FIG / "fig_empleo_largo_nea.png", dpi=200)
plt.close(fig)

# ── Two-tier scatter ─────────────────────────────────────────────────────────
master = pd.read_parquet(PROC / "supplier_master.parquet")
adjud = (master[master["adjudicado"] & master["provincia"].notna()]
         .groupby("provincia").size().rename("proveedores_adj"))
hab24 = emp[emp["anio"] == 2024].set_index("provincia")["habitantes"]
emp24 = emp[emp["anio"] == 2024].set_index("provincia")["empleo_x1000hab"]
tt = pd.DataFrame({"empleo_x1000hab_2024": emp24,
                   "proveedores_x100khab": 1e5 * adjud / hab24})
tt["nea"] = tt.index.isin(NEA)

fig, ax = plt.subplots(figsize=(8, 6))
for nea_flag, color, size in [(False, "#7fb3d5", 40), (True, "#d62728", 90)]:
    d = tt[tt["nea"] == nea_flag]
    ax.scatter(d["empleo_x1000hab_2024"], d["proveedores_x100khab"],
               color=color, s=size, zorder=3)
for prov, r in tt.iterrows():
    ax.annotate(prov, (r["empleo_x1000hab_2024"], r["proveedores_x100khab"]),
                fontsize=7, xytext=(3, 3), textcoords="offset points")
ax.set_xlabel("Empleo público provincial por 1.000 hab (2024) — clientelismo de personas")
ax.set_ylabel("Proveedores adjudicados por 100.000 hab (2016-2026) — contratismo de firmas")
ax.set_title("Dos pisos del intercambio mediado por el Estado (NEA en rojo)")
fig.tight_layout()
fig.savefig(FIG / "fig_two_tiers_scatter.png", dpi=200)
plt.close(fig)

print("\nEmpleo público x1000 hab — NEA, años seleccionados:")
sel = emp[emp["provincia"].isin(NEA) & emp["anio"].isin([1987, 1995, 2003, 2015, 2024])]
print(sel.pivot_table(index="provincia", columns="anio", values="empleo_x1000hab")
      .round(1).to_string())
print("\nDos pisos (2024):")
print(tt.sort_values("empleo_x1000hab_2024", ascending=False).round(1)
      .head(10).to_string())
print("OK -> data/processed/, tables/, figures/")
