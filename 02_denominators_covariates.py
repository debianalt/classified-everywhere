"""
02 — Denominators and covariates
================================
Project: Who Sells to the State (2026_2)

(a) Company stock per province-year from the ARCA national registry
    (companies only — no personas físicas; consistent with 2026_12).
(b) Legibility rate: SIPRO-registered suppliers per 1,000 companies.
(c) Province-level covariates: political eras, NEA flag, hand-coded
    provincial regime (data/raw/covariates/regimen_provincial.csv, flagged
    VERIFICAR), ITPP 2014-2024 (CIPPEC Tabla 9), INTRA 2024, IPC deflator
    (series API, optional).

Input:  ../../gee/arca/data/registro-nacional-sociedades-20260223.csv
        data/processed/supplier_master.parquet
        data/raw/covariates/*.csv
Output: data/processed/denominators_provincia_anio.parquet
        data/processed/covariates_provincia_anio.parquet
        data/processed/ipc_anual.csv (if API reachable)

Usage:  python 02_denominators_covariates.py
"""
import io
import unicodedata
import urllib.request
from pathlib import Path

import numpy as np
import pandas as pd

PROJECT = Path(__file__).parent
RAW = PROJECT / "data" / "raw"
PROC = PROJECT / "data" / "processed"
COV = RAW / "covariates"
ARCA_REGISTRY = PROJECT.parent.parent / "gee" / "arca" / "data" / "registro-nacional-sociedades-20260223.csv"

NEA = {"Misiones", "Corrientes", "Chaco", "Formosa"}
YEARS = list(range(2014, 2027))
ERA_OF_YEAR = {**{y: "macri" for y in range(2016, 2020)},
               **{y: "fernandez" for y in range(2020, 2024)},
               **{y: "milei" for y in range(2024, 2027)}}

PROV_CANON = [
    "Buenos Aires", "CABA", "Catamarca", "Chaco", "Chubut", "Córdoba",
    "Corrientes", "Entre Ríos", "Formosa", "Jujuy", "La Pampa", "La Rioja",
    "Mendoza", "Misiones", "Neuquén", "Río Negro", "Salta", "San Juan",
    "San Luis", "Santa Cruz", "Santa Fe", "Santiago del Estero",
    "Tierra del Fuego", "Tucumán",
]


def _strip_accents(s):
    return "".join(c for c in unicodedata.normalize("NFD", s) if unicodedata.category(c) != "Mn")


_LOOKUP = {_strip_accents(p).upper(): p for p in PROV_CANON}
_LOOKUP.update({
    "CIUDAD AUTONOMA DE BUENOS AIRES": "CABA", "CIUDAD AUTONOMA BUENOS AIRES": "CABA",
    "CIUDAD DE BUENOS AIRES": "CABA", "CAPITAL FEDERAL": "CABA",
    "TIERRA DEL FUEGO, ANTARTIDA E ISLAS DEL ATLANTICO SUR": "Tierra del Fuego",
})


def norm_provincia(s):
    if pd.isna(s) or not str(s).strip():
        return np.nan
    return _LOOKUP.get(_strip_accents(str(s).strip()).upper(), np.nan)


# ── (a) company stock per province-year ──────────────────────────────────────
parts = []
for chunk in pd.read_csv(ARCA_REGISTRY, dtype=str, chunksize=500_000,
                         usecols=["cuit", "dom_fiscal_provincia",
                                  "fecha_hora_contrato_social"],
                         on_bad_lines="skip"):
    chunk = chunk.drop_duplicates("cuit")
    parts.append(chunk)
reg = pd.concat(parts, ignore_index=True).drop_duplicates("cuit")
reg["provincia"] = reg["dom_fiscal_provincia"].map(norm_provincia)
reg["anio_fund"] = pd.to_numeric(reg["fecha_hora_contrato_social"].str[:4],
                                 errors="coerce")
reg = reg.dropna(subset=["provincia", "anio_fund"])
print(f"registro ARCA: {len(reg)} sociedades con provincia y año de fundación")

founded = (reg.groupby(["provincia", "anio_fund"]).size()
           .rename("fundadas").reset_index())
rows = []
for prov in PROV_CANON:
    f = founded[founded["provincia"] == prov].set_index("anio_fund")["fundadas"]
    for y in YEARS:
        rows.append({"provincia": prov, "ejercicio": y,
                     "stock_sociedades": int(f[f.index <= y].sum())})
den = pd.DataFrame(rows)

# ── (b) legibility rate ──────────────────────────────────────────────────────
master = pd.read_parquet(PROC / "supplier_master.parquet")
sipro_prov = (master[master["provincia_sipro"].notna()]
              .groupby("provincia_sipro").size().rename("proveedores_sipro"))
den = den.merge(sipro_prov, left_on="provincia", right_index=True, how="left")
den["legibilidad_x1000"] = np.where(
    den["ejercicio"] == 2025,
    1000 * den["proveedores_sipro"] / den["stock_sociedades"], np.nan)

# ── (c) covariates ───────────────────────────────────────────────────────────
itpp = (pd.read_csv(COV / "itpp_serie_2014_2024_wide.csv")
        .melt(id_vars="provincia", var_name="ejercicio", value_name="itpp"))
itpp["ejercicio"] = itpp["ejercicio"].astype(int)

intra = pd.read_csv(COV / "intra_2024.csv")
reg_pol = pd.read_csv(COV / "regimen_provincial.csv")

cov = den.merge(itpp, on=["provincia", "ejercicio"], how="left")
cov["era"] = cov["ejercicio"].map(ERA_OF_YEAR)
cov = cov.merge(reg_pol[["provincia", "era", "partido", "familia_politica",
                         "alineado", "partido_en_poder_desde"]],
                on=["provincia", "era"], how="left")
cov["continuidad_anios"] = cov["ejercicio"] - cov["partido_en_poder_desde"]
cov["nea"] = cov["provincia"].isin(NEA)
cov = cov.merge(intra[["provincia", "intra_2024"]], on="provincia", how="left")

# ── IPC deflator ─────────────────────────────────────────────────────────────
# The bare urlopen call returned 403 in the first pass: the series API rejects
# requests without a User-Agent. With the header it serves the CSV normally
# (checked 28-08-2026).
try:
    url = ("https://apis.datos.gob.ar/series/api/series/"
           "?ids=148.3_INIVELNAL_DICI_M_26&format=csv&limit=1000")
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=60) as r:
        ipc = pd.read_csv(io.BytesIO(r.read()))
    ipc.columns = ["fecha", "ipc"]
    ipc["anio"] = pd.to_datetime(ipc["fecha"]).dt.year
    ipc_a = ipc.groupby("anio")["ipc"].mean().reset_index()
    base = ipc_a.loc[ipc_a["anio"] == 2024, "ipc"].iat[0]
    ipc_a["deflactor_2024"] = base / ipc_a["ipc"]
    ipc_a.to_csv(PROC / "ipc_anual.csv", index=False)
    print(f"IPC: {ipc_a['anio'].min()}-{ipc_a['anio'].max()} OK (base 2024)")
except Exception as e:
    print(f"AVISO — DEFLACTOR NO DISPONIBLE ({e}); los cortes por era deflactados "
          "no se pueden correr. Las métricas intra-año son scale-invariant.")

# ── Tipo de cambio oficial (BNA vendedor, promedio anual) ────────────────────
# Las cifras en dolares del manuscrito se calculan sobre el monto NOMINAL de
# cada adjudicacion al promedio oficial de su propio ejercicio: con inflacion
# anual de tres digitos, una tasa unica no dice nada sobre lo que valia una
# adjudicacion de 2019. La serie en pesos constantes de 2024 (deflactor IPC)
# sigue siendo la que usan Theil y los cortes por era, y es otra cantidad.
# Mismo patron de User-Agent que el IPC.
try:
    url = ("https://apis.datos.gob.ar/series/api/series/"
           "?ids=168.1_T_CAMBIOR_D_0_0_26&collapse=year"
           "&collapse_aggregation=avg&format=csv&limit=1000")
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=60) as r:
        tc = pd.read_csv(io.BytesIO(r.read()))
    tc.columns = ["fecha", "tc_oficial_prom"]
    tc["anio"] = pd.to_datetime(tc["fecha"]).dt.year
    tc = tc[["anio", "tc_oficial_prom"]]
    tc.to_csv(RAW / "tipo_cambio_bna_anual.csv", index=False)
    tc.to_csv(PROC / "tipo_cambio_anual.csv", index=False)
    print(f"TC oficial: {tc['anio'].min()}-{tc['anio'].max()} OK "
          f"(2024 = {tc.loc[tc['anio'] == 2024, 'tc_oficial_prom'].iat[0]:.2f})")
except Exception as e:
    print(f"AVISO — TIPO DE CAMBIO NO DISPONIBLE ({e}); las cifras en dolares "
          "del manuscrito no se pueden regenerar.")

den.to_parquet(PROC / "denominators_provincia_anio.parquet", index=False)
cov.to_parquet(PROC / "covariates_provincia_anio.parquet", index=False)

print("\nStock de sociedades 2025 y legibilidad (NEA vs referencia):")
sel = cov[(cov["ejercicio"] == 2025)][["provincia", "stock_sociedades",
                                       "proveedores_sipro", "legibilidad_x1000", "nea"]]
print(sel.sort_values("legibilidad_x1000", ascending=False).to_string(index=False))
print("\ncovariables: filas", len(cov), "| itpp no nulo:", cov["itpp"].notna().sum(),
      "| alineado no nulo:", cov["alineado"].notna().sum())
print("OK -> data/processed/")
