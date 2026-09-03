"""
01 — Build supplier panel
=========================
Project: Who Sells to the State (2026_2)

Joins COMPR.AR adjudicaciones (2016-2026) with SIPRO (province of domicile)
and the ARCA national company registry (tipo societario, CLAE activity,
fiscal-domicile province), keyed on normalised CUIT.

Input:  data/raw/comprar_adjudicaciones_2016_2026.csv
        data/raw/sipro_proveedores.csv
        ../../gee/arca/data/registro-nacional-sociedades-20260223.csv
Output: data/processed/adjudicaciones_clean.parquet
        data/processed/supplier_master.parquet
        data/processed/province_year_rubro.parquet

Usage:  python 01_build_supplier_panel.py
"""
import re
import unicodedata
from pathlib import Path

import numpy as np
import pandas as pd

PROJECT = Path(__file__).parent
RAW = PROJECT / "data" / "raw"
PROC = PROJECT / "data" / "processed"
PROC.mkdir(parents=True, exist_ok=True)
ARCA_REGISTRY = PROJECT.parent.parent / "gee" / "arca" / "data" / "registro-nacional-sociedades-20260223.csv"

NEA = {"Misiones", "Corrientes", "Chaco", "Formosa"}

ERA = {**{y: "macri" for y in range(2016, 2020)},
       **{y: "fernandez" for y in range(2020, 2024)},
       **{y: "milei" for y in range(2024, 2027)}}

# Award types: new awards vs modifications of existing contracts
NEW_AWARD = {"Original", "SPR", "Siguiente Orden de Mérito"}

PROV_CANON = [
    "Buenos Aires", "CABA", "Catamarca", "Chaco", "Chubut", "Córdoba",
    "Corrientes", "Entre Ríos", "Formosa", "Jujuy", "La Pampa", "La Rioja",
    "Mendoza", "Misiones", "Neuquén", "Río Negro", "Salta", "San Juan",
    "San Luis", "Santa Cruz", "Santa Fe", "Santiago del Estero",
    "Tierra del Fuego", "Tucumán",
]


def _strip_accents(s: str) -> str:
    return "".join(c for c in unicodedata.normalize("NFD", s) if unicodedata.category(c) != "Mn")


_PROV_LOOKUP = {_strip_accents(p).upper(): p for p in PROV_CANON}
_PROV_LOOKUP.update({
    "CIUDAD AUTONOMA DE BUENOS AIRES": "CABA",
    "CIUDAD AUTONOMA BUENOS AIRES": "CABA",
    "CIUDAD DE BUENOS AIRES": "CABA",
    "CAPITAL FEDERAL": "CABA",
    "TIERRA DEL FUEGO, ANTARTIDA E ISLAS DEL ATLANTICO SUR": "Tierra del Fuego",
    "ENTRE RIOS": "Entre Ríos",
    "RIO NEGRO": "Río Negro",
})


def norm_provincia(s):
    if pd.isna(s) or not str(s).strip():
        return np.nan
    return _PROV_LOOKUP.get(_strip_accents(str(s).strip()).upper(), np.nan)


def norm_cuit(s):
    d = re.sub(r"\D", "", str(s))
    if len(d) == 11:
        return d
    # SPR (acuerdo marco) rows duplicate the check digit inside the middle
    # block (30-656088636-6 -> 30-65608863-6); 99.96% pass mod-11 after repair
    if len(d) == 12 and d[10] == d[11]:
        return d[:11]
    return np.nan


def split_rubros(s):
    if pd.isna(s):
        return []
    return [r.strip() for r in str(s).split(";") if r.strip()]


# ── Adjudicaciones ───────────────────────────────────────────────────────────
adj = pd.read_csv(RAW / "comprar_adjudicaciones_2016_2026.csv", dtype=str)
n0 = len(adj)
adj = adj.dropna(subset=["Ejercicio", "CUIT"])
adj["ejercicio"] = adj["Ejercicio"].astype(int)
adj["era"] = adj["ejercicio"].map(ERA)
adj["cuit"] = adj["CUIT"].map(norm_cuit)
adj = adj.dropna(subset=["cuit"])
adj["monto"] = pd.to_numeric(adj["Monto"], errors="coerce")
adj["moneda"] = adj["Moneda"].map(
    lambda m: "ARS" if m == "Peso Argentino" else ("USD" if m == "Dolar Estadounidense" else "OTRA"))
adj["es_nuevo"] = adj["Tipo"].isin(NEW_AWARD)
adj["procedimiento"] = adj["Tipo_de_Procedimiento"]
adj["rubros_lista"] = adj["Rubros"].map(split_rubros)
adj["rubro_principal"] = adj["rubros_lista"].map(lambda xs: xs[0] if xs else np.nan)
adj = adj.rename(columns={
    "Descripcion_SAF": "organismo", "Descripcion_Proveedor": "proveedor",
    "Numero_Proceso": "proceso", "Documento_Contractual": "doc_contractual",
    "Modalidad": "modalidad"})
adj = adj[["proceso", "doc_contractual", "organismo", "procedimiento", "modalidad",
           "ejercicio", "era", "cuit", "proveedor", "rubro_principal", "rubros_lista",
           "monto", "moneda", "es_nuevo"]]
print(f"adjudicaciones: {n0} -> {len(adj)} filas | CUITs unicos: {adj['cuit'].nunique()}")

# ── SIPRO ────────────────────────────────────────────────────────────────────
sip = pd.read_csv(RAW / "sipro_proveedores.csv", dtype=str)
sip.columns = [c.strip().replace(" ", "") for c in sip.columns]
sip = sip[~sip["Razon_Social"].str.contains("Prueba", case=False, na=False)]
sip["cuit"] = sip["CUIT_NIT"].map(norm_cuit)
sip = sip.dropna(subset=["cuit"])
sip["provincia_sipro"] = sip["Provincia"].map(norm_provincia)
sip["fecha_preinscripcion"] = pd.to_datetime(
    sip["Fecha_de_PreInscripcion"].str.extract(r"(\d{2}/\d{2}/\d{4})")[0],
    format="%d/%m/%Y", errors="coerce")
sip = (sip.sort_values("fecha_preinscripcion")
          .drop_duplicates("cuit", keep="last")
          .rename(columns={"Tipo_de_Personeria": "personeria",
                           "Razon_Social": "razon_social_sipro"}))
sip = sip[["cuit", "razon_social_sipro", "personeria", "provincia_sipro",
           "fecha_preinscripcion"]]
print(f"sipro: {len(sip)} proveedores | sin provincia: {sip['provincia_sipro'].isna().sum()}")

# ── ARCA registry slice (only CUITs we care about) ───────────────────────────
targets = set(adj["cuit"]) | set(sip["cuit"])
chunks = []
for chunk in pd.read_csv(
        ARCA_REGISTRY, dtype=str, chunksize=500_000,
        usecols=["cuit", "tipo_societario", "fecha_hora_contrato_social",
                 "dom_fiscal_provincia", "actividad_codigo",
                 "actividad_descripcion", "actividad_orden"],
        on_bad_lines="skip"):
    chunk["cuit"] = chunk["cuit"].map(norm_cuit)
    chunks.append(chunk[chunk["cuit"].isin(targets)])
arca = pd.concat(chunks, ignore_index=True)
arca["actividad_orden"] = pd.to_numeric(arca["actividad_orden"], errors="coerce")
arca = (arca.sort_values("actividad_orden")
            .drop_duplicates("cuit", keep="first")
            .rename(columns={"dom_fiscal_provincia": "provincia_arca_raw"}))
arca["provincia_arca"] = arca["provincia_arca_raw"].map(norm_provincia)
arca["anio_contrato_social"] = pd.to_numeric(
    arca["fecha_hora_contrato_social"].str[:4], errors="coerce")
arca = arca[["cuit", "tipo_societario", "anio_contrato_social",
             "provincia_arca", "actividad_codigo", "actividad_descripcion"]]
print(f"arca slice: {len(arca)} CUITs matcheados de {len(targets)} buscados")

# ── Supplier master (1 row per CUIT ever adjudicated or registered) ──────────
awards = (adj[adj["es_nuevo"]]
          .groupby("cuit")
          .agg(n_adjudicaciones=("cuit", "size"),
               primer_ejercicio=("ejercicio", "min"),
               ultimo_ejercicio=("ejercicio", "max"),
               monto_ars_total=("monto", lambda s: s[adj.loc[s.index, "moneda"] == "ARS"].sum()),
               n_organismos=("organismo", "nunique"),
               rubro_modal=("rubro_principal", lambda s: s.mode().iat[0] if not s.mode().empty else np.nan),
               share_directa=("procedimiento", lambda s: (s == "Contratación Directa").mean()))
          .reset_index())
master = sip.merge(awards, on="cuit", how="outer").merge(arca, on="cuit", how="left")
master["provincia"] = master["provincia_sipro"].fillna(master["provincia_arca"])
master["nea"] = master["provincia"].isin(NEA)
master["adjudicado"] = master["n_adjudicaciones"].notna()
print(f"master: {len(master)} CUITs | adjudicados: {master['adjudicado'].sum()}")
print(f"  provincia asignada: {master['provincia'].notna().mean():.1%} "
      f"(sipro {master['provincia_sipro'].notna().mean():.1%}, "
      f"+arca rescata {((master['provincia_sipro'].isna()) & (master['provincia_arca'].notna())).sum()})")

adj_cuits = set(adj["cuit"])
print(f"match adjudicaciones->SIPRO: {len(adj_cuits & set(sip['cuit'])) / len(adj_cuits):.1%}")
print(f"match adjudicaciones->ARCA:  {len(adj_cuits & set(arca['cuit'])) / len(adj_cuits):.1%}")

# ── Adjudicaciones enriquecidas + agregado provincia×año×rubro ───────────────
adj = adj.merge(master[["cuit", "provincia", "nea", "personeria"]], on="cuit", how="left")
print(f"adjudicaciones con provincia: {adj['provincia'].notna().mean():.1%}")

pyr = (adj[adj["es_nuevo"] & adj["provincia"].notna()]
       .groupby(["provincia", "ejercicio", "rubro_principal"])
       .agg(n_adj=("cuit", "size"), n_proveedores=("cuit", "nunique"),
            monto_ars=("monto", lambda s: s[adj.loc[s.index, "moneda"] == "ARS"].sum()))
       .reset_index())

adj.drop(columns=["rubros_lista"]).to_parquet(PROC / "adjudicaciones_clean.parquet", index=False)
master.to_parquet(PROC / "supplier_master.parquet", index=False)
pyr.to_parquet(PROC / "province_year_rubro.parquet", index=False)

# ── Sanity: NEA cells ────────────────────────────────────────────────────────
print("\nNEA - proveedores adjudicados por provincia:")
print(master[master["adjudicado"]].groupby("provincia").size()
      .reindex(sorted(NEA)).to_string())
print("\nNEA - adjudicaciones nuevas por provincia x era:")
print(adj[adj["es_nuevo"] & adj["nea"]].pivot_table(
    index="provincia", columns="era", values="cuit", aggfunc="size", fill_value=0)
    .reindex(sorted(NEA)).to_string())
print("\ncelda provincia x año x rubro: filas =", len(pyr))
print("OK -> data/processed/")
