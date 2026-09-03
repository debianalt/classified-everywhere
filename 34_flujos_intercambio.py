"""34 — La geografía del intercambio: flujos unidad→localidad (29 ago 2026).

CAPA DE DATOS (regla del árbol: análisis en R). Geocodifica el domicilio
fiscal del proveedor (SIPRO -> georef) y une cada adjudicación con la sede
de su unidad compradora (gacetero de 33). Entrega coordenadas de ambas
puntas; la distancia y toda estadística se computan en R (script 35).

Caveat de diseño, declarado también en métodos y en el caption: la distancia
une la sede de la OFICINA compradora con el DOMICILIO FISCAL del proveedor —
la geometría administrativa de la relación, no el flete. La compra
centralizada en CABA y el registro fiscal centralizado empujan ambas puntas
del polo corporativo hacia la metrópoli, en la misma dirección que la
predicción del marco; se dice, no se esconde.

Salidas:
  data/processed/proveedores_geo.parquet     (cuit, localidad, lat, lon, capa)
  data/processed/flujos_adjudicacion.parquet (por adjudicación, ambas puntas)

Run: python 34_flujos_intercambio.py
"""
import io
import json
import math
import re
import unicodedata
from pathlib import Path

import numpy as np
import pandas as pd

PROJECT = Path(__file__).resolve().parent
RAW = PROJECT / "data" / "raw"
PROC = PROJECT / "data" / "processed"
WIN0, WIN1 = 2019, 2025


def norm_cuit(s):
    d = re.sub(r"\D", "", str(s))
    if len(d) == 11:
        return d
    # SPR (acuerdo marco): digito verificador duplicado en el bloque medio
    # (30-656088636-6 -> 30-65608863-6); reparacion canonica del script 01
    if len(d) == 12 and d[10] == d[11]:
        return d[:11]
    return np.nan


def norm(s):
    s = "".join(c for c in unicodedata.normalize("NFD", str(s))
                if unicodedata.category(c) != "Mn")
    return re.sub(r"\s+", " ", s).upper().strip()


# ── georef: localidades oficiales ────────────────────────────────────────────
locs = json.loads((RAW / "georef_localidades.json").read_text(encoding="utf-8"))
geo = {}
for l in locs:
    key = (norm(l["nombre"]), norm(l["provincia"]["nombre"]))
    geo.setdefault(key, (l["centroide"]["lat"], l["centroide"]["lon"]))

PROV_FIX = {"CABA": "CIUDAD AUTONOMA DE BUENOS AIRES",
            "CIUDAD DE BUENOS AIRES": "CIUDAD AUTONOMA DE BUENOS AIRES",
            "CAPITAL FEDERAL": "CIUDAD AUTONOMA DE BUENOS AIRES",
            "TIERRA DEL FUEGO":
            "TIERRA DEL FUEGO, ANTARTIDA E ISLAS DEL ATLANTICO SUR",
            "STGO. DEL ESTERO": "SANTIAGO DEL ESTERO",
            "SGO. DEL ESTERO": "SANTIAGO DEL ESTERO"}
CABA_LATLON = (-34.6075, -58.4371)

# capitales provinciales, para la variante "capital" tipeada por el proveedor
CAPITAL_OF = {
    "BUENOS AIRES": "LA PLATA", "CATAMARCA": "SAN FERNANDO DEL VALLE DE CATAMARCA",
    "CHACO": "RESISTENCIA", "CHUBUT": "RAWSON", "CORDOBA": "CORDOBA",
    "CORRIENTES": "CORRIENTES", "ENTRE RIOS": "PARANA", "FORMOSA": "FORMOSA",
    "JUJUY": "SAN SALVADOR DE JUJUY", "LA PAMPA": "SANTA ROSA",
    "LA RIOJA": "LA RIOJA", "MENDOZA": "MENDOZA", "MISIONES": "POSADAS",
    "NEUQUEN": "NEUQUEN", "RIO NEGRO": "VIEDMA", "SALTA": "SALTA",
    "SAN JUAN": "SAN JUAN", "SAN LUIS": "SAN LUIS",
    "SANTA CRUZ": "RIO GALLEGOS", "SANTA FE": "SANTA FE",
    "SANTIAGO DEL ESTERO": "SANTIAGO DEL ESTERO",
    "TIERRA DEL FUEGO, ANTARTIDA E ISLAS DEL ATLANTICO SUR": "USHUAIA",
    "TUCUMAN": "SAN MIGUEL DE TUCUMAN"}

# normalizacion de variantes tipeadas frecuentes (texto libre de SIPRO)
ALIAS = {
    "SAN FDO DEL VALLE DE CATAMARCA": "SAN FERNANDO DEL VALLE DE CATAMARCA",
    "S F DEL VALLE DE CATAMARCA": "SAN FERNANDO DEL VALLE DE CATAMARCA",
    "SANTIAGO DEL ESTERO CAPITAL - SANTIAGO DEL ESTERO": "SANTIAGO DEL ESTERO",
    "SANTIAGO DEL ESTERO CAPITAL": "SANTIAGO DEL ESTERO",
    "SALTA - ARGENTINA": "SALTA", "SALTA CAPITAL": "SALTA",
    "CORDOBA CAPITAL": "CORDOBA", "MENDOZA CAPITAL": "MENDOZA",
    "SAN SALVADOR DE JUJUY - JUJUY": "SAN SALVADOR DE JUJUY",
    "SS DE JUJUY": "SAN SALVADOR DE JUJUY",
    "CIUDAD DE CORRIENTES": "CORRIENTES",
    "MAR DEL PLATA - BUENOS AIRES": "MAR DEL PLATA",
    "CDAD. AUTONOMA DE BUENOS AIRES": "CABA",
    "CIUDAD AUTONOMA DE BUENOS AIRES": "CABA",
}


def locate(localidad, provincia):
    """(lat, lon, capa) para la localidad declarada, o None."""
    lp, pp = norm(localidad), PROV_FIX.get(norm(provincia), norm(provincia))
    lp = ALIAS.get(lp, lp)
    if lp in ("CABA", "CAPITAL FEDERAL") or pp == "CIUDAD AUTONOMA DE BUENOS AIRES":
        return (*CABA_LATLON, "caba")
    if lp in ("CAPITAL", "") and pp in CAPITAL_OF:
        hit = geo.get((CAPITAL_OF[pp], pp))
        return (*hit, "capital_regla") if hit else None
    hit = geo.get((lp, pp))
    if hit:
        return (*hit, "exacto")
    # nombre unico a nivel pais
    cands = [(k, v) for k, v in geo.items() if k[0] == lp]
    if len(cands) == 1:
        return (*cands[0][1], "nombre_unico")
    return None


# ── proveedores: geocodificacion ─────────────────────────────────────────────
sip = pd.read_csv(RAW / "sipro_proveedores.csv", dtype=str,
                  usecols=["CUIT_NIT", "Localidad", "Provincia"])
sip["cuit"] = sip["CUIT_NIT"].map(norm_cuit)
sip = sip.drop_duplicates("cuit")
res = sip.apply(lambda r: locate(r["Localidad"], r["Provincia"]), axis=1)
sip["lat"] = res.map(lambda x: x[0] if x else None)
sip["lon"] = res.map(lambda x: x[1] if x else None)
sip["capa"] = res.map(lambda x: x[2] if x else "sin_resolver")
cov = sip["lat"].notna().mean()
print(f"proveedores: {len(sip):,} | geocodificados: {cov:.1%}")
print(sip["capa"].value_counts().to_string())
sip[["cuit", "Localidad", "Provincia", "lat", "lon", "capa"]].rename(
    columns={"Localidad": "localidad", "Provincia": "provincia"}
).to_parquet(PROC / "proveedores_geo.parquet", index=False)

# ── adjudicaciones: join de las dos puntas ───────────────────────────────────
adj = pd.read_csv(RAW / "comprar_adjudicaciones_2016_2026.csv", dtype=str,
                  usecols=["Documento_Contractual", "Descripcion_UOC",
                           "Descripcion_SAF", "Ejercicio", "CUIT", "Tipo"])
adj = adj[adj["Ejercicio"].astype(float).between(WIN0, WIN1)].copy()
# mismo universo que todo el pipeline: solo adjudicaciones nuevas (es_nuevo
# del script 01), no renovaciones ni modificaciones
NEW_AWARD = {"Original", "SPR", "Siguiente Orden de Mérito"}  # = script 01
adj = adj[adj["Tipo"].isin(NEW_AWARD)]
adj["cuit"] = adj["CUIT"].map(norm_cuit)
adj = adj.dropna(subset=["cuit"])

gaz = pd.read_csv(RAW / "uoc_gazetteer.csv")
gaz["es_caba"] = gaz["localidad"].fillna("").map(norm) == "CABA"
gaz = gaz[(gaz["lat"].notna()) & ((gaz["verificado"] == 1) | gaz["es_caba"])]
gaz = gaz[["uoc", "saf", "tipo_organismo", "localidad", "lat", "lon"]].rename(
    columns={"localidad": "loc_uoc", "lat": "lat_u", "lon": "lon_u"})

n0 = len(adj)
adj = adj.merge(gaz, left_on=["Descripcion_UOC", "Descripcion_SAF"],
                right_on=["uoc", "saf"], how="inner")
n1 = len(adj)
adj = adj.merge(sip[["cuit", "Localidad", "lat", "lon"]].rename(
    columns={"Localidad": "loc_prov", "lat": "lat_p", "lon": "lon_p"}),
    on="cuit", how="inner")
adj = adj[adj["lat_p"].notna()]
n2 = len(adj)
print(f"\nadjudicaciones ventana: {n0:,} | con sede de unidad: {n1:,} "
      f"({n1 / n0:.1%}) | con ambas puntas: {n2:,} ({n2 / n0:.1%})")


adj[["Documento_Contractual", "Ejercicio", "cuit", "uoc", "saf",
     "tipo_organismo", "loc_uoc", "loc_prov", "lat_u", "lon_u",
     "lat_p", "lon_p"]].to_parquet(
    PROC / "flujos_adjudicacion.parquet", index=False)
print("-> flujos_adjudicacion.parquet (coordenadas de ambas puntas; la")
print("   distancia, las estadisticas y la figura se computan en R, script 35)")
