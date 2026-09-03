"""
14 — Buyer-unit location check (data layer)
===========================================
Project: Who Sells to the State (2026_2). DeepSeek review point 4.2 remedy.

The garrison-state claim requires that NEA suppliers sell to units LOCATED in
the NEA, not merely to security-type buyers. Descripcion_UOC names the actual
buying unit and often carries a toponym ("Escuadrón 16 Clorinda"). This script
matches UOC names against a curated NEA gazetteer (VERIFICAR: hand list) and
computes both directions of the test:
  (a) awards to NEA-domiciled suppliers: share from NEA-located units;
  (b) awards issued by NEA-located units: supplier origin composition.

Input:  data/raw/comprar_adjudicaciones_2016_2026.csv
        data/processed/adjudicaciones_tipo.parquet
Output: data/processed/adjudicaciones_uoc.parquet (adds uoc_nea flag)
        tables/tab_uoc_localizacion_nea.csv

Usage:  python 14_uoc_location_check.py
"""
import re
import unicodedata
from pathlib import Path

import numpy as np
import pandas as pd

PROJECT = Path(__file__).parent
RAW = PROJECT / "data" / "raw"
PROC = PROJECT / "data" / "processed"
TAB = PROJECT / "tables"

NEA = {"Misiones", "Corrientes", "Chaco", "Formosa"}
METRO = {"CABA", "Buenos Aires"}

# Curated NEA toponyms found in unit names (VERIFICAR; ambiguous names that
# also exist in other provinces — Mercedes, Ituzaingó, Alvear — are EXCLUDED
# to keep the match conservative).
TOPONIMOS_NEA = [
    # Formosa
    "CLORINDA", "FORMOSA", "LAS LOMITAS", "INGENIERO JUAREZ", "PIRANE",
    "EL COLORADO", "LAGUNA BLANCA",
    # Chaco
    "RESISTENCIA", "SAENZ PENA", "CHARATA", "VILLA ANGELA", "CASTELLI",
    "CHACO",
    # Corrientes
    "CORRIENTES", "PASO DE LOS LIBRES", "SANTO TOME", "CURUZU CUATIA",
    "MONTE CASEROS", "GOYA", "VIRASORO", "LA CRUZ",
    # Misiones
    "POSADAS", "IGUAZU", "ELDORADO", "OBERA", "SAN JAVIER", "APOSTOLES",
    "BERNARDO DE IRIGOYEN", "MONTECARLO", "MISIONES",
]
# Units garrisoned in the NEA whose names carry no toponym (hand-curated from
# the top buying units of NEA suppliers; VERIFICAR garrison seats):
# Brigada de Monte III y 1ra División: Curuzú Cuatiá; Brigada de Monte XII:
# Posadas; Escuela Militar de Monte: Iguazú; Liceo Storni: Posadas;
# Esc. 47: Ituzaingó (Corrientes); Esc. 8 Alto Uruguay: Misiones;
# Bat. Ingenieros Anfibios 121: Santo Tomé.
UNIDADES_NEA = [
    r"BRIGADA DE MONTE", r"ESCUELA MILITAR DE MONTE",
    r"1RA DIVISION DE EJERCITO", r"LICEO NAVAL MILITAR ALMIRANTE STORNI",
    r"ESCUADRON 47 ITUZAINGO", r"ALTO URUGUAY",
    r"INGENIEROS ANFIBIOS 121",
    # data-entry variants and remaining NEA squadrons (VERIFICAR seats):
    r"PASOS DE LOS LIBRES", r"VIFORMOSA", r"SAN IGNACIO", r"BAJO PARAGUAY",
]
_PAT = re.compile(r"\b(" + "|".join(TOPONIMOS_NEA) + r")\b|" +
                  "|".join(UNIDADES_NEA))


def norm(s):
    s = "".join(c for c in unicodedata.normalize("NFD", str(s))
                if unicodedata.category(c) != "Mn")
    return s.upper()


raw = pd.read_csv(RAW / "comprar_adjudicaciones_2016_2026.csv", dtype=str,
                  usecols=["Documento_Contractual", "Descripcion_UOC"])
raw["uoc_nea"] = raw["Descripcion_UOC"].map(
    lambda s: bool(_PAT.search(norm(s))) if pd.notna(s) else False)
raw["uoc_toponimo"] = raw["Descripcion_UOC"].map(
    lambda s: (_PAT.search(norm(s)) or [None])[0] if pd.notna(s) else None)
uoc = raw.drop_duplicates("Documento_Contractual")

adj = pd.read_parquet(PROC / "adjudicaciones_tipo.parquet")
adj = adj.merge(uoc[["Documento_Contractual", "uoc_nea"]],
                left_on="doc_contractual", right_on="Documento_Contractual",
                how="left").drop(columns=["Documento_Contractual"])
adj["uoc_nea"] = adj["uoc_nea"].fillna(False)
adj.to_parquet(PROC / "adjudicaciones_uoc.parquet", index=False)

sub = adj[adj["es_nuevo"] & adj["provincia"].notna()].copy()
sub["region"] = np.select([sub["provincia"].isin(NEA), sub["provincia"].isin(METRO)],
                          ["NEA", "Metro"], default="Resto")

# (a) awards to NEA suppliers: share from NEA-located units, by buyer type
a = (sub[sub["region"] == "NEA"]
     .groupby(["tipo_organismo"])
     .agg(n=("uoc_nea", "size"), share_uoc_nea=("uoc_nea", "mean"))
     .reset_index().assign(direccion="proveedor_NEA->unidad"))

# (b) awards issued by NEA-located units: supplier origin composition
b = (sub[sub["uoc_nea"]]
     .groupby("region")
     .agg(n=("uoc_nea", "size"))
     .reset_index())
b["share"] = b["n"] / b["n"].sum()
b["direccion"] = "unidad_NEA->proveedor"

out = pd.concat([a.rename(columns={"tipo_organismo": "clave",
                                   "share_uoc_nea": "share"})
                 [["direccion", "clave", "n", "share"]],
                 b.rename(columns={"region": "clave"})
                 [["direccion", "clave", "n", "share"]]])
out.to_csv(TAB / "tab_uoc_localizacion_nea.csv", index=False)

total_nea = sub[sub["region"] == "NEA"]
print(f"Adjudicaciones a proveedores NEA: {len(total_nea)} | "
      f"desde unidades con toponimo NEA: {total_nea['uoc_nea'].mean():.1%}")
seg = total_nea[total_nea["tipo_organismo"] == "seguridad_defensa"]
print(f"  solo compradores seguridad_defensa: {seg['uoc_nea'].mean():.1%} "
      f"(n={len(seg)})")
print("\n(a) proveedor NEA -> ubicación de la unidad, por tipo:")
print(a[["tipo_organismo", "n", "share_uoc_nea"]].round(3).to_string(index=False))
print("\n(b) unidades NEA -> origen del proveedor:")
print(b[["region", "n", "share"]].round(3).to_string(index=False))
print("\nTop 15 UOC aún no localizadas entre las que compran a proveedores NEA (seguridad):")
raw2 = pd.read_csv(RAW / "comprar_adjudicaciones_2016_2026.csv", dtype=str,
                   usecols=["Documento_Contractual", "Descripcion_UOC"]) \
    .drop_duplicates("Documento_Contractual")
chk = (sub[(sub["region"] == "NEA") &
           (sub["tipo_organismo"] == "seguridad_defensa") & ~sub["uoc_nea"]]
       .merge(raw2, left_on="doc_contractual",
              right_on="Documento_Contractual", how="left"))
print(chk["Descripcion_UOC"].value_counts().head(15).to_string())
print("\nNota: matching por toponimo + unidades curadas (conservador); "
      "lo no matcheado queda como no-NEA -> (a) sigue siendo un piso.")
