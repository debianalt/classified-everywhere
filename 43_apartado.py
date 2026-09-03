"""
43 — The legal ground of the exception
======================================
Project: Who Sells to the State (2026_2)

COMPR.AR records, for every award that departs from open competition, the legal
ground the buying organ invoked. The field is `Apartado_Directa` and the
pipeline never carried it. Two families of ground do very different symbolic
work:

  cuantitativo  the award is small enough that no competition is required.
                An arithmetic, impersonal ground: the threshold decides.
  cualitativo   the state declares this supplier the only possible one
                (exclusividad) or its competence the one required
                (especialidad). Here the state names, and the naming is the
                ground.

This script normalises the field, maps the older lettered nomenclature onto the
numbered one, and writes an award-level table keyed on the contractual
document. It applies NO analysis window: the window (2019-2025, new awards,
supplier with a province) lives once in theme_house.R, and the flag a supplier
carries is derived there, so the two layers cannot drift.

Input:  data/raw/comprar_adjudicaciones_2016_2026.csv
Output: data/processed/adjudicaciones_apartado.parquet
        tables/tab_apartado_crosswalk.csv   (for hand review)

Usage:  python 43_apartado.py
"""
import sys
from pathlib import Path

import pandas as pd

PROJECT = Path(__file__).parent
RAW = PROJECT / "data" / "raw"
PROC = PROJECT / "data" / "processed"
TAB = PROJECT / "tables"
TAB.mkdir(parents=True, exist_ok=True)

# The nineteen distinct grounds in the record, mapped to the numbered
# nomenclature and to a family. The lettered variants are an older vocabulary
# that survives in 235 awards inside the window; they are matched on their text
# and carry the same substantive ground as their numbered twin. No legal
# provenance is asserted here beyond what the strings themselves say.
CROSSWALK = {
    "Apartado 1: Compulsa Abreviada Por Monto": (1, "cuantitativo"),
    "Apartado a: Compulsa Abreviada por monto mayor a M 150 y menor a M 1.300": (1, "cuantitativo"),
    "Apartado f: Compulsa Abreviada por monto hasta M 150": (1, "cuantitativo"),
    "Apartado 2: Adjudicación Simple por Especialidad": (2, "cualitativo"),
    "Apartado g: Adjudicación Simple por especialidad": (2, "cualitativo"),
    "Apartado 3: Adjudicación Simple por Exclusividad": (3, "cualitativo"),
    "Apartado h: Adjudicación Simple por exclusividad": (3, "cualitativo"),
    "Apartado 4: Compulsa Abreviada por Licitación o Concurso Desierto o Fracasado": (4, "otros"),
    "Apartado 5: Compulsa Abreviada por Urgencia": (5, "otros"),
    "Apartado 5: Adjudicación Simple por Emergencia": (5, "otros"),
    "Apartado d: Compulsa Abreviada por urgencia": (5, "otros"),
    "Apartado 7: Adjudicación Simple por Desarme  Traslado o Examen Previo": (7, "otros"),
    "Apartado k: Adjudicación Simple por desarme  traslado o examen previo": (7, "otros"),
    "Apartado 8: Adjudicación Simple Interadministrativa": (8, "interadministrativo"),
    "Apartado l: Adjudicación Simple interadministrativa": (8, "interadministrativo"),
    "Apartado 9: Adjudicación Simple con Universidades Nacionales": (9, "interadministrativo"),
    "Apartado 10: Adjudicación Simple con Efectores de Desarrollo Local y Economía Social": (10, "otros"),
    "Apartado 10: Compulsa Abreviada con Efectores de Desarrollo Local y Economía Social": (10, "otros"),
    "Apartado 11: Adjudicación Simple por Locación de Inmuebles": (11, "otros"),
}

# What the canonical window must return once theme_house.R applies it. These
# are the numbers the manuscript reports; the script fails loudly if the source
# snapshot ever changes underneath them.
EXPECTED = {"con_fundamento": 57304, "cualitativo": 3758, "cuantitativo": 45932}

adj = pd.read_csv(RAW / "comprar_adjudicaciones_2016_2026.csv", dtype=str)
n0 = len(adj)

unseen = set(adj["Apartado_Directa"].dropna().unique()) - set(CROSSWALK)
if unseen:
    sys.exit("apartados sin mapear en CROSSWALK:\n  " + "\n  ".join(sorted(unseen)))

ap = adj[["Documento_Contractual", "Apartado_Directa"]].copy()
ap = ap.dropna(subset=["Documento_Contractual"])
ap["apartado_num"] = ap["Apartado_Directa"].map(
    lambda s: CROSSWALK[s][0] if isinstance(s, str) else pd.NA).astype("Int64")
ap["familia"] = ap["Apartado_Directa"].map(
    lambda s: CROSSWALK[s][1] if isinstance(s, str) else "competitivo")
ap = ap.rename(columns={"Documento_Contractual": "doc_contractual",
                        "Apartado_Directa": "apartado_raw"})

# One contractual document can appear on several rows (one per item awarded);
# the ground belongs to the document, so the table is unique on it. Guard
# against a document carrying two different grounds before collapsing.
mixed = ap.groupby("doc_contractual")["apartado_num"].nunique(dropna=False)
if (mixed > 1).any():
    sys.exit(f"{int((mixed > 1).sum())} documentos con más de un apartado")
ap = ap.drop_duplicates("doc_contractual").reset_index(drop=True)

ap.to_parquet(PROC / "adjudicaciones_apartado.parquet", index=False)

cw = (pd.DataFrame([(k, v[0], v[1]) for k, v in CROSSWALK.items()],
                   columns=["apartado_raw", "apartado_num", "familia"])
      .merge(adj["Apartado_Directa"].value_counts().rename("n_filas"),
             left_on="apartado_raw", right_index=True, how="left")
      .sort_values(["familia", "apartado_num", "apartado_raw"]))
cw.to_csv(TAB / "tab_apartado_crosswalk.csv", index=False)

print(f"adjudicaciones: {n0} filas -> {len(ap)} documentos contractuales únicos")
print(f"con fundamento de excepción: {(ap['familia'] != 'competitivo').sum()}")
print("\npor familia (documentos, registro completo):")
print(ap["familia"].value_counts().to_string())

# ── Audit against the canonical window ───────────────────────────────────────
# Reproduces theme_house.R's filter to check the join and the counts the
# manuscript reports. The parquet written above is NOT windowed.
tipo = pd.read_parquet(PROC / "adjudicaciones_tipo.parquet")
w = tipo[(tipo["ejercicio"].between(2019, 2025)) & tipo["es_nuevo"]
         & tipo["provincia"].notna()]
m = w.merge(ap, on="doc_contractual", how="left")
# one award in the window carries no contractual document at all, so it has no
# ground to carry either; it joins as competitive rather than as missing
sin_doc = int(w["doc_contractual"].isna().sum())
con_doc = m[m["doc_contractual"].notna()]
match = con_doc["familia"].notna().mean()
m["familia"] = m["familia"].fillna("competitivo")
print(f"\nventana canónica: {len(w)} adjudicaciones, {w['cuit'].nunique()} proveedores")
print(f"adjudicaciones sin documento contractual: {sin_doc}")
print(f"match por doc_contractual: {match:.6f}")
if match < 1.0:
    sys.exit("el join por doc_contractual no cubre la ventana")

got = {"con_fundamento": int((m["familia"] != "competitivo").sum()),
       "cualitativo": int((m["familia"] == "cualitativo").sum()),
       "cuantitativo": int((m["familia"] == "cuantitativo").sum())}
for k, v in EXPECTED.items():
    flag = "OK" if got[k] == v else f"ESPERADO {v}"
    print(f"  {k:16s} {got[k]:7d}  {flag}")
if got != EXPECTED:
    sys.exit("los conteos de la ventana se movieron respecto del snapshot")

print("\nfamilia por año (share de adjudicaciones con fundamento):")
g = m[m["familia"] != "competitivo"]
print((g.pivot_table(index="ejercicio", columns="familia", values="cuit",
                     aggfunc="size", fill_value=0)
       .pipe(lambda d: (100 * d.div(d.sum(axis=1), axis=0)).round(1)).to_string())
      )
print("\nproveedores consagrados alguna vez (fundamento cualitativo):")
cons = m.groupby("cuit")["familia"].apply(lambda s: (s == "cualitativo").any())
print(f"  {int(cons.sum())} de {len(cons)} = {100 * cons.mean():.1f}%")
