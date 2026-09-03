"""
09 — Buyer typology: the demand side of the space
=================================================
Project: Who Sells to the State (2026_2). GAN V4/V5 remedy.

Classifies the 162 buying organisms into six types plus a territorial-
anchoring flag (organs with distributed territorial delegations: armed and
border forces, road agency, national parks, migrations). The flag is the
theoretically relevant distinction: territorially anchored demand does not
rotate with presidents and is the demand pole of the garrison sub-space.

Input:  data/processed/adjudicaciones_clean.parquet
Output: data/processed/adjudicaciones_tipo.parquet
        tables/tab_demanda_region_era.csv

Usage:  python 09_buyer_typology.py
"""
import re
import unicodedata
from pathlib import Path

import numpy as np
import pandas as pd

PROJECT = Path(__file__).parent
PROC = PROJECT / "data" / "processed"
TAB = PROJECT / "tables"

NEA = {"Misiones", "Corrientes", "Chaco", "Formosa"}
METRO = {"CABA", "Buenos Aires"}


def _norm(s):
    s = "".join(c for c in unicodedata.normalize("NFD", str(s))
                if unicodedata.category(c) != "Mn")
    return s.upper()


# v2 (28-08-2026, hand-check of the 162 labels): word-bounded CULTURA (the
# substring matched AGRICULTURA and VITIVINICULTURA); cultura_educacion moved
# ahead of salud_social (Educación under Capital Humano); force-attached
# agencies (Fabricaciones Militares, military pensions) to seguridad; water
# works to infraestructura; transport REGULATORS stay administracion (only the
# ministry and works go to infraestructura); Miguel Lillo and INTI to ciencia;
# Cáncer and Laboratorios Públicos to salud.
RULES = [
    ("seguridad_defensa", r"EJERCITO|ARMADA|FUERZA AEREA|GENDARMERIA|ESTADO MAYOR|"
                          r"DEFENSA|POLICIA|PREFECTURA|SEGURIDAD INTERIOR|"
                          r"MINISTERIO DE SEGURIDAD|PENITENCIARIO|INTELIGENCIA|"
                          r"FABRICACIONES MILITARES|PENSIONES MILITARES"),
    ("infraestructura", r"VIALIDAD|OBRAS PUBLICAS|INFRAESTRUCTURA|"
                        r"MINISTERIO DE TRANSPORTE|OBRAS HIDRICAS|SANEAMIENTO|"
                        r"AGUA Y SANEAMIENTO|FERROVIARI"),
    ("cultura_educacion", r"\bCULTURA\b|TEATRO|BIBLIOTECA|EDUCACION|MUSEO|CINE|"
                          r"AUDIOVISUAL|CERVANTES"),
    ("salud_social", r"HOSPITAL|SALUD|ANMAT|MALBRAN|ABLACION|COLONIA NACIONAL|"
                     r"REHABILITACION|DESARROLLO SOCIAL|SEGURIDAD SOCIAL|"
                     r"INSSJP|PAMI|MEDICAMENTOS|CAPITAL HUMANO|"
                     r"INSTITUTO NACIONAL DEL CANCER|LABORATORIOS PUBLICOS"),
    ("ciencia_universidad", r"UNIVERSIDAD|CONICET|ENERGIA ATOMICA|ESPACIALES|"
                            r"PESQUERO|INVESTIGACION|INTA\b|NUCLEAR|"
                            r"CIENCIA|TECNOLOGIA AGROPECUARIA|"
                            r"TECNOLOGIA INDUSTRIAL|MIGUEL LILLO"),
]
TERRITORIAL = (r"EJERCITO|ARMADA|FUERZA AEREA|GENDARMERIA|PREFECTURA|"
               r"VIALIDAD|PARQUES NACIONALES|MIGRACIONES")


def classify(name):
    n = _norm(name)
    for label, pat in RULES:
        if re.search(pat, n):
            return label
    return "administracion"


adj = pd.read_parquet(PROC / "adjudicaciones_clean.parquet")
orgs = adj["organismo"].dropna().unique()
mapping = {o: classify(o) for o in orgs}

# v3 (28-08-2026, full hand-check of the 162 labels): the name rules cannot
# reach organisms whose title carries no domain keyword, and they split one
# jurisdiction across two types when the label is written differently (350
# Labour, 311 Social Development). Corrections live in an explicit override
# table so the classification stays auditable rule by rule.
OVERRIDES = PROJECT / "data" / "raw" / "covariates" / "buyer_typology_overrides.csv"
ov = pd.read_csv(OVERRIDES)
missing = sorted(set(ov["organismo"]) - set(orgs))
if missing:
    raise SystemExit(f"override labels not found in the data: {missing}")
source = {o: "rule" for o in orgs}
for org, tipo in zip(ov["organismo"], ov["tipo_override"]):
    mapping[org] = tipo
    source[org] = "hand-check"
print(f"overrides applied: {len(ov)} labels, "
      f"{adj['organismo'].isin(ov['organismo']).sum()} adjudicaciones "
      f"({100 * adj['organismo'].isin(ov['organismo']).mean():.1f}%)")

adj["tipo_organismo"] = adj["organismo"].map(mapping)
adj["anclaje_territorial"] = adj["organismo"].map(
    lambda o: bool(re.search(TERRITORIAL, _norm(o))) if pd.notna(o) else False)
adj.to_parquet(PROC / "adjudicaciones_tipo.parquet", index=False)

# audit table: every organism label with its assigned type, territorial flag
# and volume — the reviewable record of the classification (review item 2.7)
audit = (adj.groupby("organismo")
         .agg(n_adj=("organismo", "size"),
              tipo=("tipo_organismo", "first"),
              anclaje_territorial=("anclaje_territorial", "first"))
         .sort_values(["tipo", "n_adj"], ascending=[True, False])
         .reset_index())
audit["asignacion"] = audit["organismo"].map(source)
audit.to_csv(TAB / "tab_buyer_typology_audit.csv", index=False)

# sanity
chk = (adj.groupby("tipo_organismo").size().sort_values(ascending=False))
print("Adjudicaciones por tipo de organismo:")
print(chk.to_string())
print("\nanclaje_territorial:", adj["anclaje_territorial"].mean().round(3),
      "de las adjudicaciones")
for probe, expect in [("Estado Mayor General del Ejercito", "seguridad_defensa"),
                      ("Dirección Nacional de Vialidad", "infraestructura"),
                      ("Hospital Nacional Profesor Alejandro Posadas", "salud_social"),
                      ("Comisión Nacional de Energía Atómica", "ciencia_universidad")]:
    got = [v for k, v in mapping.items() if probe.split()[-1] in k]
    print(f"  probe {probe[:40]:42s} -> {classify(probe)} (esperado {expect})")

# demand mix by region x era
sub = adj[adj["es_nuevo"] & adj["provincia"].notna()].copy()
sub["region"] = np.select([sub["provincia"].isin(NEA), sub["provincia"].isin(METRO)],
                          ["NEA", "Metro"], default="Resto")
mix = (sub.pivot_table(index=["region", "era"], columns="tipo_organismo",
                       values="cuit", aggfunc="size", fill_value=0))
mix = mix.div(mix.sum(axis=1), axis=0).round(3)
mix["share_territorial"] = (sub.groupby(["region", "era"])["anclaje_territorial"]
                            .mean().round(3))
mix.to_csv(TAB / "tab_demanda_region_era.csv")
print("\nMix de demanda por región y era (shares):")
print(mix.to_string())
print("OK -> data/processed/adjudicaciones_tipo.parquet, tables/")
