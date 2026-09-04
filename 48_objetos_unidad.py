# 48 — A year in the ledger of one anchored unit (round 35, 4 Sep 2026)
#
# Data layer. The register carries the object of every process in the state's
# own words (Objeto_del_Proceso, comprar_convocatorias_2016_2026.csv), and it
# joins to every award of the window. This script describes what ONE anchored
# purchasing unit of the north-east buys, from whom, and on what ground, so
# that section 6 can put the garrison door on the ground with the state's
# words instead of a median in kilometres.
#
# The unit is chosen by a declared rule, not by eye: among the anchored units
# seated in the four north-eastern provinces with at least 50 awards in the
# window, the one at the median of the award count. Suppliers are counted,
# never named.
#
# Outputs (tables/):
#   tab_unidad_mediana_eleccion.csv  every candidate unit with its award count
#   tab_unidad_mediana_resumen.csv   the chosen unit, year by year: awards,
#                                    distinct suppliers, share to natural
#                                    persons, share of local suppliers, share
#                                    by family of the ground of exception
#   tab_unidad_mediana_objetos.csv   its objects of purchase, normalised and
#                                    grouped by family, with counts
#
# Counts and shares only; no statistic that belongs to the analysis layer.
# Run: python 48_objetos_unidad.py

import re
import unicodedata
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parent
RAW, PROC, TAB = ROOT / "data" / "raw", ROOT / "data" / "processed", ROOT / "tables"
WIN0, WIN1 = 2019, 2025
NEA = ["Misiones", "Corrientes", "Chaco", "Formosa"]
MIN_AWARDS = 50

adj = pd.read_parquet(PROC / "adjudicaciones_tipo.parquet")
adj = adj[adj.es_nuevo & adj.ejercicio.between(WIN0, WIN1)].copy()
flu = pd.read_parquet(PROC / "flujos_adjudicacion.parquet",
                      columns=["Documento_Contractual", "uoc", "loc_uoc", "loc_prov"])
gaz = pd.read_csv(RAW / "uoc_gazetteer.csv", encoding="utf-8")
conv = pd.read_csv(RAW / "comprar_convocatorias_2016_2026.csv", sep=None, engine="python",
                   encoding="utf-8", encoding_errors="replace",
                   usecols=["Numero_Proceso", "Objeto_del_Proceso"]).drop_duplicates("Numero_Proceso")
apa = pd.read_parquet(PROC / "adjudicaciones_apartado.parquet", columns=["doc_contractual", "familia"])

a = (adj.merge(flu, left_on="doc_contractual", right_on="Documento_Contractual", how="left")
        .merge(gaz[["uoc", "provincia", "localidad"]].rename(
            columns={"provincia": "prov_uoc", "localidad": "loc_gaz"}), on="uoc", how="left")
        .merge(conv, left_on="proceso", right_on="Numero_Proceso", how="left")
        .merge(apa, on="doc_contractual", how="left"))

# ── the rule ─────────────────────────────────────────────────────────────────
cand = (a[a.anclaje_territorial & a.prov_uoc.isin(NEA)]
        .groupby(["uoc", "prov_uoc", "loc_gaz", "tipo_organismo"]).size()
        .reset_index(name="n_adj").query("n_adj >= @MIN_AWARDS")
        .sort_values("n_adj", ascending=False).reset_index(drop=True))
cand["rango"] = range(1, len(cand) + 1)
med = cand.iloc[len(cand) // 2]
garr = cand[cand.tipo_organismo == "seguridad_defensa"].reset_index(drop=True)
med_g = garr.iloc[len(garr) // 2]
cand["elegida"] = cand.uoc.map({med.uoc: "median anchored unit", med_g.uoc: "median garrison"}).fillna("")
cand.to_csv(TAB / "tab_unidad_mediana_eleccion.csv", index=False, encoding="utf-8")
print(f"{len(cand)} anchored units in the north-east with >= {MIN_AWARDS} awards; "
      f"median unit: {med.uoc} ({med.loc_gaz}, {med.prov_uoc}), {med.n_adj} awards; "
      f"median garrison of {len(garr)}: {med_g.uoc} ({med_g.loc_gaz}, {med_g.prov_uoc}), {med_g.n_adj} awards")

def describe(unit_uoc, tag):
    u = a[a.uoc == unit_uoc].copy()
    u["persona_fisica"] = u.personeria.astype(str).str.contains("Fisica", case=False)
    def key(x):
        x = unicodedata.normalize("NFKD", str(x)).encode("ascii", "ignore").decode().upper().strip()
        return re.sub(r"\s+", " ", x)
    # same locality, compared without case or accents: the register writes
    # "FORMOSA" and "Formosa" for the same town
    u["local"] = u.loc_prov.notna() & (u.loc_prov.map(key) == u.loc_uoc.map(key))
    u["loc_prov"] = u.loc_prov.map(lambda x: key(x) if pd.notna(x) else x)
    u["familia"] = u.familia.fillna("competitivo")

    # ── year by year ─────────────────────────────────────────────────────────────
    res = (u.groupby("ejercicio")
            .agg(adjudicaciones=("doc_contractual", "size"),
                 proveedores=("cuit", "nunique"),
                 pct_personas_fisicas=("persona_fisica", lambda s: round(100 * s.mean(), 1)),
                 pct_proveedores_locales=("local", lambda s: round(100 * s.mean(), 1)),
                 pct_directa=("procedimiento", lambda s: round(100 * s.astype(str).str.contains("Directa", case=False).mean(), 1)))
            .reset_index())
    fam = (u.groupby(["ejercicio", "familia"]).size().unstack(fill_value=0))
    fam = (100 * fam.div(fam.sum(axis=1), axis=0)).round(1).add_prefix("pct_")
    res = res.merge(fam, left_on="ejercicio", right_index=True, how="left")
    res.to_csv(TAB / f"tab_unidad_{tag}_resumen.csv", index=False, encoding="utf-8")
    print(res.to_string(index=False))

    # ── the objects, in the state's words ───────────────────────────────────────
    def norm(s):
        s = unicodedata.normalize("NFKD", str(s)).encode("ascii", "ignore").decode().lower()
        s = re.sub(r"[^a-z0-9 ]+", " ", s)
        s = re.sub(r"^(adquisicion|adquirir|compra|contratacion|contratar|provision|servicio|"
                   r"reparacion|renovacion|alquiler)( de| del| de la| de los| de las)?\s+", "", s)
        return re.sub(r"\s+", " ", s).strip()

    FAMILIES = [
        ("food", r"vivere|aliment|carne|pan|panif|lacteo|verdur|fruta|racion|comida|bebida|yerba|azucar"),
        ("fuel", r"combust|gasoil|nafta|lubric|gas\b|garrafa"),
        ("cleaning", r"limpieza|higien|desinfec|lavander"),
        ("stationery and printing", r"libreria|papel|impres|toner|cartucho|formular"),
        ("spare parts and tyres", r"repuesto|neumat|cubierta|bateria|filtro|accesorio"),
        ("maintenance and repair", r"mantenimiento|reparac|refaccion|pintura|electric|plomer|albanil"),
        ("vehicles and transport", r"vehicul|camion|transporte|flete|pasaje"),
        ("clothing and equipment", r"indument|uniform|calzado|bota|equipo|equipam|carpa"),
        ("computing and communications", r"informat|computad|notebook|impresora|software|internet|telefon|comunic"),
        ("health", r"medic|farmac|odontol|insumo hospit|sanit"),
        ("construction materials", r"material|cemento|hierro|madera|chapa|arena|ladrillo|construc"),
        ("services", r"servicio|seguro|capacit|catering|hoteler|evento|seguridad|vigilan"),
    ]
    def family(s):
        for name, pat in FAMILIES:
            if re.search(pat, s):
                return name
        return "other"

    u["objeto_norm"] = u.Objeto_del_Proceso.map(norm)
    u["familia_objeto"] = u.objeto_norm.map(family)
    obj = (u.groupby(["familia_objeto", "objeto_norm"])
            .agg(adjudicaciones=("doc_contractual", "size"), proveedores=("cuit", "nunique"),
                 pct_personas_fisicas=("persona_fisica", lambda s: round(100 * s.mean(), 1)))
            .reset_index().sort_values(["familia_objeto", "adjudicaciones"], ascending=[True, False]))
    obj.to_csv(TAB / f"tab_unidad_{tag}_objetos.csv", index=False, encoding="utf-8")
    famtab = (u.groupby("familia_objeto").agg(adjudicaciones=("doc_contractual", "size"),
                                              proveedores=("cuit", "nunique"))
               .assign(pct=lambda d: (100 * d.adjudicaciones / d.adjudicaciones.sum()).round(1))
               .sort_values("adjudicaciones", ascending=False))
    print("\nFamilies of purchase, whole window:\n", famtab.to_string())
    print("\nTop objects:\n", obj.sort_values("adjudicaciones", ascending=False).head(25).to_string(index=False))
    locs = (u.groupby("loc_prov").agg(adjudicaciones=("doc_contractual", "size"),
                                      proveedores=("cuit", "nunique"))
              .sort_values("adjudicaciones", ascending=False).reset_index())
    locs["pct"] = (100 * locs.adjudicaciones / len(u)).round(1)
    locs.to_csv(TAB / f"tab_unidad_{tag}_localidades.csv", index=False, encoding="utf-8")
    print("\nSupplier localities:\n", locs.head(8).to_string(index=False))
    print(f"\nwindow totals: {len(u)} awards, {u.cuit.nunique()} distinct suppliers, "
          f"{100*u.persona_fisica.mean():.1f}% to natural persons, {100*u.local.mean():.1f}% local")


print("\n" + "=" * 70 + "\nMEDIAN ANCHORED UNIT\n" + "=" * 70)
describe(med.uoc, "mediana")
print("\n" + "=" * 70 + "\nMEDIAN GARRISON\n" + "=" * 70)
describe(med_g.uoc, "guarnicion")
