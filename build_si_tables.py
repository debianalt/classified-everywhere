"""
build_si_tables.py — write the supplementary tables into sections/S_supplementary.md.

The supplement narrates thirty tables (S1-S27) and, until this script, carried
exactly one: the table of sources. Everything else lived as a CSV in tables/,
which meant a reviewer could not see a single coefficient, interval or test.

The tables are written into the markdown itself, not injected at DOCX time, so
that the author reads the same document in Obsidian that the journal receives.
Each block is delimited by HTML comments, invisible in Obsidian's reading view,
and the script rebuilds only what lies between them: prose edits outside the
markers survive a rerun. Placement is automatic — a block goes after the first
paragraph that names its table — so the running order follows the prose.

This is document assembly, not analysis, so it is Python by the tree rule.
Every number comes from a canonical CSV in tables/; nothing is computed here
beyond unit conversion (proportion to percentage) and rounding.

Run: python build_si_tables.py
"""
import csv
import re
from pathlib import Path

BASE = Path(__file__).resolve().parent
TABLES = BASE / "tables"
SI = BASE / "sections" / "S_supplementary.md"

# ---------------------------------------------------------------------------
# Label maps, mirroring theme_house.R. Anything that reaches the page in
# English there reaches it in English here.
# ---------------------------------------------------------------------------
ERA = {"macri": "Macri", "fernandez": "Fernández", "milei": "Milei"}
STRATUM = {"core": "Core", "middle": "Middle", "outer": "Outer"}
REGION = {"NEA": "North-east", "Metro": "Metropolitan", "Resto": "Rest of country"}
TIPO = {
    "seguridad_defensa": "Security and defence",
    "infraestructura": "Infrastructure",
    "salud_social": "Health and social",
    "ciencia_universidad": "Science and universities",
    "cultura_educacion": "Culture and education",
    "administracion": "General administration",
}
FORMA = {
    "Persona fisica": "Sole proprietor", "Persona Fisica": "Sole proprietor",
    "Sociedad Anonima": "SA", "Sociedad de Responsabilidad Limitada": "SRL",
    "Otra Forma": "Other legal form", "sociedad": "Company",
    "Sociedad Responsabilidad Limitada": "SRL",
    "Otras Formas Societarias": "Other legal form",
    "persona_fisica": "Sole proprietor", "Sociedad Anónima": "SA",
}
CAT = {
    "A_personeria.SA": "SA", "A_personeria.SRL": "SRL",
    "A_personeria.PersFisica": "Sole proprietor",
    "A_personeria.OtraForma": "Other legal form",
    "A_rubro.PROD. MEDICO/FARMA": "Medical and pharmaceutical",
    "A_rubro.ALQUILER": "Rental", "A_rubro.ALIMENTOS": "Food",
    "A_rubro.EQUIPOS": "Equipment",
    "A_rubro.SERV. PROFESIONAL ": "Professional services",
    # the cell is stripped before the lookup, so the key without the trailing
    # space of the source table has to exist too (3 Sep 2026: this one category
    # printed as its internal code in Table S8)
    "A_rubro.SERV. PROFESIONAL": "Professional services",
    "A_rubro.MANT. REPARACION Y": "Maintenance and repair",
    "A_rubro.ELECTRICIDAD Y TEL": "Electrical and telecoms",
    "A_rubro.REPUESTOS": "Spare parts", "A_rubro.OTROS": "Other sectors",
    "A_cliente.seguridad_defensa": "Security and defence",
    "A_cliente.infraestructura": "Infrastructure",
    "A_cliente.salud_social": "Health and social",
    "A_cliente.ciencia_universidad": "Science and universities",
    "A_cliente.cultura_educacion": "Culture and education",
    "A_cliente.administracion": "General administration",
    "A_escala.chica": "Small median award",
    "A_escala.media": "Mid median award",
    "A_escala.grande": "Large median award",
    "A_escala.sin_ars": "No peso awards",
    "A_directa.sin_directa": "No direct contracting",
    "A_directa.mixto": "Mixed procedure",
    "A_directa.mayoria_directa": "Mostly direct",
    "A_registro.reg16_18": "Registered 2016-18",
    "A_registro.reg19_21": "Registered 2019-21",
    "A_registro.reg22plus": "Registered 2022+",
}
VAR = {
    "A_personeria": "Legal form", "A_rubro": "Sector",
    "A_cliente": "Modal client", "A_escala": "Scale",
    "A_directa": "Procedure", "A_registro": "Registration",
    "intensidad": "Award intensity", "antiguedad": "Tenure",
    "S_cohorte": "Founding cohort", "estrato": "Stratum",
    "provincia": "Jurisdiction",
}
KM_GROUP = {
    "region=Metro": "Metropolitan", "region=NEA": "North-east",
    "region=Resto": "Rest of country",
    "territorial=anclado": "Anchored client at entry",
    "territorial=otro": "Other client at entry",
}
# The interaction fit labels its strata "region=NEA, territorial=otro", and the
# survival package pads the shorter level with trailing spaces. Neither the
# internal codes nor the Spanish reach the page (3 Sep 2026).
for _r, _rl in (("Metro", "Metropolitan"), ("NEA", "North-east"),
                ("Resto", "Rest of country")):
    for _t, _tl in (("anclado", "anchored client"), ("otro", "other client")):
        for _pad in ("", "   "):
            KM_GROUP[f"region={_r}, territorial={_t}{_pad}"] = f"{_rl}, {_tl}"
KM_MODEL = {
    "region": "By region", "territorial": "By client anchoring",
    "region x territorial": "Region by anchoring",
}
ANCLA = {"anclado": "Anchored", "no_anclado": "Other", "otro": "Other"}
ENDOW = {
    "benzecri_dim1": "Modified rate, axis 1 (%)",
    "benzecri_dim2": "Modified rate, axis 2 (%)",
    "cor_dim1_E_vs_F_bestmatch": "Axis 1 best match to the published space",
    "cor_dim2_E_vs_F_bestmatch": "Axis 2 best match to the published space",
    "typic_seguridad_dim1_en_E": "Test value, security and defence, axis 1",
    "typic_seguridad_dim2_en_E": "Test value, security and defence, axis 2",
    "typic_outer_dim1_en_E": "Test value, outer stratum, axis 1",
    "typic_outer_dim2_en_E": "Test value, outer stratum, axis 2",
}

# Free-text labels: test names, variant names, model terms. They are data
# values written by the analysis scripts in Spanish and reach the page in
# English, exactly as the figure labels do.
TEXT = {
    "True": "Yes", "TRUE": "Yes", "False": "No", "FALSE": "No",
    # the composition counterfactual summary (Table S24)
    "rango observado": "Observed range",
    "rango contrafactual": "Counterfactual range",
    "desvio observado": "Observed standard deviation",
    "desvio contrafactual": "Counterfactual standard deviation",
    "gap Formosa-CABA observado": "Observed Formosa-CABA gap",
    "gap Formosa-CABA contrafactual": "Counterfactual Formosa-CABA gap",
    "gap cerrado por composicion (%)": "Gap closed by composition (%)",
    "rho con anclado, observado":
        "Rank correlation with anchored demand, observed",
    "rho con anclado, contrafactual":
        "Rank correlation with anchored demand, counterfactual",
    # supplementary variables and their categories
    "cohorte_arca": "Founding cohort", "intensidad": "Award intensity",
    "tenure": "Tenure", "estrato": "Stratum", "q2": "Quartile on axis 2",
    "1adj": "1 award", "2-5adj": "2-5 awards", "6-20adj": "6-20 awards",
    "20+adj": "Over 20 awards", "1anio": "1 year", "2-4anios": "2-4 years",
    "5+anios": "5 or more years", "fund_pre2000": "Founded before 2000",
    "fund_2001_2010": "Founded 2001-10", "fund_2011_2016": "Founded 2011-16",
    "fund_2017plus": "Founded after 2016", "pers_fisica": "Natural person",
    "sin_match": "Unmatched in the companies register",
    "Cooperativas": "Cooperative",
    # inequality variants
    "baseline": "Baseline", "sin_seguridad": "Excluding security and defence",
    "mix_constante": "Buyer mix held constant",
    "mix_constante_2019": "Buyer mix held at 2019",
    "mix_constante_organismo": "Buyer mix held at the organism level",
    "nominal": "Nominal pesos", "constante 2024": "Constant 2024 pesos",
    "Theil crudo vs n de proveedores": "Theil T against the number of suppliers",
    "Theil normalizado vs n de proveedores":
        "Normalised Theil against the number of suppliers",
    "Theil normalizado vs demanda anclada":
        "Normalised Theil against anchored demand",
    "Theil normalizado vs proveedores persona física":
        "Normalised Theil against the share of natural persons",
    "Theil normalizado vs share del proveedor principal":
        "Normalised Theil against the leading supplier's share",
    # RV size control
    "Kruskal-Wallis entre estratos, RV observado":
        "Kruskal-Wallis between strata, RV as observed",
    "Kruskal-Wallis entre estratos, RV a n = 150":
        "Kruskal-Wallis between strata, RV at *n* = 150",
    "Spearman: n vs RV observado":
        "Spearman, suppliers against RV as observed",
    "Spearman: n vs RV a n = 150":
        "Spearman, suppliers against RV at *n* = 150",
    # gradient checks
    "rho observado": "As observed",
    "rho sin el proveedor lider de cada jurisdiccion":
        "Removing the leading supplier of every jurisdiction",
    "anclado vs alineado": "Anchored demand against political alignment",
    "anclado vs ITPP": "Anchored demand against budget transparency",
    "persona fisica vs alineado":
        "Natural persons against political alignment",
    "persona fisica vs ITPP":
        "Natural persons against budget transparency",
    "anclado vs alineado (recodificado)":
        "Anchored demand against alignment, recoded",
    "persona fisica vs alineado (recodificado)":
        "Natural persons against alignment, recoded",
    "sin controles (Spearman)": "No controls (Spearman)",
    "parcial | log stock de sociedades":
        "Partial, given the log company stock",
    "parcial | log stock + ITPP":
        "Partial, given the log company stock and budget transparency",
    "share_actual": "Published flag", "share_amplio": "Broad flag",
    "share_angosto": "Narrow flag",
    "share lider vs numero de proveedores":
        "Leading share against the number of suppliers",
    "share lider vs numero de adjudicaciones":
        "Leading share against the number of awards",
    "share normalizado vs numero de proveedores":
        "Normalised share against the number of suppliers",
    # counterfactual and shift-share
    "rango observado": "Observed range across jurisdictions",
    "rango contrafactual": "Counterfactual range",
    "desvio observado": "Observed standard deviation",
    "desvio contrafactual": "Counterfactual standard deviation",
    "gap Formosa-CABA observado": "Observed Formosa-CABA gap",
    "gap Formosa-CABA contrafactual": "Counterfactual Formosa-CABA gap",
    "gap cerrado por composicion (%)": "Gap closed by composition (%)",
    "caida total (pp)": "Total fall (percentage points)",
    "composicion hacia anclados (pp)":
        "Composition, the shift towards anchored organs (pp)",
    "estrechamiento dentro de cada tipo (pp)":
        "Narrowing inside each type (pp)",
    "share composicion (%)": "Composition share of the fall (%)",
    "share estrechamiento (%)": "Within share of the fall (%)",
    # survival
    "evento_pub": "Published: no award after the last one",
    "evento_2y": "Stricter: at least two silent years inside the window",
    "log-rank region": "Log-rank, region",
    "log-rank territorial en entrada": "Log-rank, client anchoring at entry",
    "aditivo": "Additive",
    "con interaccion region x territorial": "Region by anchoring interaction",
    "estratificado por rubro y cohorte":
        "Stratified by sector and entry cohort",
    "estratificado + intensidad":
        "Stratified, adding initial award volume",
    "efecto por periodo": "Anchoring estimated by period",
    "territorial": "Anchored client at entry", "region": "Region",
    "regionMetro": "Region: metropolitan",
    "regionResto": "Region: rest of country",
    "regionMetro:territorialanclado":
        "Region: metropolitan × anchored client",
    "formaPersFisica": "Legal form: sole proprietor",
    "formaSRL": "Legal form: SRL", "formaOtraForma": "Legal form: other",
    "forma": "Legal form", "rubro": "Sector", "entrada_c": "Year of entry",
    "intensidad_entrada": "Awards in the year of entry",
    "intensidad_banda": "Award intensity band",
    "intensidad_banda2-5": "Award intensity: 2-5",
    "intensidad_banda6-20": "Award intensity: 6-20",
    "intensidad_banda20+": "Award intensity: over 20",
    "periodoanios 4+": "Period: from the fourth year",
    "GLOBAL": "Global test",
    "Persona fisica: NEA vs Metro (valor por adjudicacion)":
        "Sole proprietors, north-east against metropolitan, value per award",
    "Sociedad: NEA vs Metro (valor por adjudicacion)":
        "Companies, north-east against metropolitan, value per award",
    # bare category codes, as the distance tables record them
    "chica": "Small median award", "media": "Mid median award",
    "grande": "Large median award", "sin_ars": "No peso awards",
    "sin_directa": "No direct contracting", "mixto": "Mixed procedure",
    "mayoria_directa": "Mostly direct", "PersFisica": "Sole proprietor",
    "OtraForma": "Other legal form", "reg16_18": "Registered 2016-18",
    "reg19_21": "Registered 2019-21", "reg22plus": "Registered 2022+",
    "Q1": "First quartile on the axis of access",
    "Q2": "Second quartile", "Q3": "Third quartile",
    "Q4": "Fourth quartile, the corporate end",
    "OLS: coef de anclado (shares), con log stock + ITPP":
        "Least squares on the shares, with both controls",
    # batteries
    "volumen": "Eleven variables of the realised relation",
    "declarada": "Eleven constitutive variables, declared capacities",
}

ALL_MAPS = {}
for _m in (ERA, STRATUM, REGION, TIPO, FORMA, CAT, VAR,
           KM_GROUP, KM_MODEL, ENDOW, TEXT):
    ALL_MAPS.update(_m)


# Columns that hold a year, an axis number or a count of dimensions take no
# thousands separator: 2019 is a label, not a quantity, and "2,019" is wrong.
NO_SEPARATOR = {"ejercicio", "Year", "dim", "eje_pub", "eje_rob", "mejor_eje",
                "primer_ejercicio", "ultimo_ejercicio", "gl", "t",
                "dim_que_matchea", "anios_lider", "anios_medianos"}


def _fmt(value, digits=None, pct=False, plain=False):
    """One cell. Proportions become percentages where asked; blanks become em
    dashes so an empty cell is never mistaken for a zero."""
    s = "" if value is None else str(value).strip()
    if s in ("", "NA", "NaN", "None"):
        return "—"
    if digits is None and not pct:
        return ALL_MAPS.get(s, s)
    try:
        x = float(s)
    except ValueError:
        return ALL_MAPS.get(s, s)
    if pct:
        x *= 100
    d = 1 if digits is None else digits
    # fixed decimals: a table column reads down, so 1.0 must not print as 1
    return f"{x:.{d}f}" if plain else f"{x:,.{d}f}"


def md_table(csv_name, cols=None, renames=None, digits=None, pct=(),
             maps=None, sort=None, where=None, limit=None):
    """A CSV as a markdown table, curated: chosen columns, English headers,
    fixed decimals, proportions as percentages."""
    path = TABLES / csv_name
    with open(path, encoding="utf-8-sig", newline="") as fh:
        rows = list(csv.DictReader(fh))
    if where:
        rows = [r for r in rows if where(r)]
    if sort:
        rows.sort(key=sort)
    if limit:
        rows = rows[:limit]
    if not rows:
        raise SystemExit(f"empty table: {csv_name}")
    cols = cols or list(rows[0].keys())
    renames = renames or {}
    digits = digits or {}
    maps = maps or {}
    head = [renames.get(c, c) for c in cols]
    out = ["| " + " | ".join(head) + " |",
           "|" + "|".join(["---"] * len(head)) + "|"]
    for r in rows:
        cells = []
        for c in cols:
            v = r.get(c, "")
            if c in maps:
                s = str(v).strip()
                # a cell may hold several codes joined by "; "; map each
                v = ("; ".join(maps[c].get(x, x) for x in s.split("; "))
                     if "; " in s else maps[c].get(s, v))
            cells.append(_fmt(v, digits.get(c), c in pct,
                              plain=c in NO_SEPARATOR))
        out.append("| " + " | ".join(cells) + " |")
    return "\n".join(out)


def block(key, title, body, note=None):
    parts = [f"<!-- TABLE:{key} -->", "", f"**Table {key}.** {title}", "", body]
    if note:
        parts += ["", f"*Note*: {note}"]
    parts += ["", f"<!-- /TABLE:{key} -->"]
    return "\n".join(parts)


# ---------------------------------------------------------------------------
# The tables, in the order the supplement names them.
# ---------------------------------------------------------------------------
def build():
    B = {}

    B["S1"] = block(
        "S1", "Record linkage by year, and the identifier repair.",
        md_table("tab_linkage_por_anio.csv",
                 renames={"Suppliers in the register (%)": "In the register (%)",
                          "Awards carrying a province (%)": "With a province (%)"},
                 digits={"Awards": 0, "New awards": 0, "Suppliers": 0,
                         "Suppliers in the register (%)": 1,
                         "Awards matched (%)": 1,
                         "Awards carrying a province (%)": 1},
                 maps={"Year": {"—": "All years"}})
        + "\n\n" + md_table("tab_linkage_reparacion.csv",
                            digits={"Rows": 0,
                                    "Passing the check digit (%)": 2}),
        "The last row of the first panel is the window as a whole. The second "
        "panel gives the check-digit repair: rows whose twelve-digit "
        "identifier duplicates the check digit inside the middle block, and "
        "rows recorded with eleven digits, each tested against the " "modulus-11 algorithm. The supplier column counts every supplier with an " "award in the year, renewals included; the 10,724 from which the text's " "exclusions start are the suppliers holding a new award in the window.")

    B["S2"] = block(
        "S2", "The 162 buying organisms, with volume, functional type, "
              "anchoring flag and the source of the label.",
        md_table("tab_buyer_typology_audit.csv",
                 cols=["organismo", "n_adj", "tipo", "anclaje_territorial",
                       "asignacion"],
                 renames={"organismo": "Buying organism", "n_adj": "Awards",
                          "tipo": "Functional type",
                          "anclaje_territorial": "Anchored",
                          "asignacion": "Label from"},
                 digits={"n_adj": 0}, maps={"tipo": TIPO},
                 sort=lambda r: -int(r["n_adj"])),
        "Anchored marks an organism operating through permanent delegations "
        "across the country. \"Label from\" distinguishes the labels produced "
        "by name rules from those corrected in the individual check.")

    B["S2b"] = block(
        "S2b", "The access gradient under two variant anchoring flags.",
        md_table("tab_anclaje_robustez.csv",
                 renames={"provincia": "Jurisdiction",
                          "share_actual": "Anchored, published flag (%)",
                          "share_angosto": "Anchored, narrow flag (%)",
                          "share_amplio": "Anchored, broad flag (%)",
                          "share_persona_fisica": "Natural persons (%)"},
                 pct=("share_actual", "share_angosto", "share_amplio",
                      "share_persona_fisica"),
                 digits={"share_actual": 1, "share_angosto": 1,
                         "share_amplio": 1, "share_persona_fisica": 1})
        + "\n\n" + md_table("tab_anclaje_robustez_rho.csv",
                            renames={"flag": "Flag", "rho": "*ρ*", "p": "*p*"},
                            digits={"rho": 3, "p": 3}),
        "The broad flag adds the conservatively unflagged organisms; the "
        "narrow flag keeps only the armed and border forces. The second panel "
        "gives the rank correlation of each flag with the share of suppliers "
        "that are natural persons, across the 24 jurisdictions.")

    B["S3"] = block(
        "S3", "North-eastern purchasing and the units stationed there, in "
              "both directions.",
        md_table("tab_uoc_localizacion_nea.csv",
                 renames={"direccion": "Direction", "clave": "Category",
                          "n": "Awards", "share": "Locally stationed (%)"},
                 pct=("share",), digits={"n": 0, "share": 1},
                 maps={"clave": {**TIPO, **REGION},
                       "direccion": {
                           "proveedor_NEA->unidad":
                               "To north-eastern suppliers, by type of organ",
                           "unidad_NEA->proveedor":
                               "From north-eastern units, by supplier location"}}),
        "Unmatched units count as non-local, so every locally sourced share "
        "is a floor. The share column is empty where the direction reports a "
        "destination rather than a local share.")

    B["S4"] = block(
        "S4", "The platform year by year: awards, buying organisms and "
              "anchored demand.",
        md_table("tab_platform_por_anio.csv",
                 renames={"ejercicio": "Year", "n_adj": "Awards",
                          "organismos": "Buying organisms",
                          "share_anclado": "Anchored demand (%)",
                          "share_seguridad": "Security and defence (%)"},
                 pct=("share_anclado", "share_seguridad"),
                 digits={"ejercicio": 0, "n_adj": 0, "organismos": 0,
                         "share_anclado": 1, "share_seguridad": 1}),
        "The years before 2019 are the platform's rollout and are excluded "
        "from the analysis.")

    B["S5"] = block(
        "S5", "Every buying organism with its first and last year in the "
              "record.",
        md_table("tab_platform_altas_organismos.csv",
                 renames={"organismo": "Buying organism",
                          "primer_ejercicio": "First year",
                          "ultimo_ejercicio": "Last year", "n_adj": "Awards",
                          "tipo": "Functional type", "anclaje": "Anchored"},
                 digits={"primer_ejercicio": 0, "ultimo_ejercicio": 0,
                         "n_adj": 0},
                 maps={"tipo": TIPO},
                 sort=lambda r: (int(r["primer_ejercicio"]), -int(r["n_adj"]))),
        "The army enters in 2019; the navy, the air force, the border force "
        "and the parks administration in 2018.")

    B["S6"] = block(
        "S6", "Between-province share of supplier inequality, on all buyers "
              "and on the balanced panel.",
        md_table("tab_platform_theil_panel.csv",
                 renames={"ejercicio": "Year",
                          "share_between_todos": "All buyers (%)",
                          "n_firmas_todos": "Suppliers, all buyers",
                          "share_between_panel": "Balanced panel (%)",
                          "n_firmas_panel": "Suppliers, panel"},
                 pct=("share_between_todos", "share_between_panel"),
                 digits={"ejercicio": 0, "share_between_todos": 1,
                         "n_firmas_todos": 0, "share_between_panel": 1,
                         "n_firmas_panel": 0})
        + "\n\n" + md_table("tab_boot_theil_ci.csv",
                            renames={"ejercicio": "Year",
                                     "n_firmas": "Suppliers",
                                     "share_between": "All buyers (%)",
                                     "ic_lo": "95% lower (%)",
                                     "ic_hi": "95% upper (%)", "B": "*B*"},
                            pct=("share_between", "ic_lo", "ic_hi"),
                            digits={"ejercicio": 0, "n_firmas": 0,
                                    "share_between": 2, "ic_lo": 2,
                                    "ic_hi": 2, "B": 0}),
        "Theil T on peso amounts among the suppliers winning within each "
        "year. The 2017 and 2018 rows are the platform's rollout and fall "
        "outside the analysis window; they are shown because they are what "
        "makes the all-buyers series rise. The second panel gives the "
        "bootstrap intervals for the all-buyers series, resampling suppliers "
        "within the year.")

    B["S6a"] = block(
        "S6a", "Anchored demand and procedure by region and presidency.",
        md_table("tab_gradiente_grupo_era.csv",
                 renames={"grupo": "Region", "era": "Presidency",
                          "n_adj": "Awards",
                          "share_anclado": "Anchored demand (%)",
                          "share_directa": "Direct contracting (%)",
                          "share_abierta": "Open procedures (%)"},
                 pct=("share_anclado", "share_directa", "share_abierta"),
                 digits={"n_adj": 0, "share_anclado": 1, "share_directa": 1,
                         "share_abierta": 1},
                 maps={"era": ERA}),
        "The Macri presidency is represented by 2019 alone.")

    B["S6b"] = block(
        "S6b", "The access gradient by region, and representation ratios by "
               "jurisdiction.",
        md_table("tab_gradiente_grupos.csv",
                 renames={"grupo": "Region", "provincias": "Jurisdictions",
                          "n_proveedores": "Suppliers",
                          "share_anclado": "Anchored demand (%)",
                          "share_seguridad": "Security and defence (%)",
                          "share_persona_fisica": "Natural persons (%)",
                          "share_directa": "Direct contracting (%)",
                          "ratio_firmas": "Representation, all suppliers",
                          "ratio_sociedades": "Representation, companies"},
                 pct=("share_anclado", "share_seguridad",
                      "share_persona_fisica", "share_directa"),
                 digits={"provincias": 0, "n_proveedores": 0,
                         "share_anclado": 1, "share_seguridad": 1,
                         "share_persona_fisica": 1, "share_directa": 1,
                         "ratio_firmas": 2, "ratio_sociedades": 2})
        + "\n\n" + md_table("tab_gradiente_provincias.csv",
                            cols=["provincia", "grupo", "proveedores",
                                  "ratio_firmas", "ratio_sociedades"],
                            renames={"provincia": "Jurisdiction",
                                     "grupo": "Region",
                                     "proveedores": "Suppliers",
                                     "ratio_firmas": "Representation, all suppliers",
                                     "ratio_sociedades": "Representation, companies"},
                            digits={"proveedores": 0, "ratio_firmas": 2,
                                    "ratio_sociedades": 2},
                            sort=lambda r: -float(r["ratio_firmas"])),
        "A representation ratio is a jurisdiction's winning suppliers against "
        "its stock of companies, indexed so that parity is 1. The region "
        "column is the country's standard six-region division, a label rather "
        "than an analytical unit.")

    B["S6c"] = block(
        "S6c", "The largest supplier in each jurisdiction.",
        md_table("tab_concentracion.csv",
                 cols=["provincia", "grupo", "n_adj", "proveedores",
                       "share_prov_lider", "personeria_lider", "cliente_lider",
                       "anios_lider", "directa_lider"],
                 renames={"provincia": "Jurisdiction", "grupo": "Region",
                          "n_adj": "Awards", "proveedores": "Suppliers",
                          "share_prov_lider": "Held by the largest supplier (%)",
                          "personeria_lider": "Its legal form",
                          "cliente_lider": "Its modal client",
                          "anios_lider": "Years present",
                          "directa_lider": "Its direct contracting (%)"},
                 pct=("share_prov_lider", "directa_lider"),
                 digits={"n_adj": 0, "proveedores": 0, "share_prov_lider": 1,
                         "anios_lider": 0, "directa_lider": 1},
                 maps={"personeria_lider": FORMA, "cliente_lider": TIPO},
                 sort=lambda r: -float(r["share_prov_lider"])),
        "No identifier and no name is reported. The largest supplier is "
        "identified by rank within the jurisdiction only.")

    B["S6d"] = block(
        "S6d", "North-eastern direct contracting with and without the "
               "region's leading supplier.",
        md_table("tab_concentracion_nea_serie.csv",
                 renames={"era": "Presidency", "universo": "Series",
                          "n_adj": "Awards",
                          "share_directa": "Direct contracting (%)"},
                 pct=("share_directa",),
                 digits={"n_adj": 0, "share_directa": 1},
                 maps={"era": ERA,
                       "universo": {"con_lider": "With the leading supplier",
                                    "sin_lider": "Without it"}}),
        "The regional series changes direction when one supplier of 1,568 "
        "awards is set aside. This is the arithmetic behind reporting "
        "procedure by jurisdiction rather than by region.")

    B["S6e"] = block(
        "S6e", "The leading share against the number of suppliers.",
        md_table("tab_concentracion_normalizada.csv",
                 renames={"provincia": "Jurisdiction",
                          "proveedores": "Suppliers", "n_adj": "Awards",
                          "share_prov_lider": "Leading share (%)",
                          "share_norm": "Leading share over the log-log fit"},
                 pct=("share_prov_lider",),
                 digits={"proveedores": 0, "n_adj": 0,
                         "share_prov_lider": 1, "share_norm": 3},
                 sort=lambda r: -float(r["share_prov_lider"]))
        + "\n\n" + md_table("tab_concentracion_vs_n.csv",
                            renames={"prueba": "Test", "rho": "*ρ*",
                                     "p": "*p*"},
                            digits={"rho": 2, "p": 3}),
        "A leading share falls as suppliers multiply, as the largest share of "
        "any set must. The second panel gives the rank correlations.")

    B["S7"] = block(
        "S7", "Variants of the inequality series.",
        md_table("tab_robustez_theil.csv",
                 renames={"ejercicio": "Year", "variante": "Variant",
                          "share_between": "Between-province share (%)"},
                 pct=("share_between",),
                 digits={"ejercicio": 0, "share_between": 2})
        + "\n\n" + md_table("tab_theil_era_deflactado.csv",
                            renames={"era": "Presidency", "medida": "Units",
                                     "theil_total": "Theil T",
                                     "between_provincia": "Between provinces",
                                     "between_region_nea": "Between regions",
                                     "share_between_provincia":
                                         "Between-province share (%)"},
                            pct=("share_between_provincia",),
                            digits={"theil_total": 3,
                                    "between_provincia": 3,
                                    "between_region_nea": 3,
                                    "share_between_provincia": 1},
                            maps={"era": ERA}),
        "Because the decomposition is computed within years it is unaffected "
        "by inflation; the era-pooled panel is reported in nominal pesos and "
        "in constant 2024 pesos.")

    B["S7b"] = block(
        "S7b", "Inequality within each jurisdiction.",
        md_table("tab_theil_jurisdiccion.csv",
                 renames={"provincia": "Jurisdiction", "grupo": "Region",
                          "proveedores": "Suppliers", "theil": "Theil T",
                          "theil_norm": "Theil T over its ceiling",
                          "share_anclado": "Anchored demand (%)",
                          "share_persona_fisica": "Natural persons (%)",
                          "share_prov_lider": "Leading share (%)"},
                 pct=("share_anclado", "share_persona_fisica",
                      "share_prov_lider"),
                 digits={"proveedores": 0, "theil": 2, "theil_norm": 3,
                         "share_anclado": 1, "share_persona_fisica": 1,
                         "share_prov_lider": 1},
                 sort=lambda r: -float(r["theil"])),
        "Theil T of award amounts in constant 2024 pesos across the suppliers "
        "of each jurisdiction, and the same index divided by its ceiling, the "
        "logarithm of the number of suppliers.")

    B["S7c"] = block(
        "S7c", "What the within-jurisdiction index tracks.",
        md_table("tab_theil_jurisdiccion_cor.csv",
                 renames={"relacion": "Relation", "rho": "*ρ*", "p": "*p*",
                          "jurisdicciones": "Jurisdictions"},
                 digits={"rho": 2, "p": 3, "jurisdicciones": 0}),
        "The raw index rises with the number of suppliers, which is "
        "mechanical; the normalised one does not, and relates to nothing in "
        "the argument.")

    B["S7d"] = block(
        "S7d", "The series on the suppliers present in every year.",
        md_table("tab_theil_panel_proveedores.csv",
                 renames={"ejercicio": "Year", "n_prov": "Jurisdictions",
                          "n_sup": "Suppliers", "theil_total": "Theil T",
                          "between": "Between provinces",
                          "share_between": "Between-province share (%)"},
                 pct=("share_between",),
                 digits={"ejercicio": 0, "n_prov": 0, "n_sup": 0,
                         "theil_total": 3, "between": 3,
                         "share_between": 1}),
        "The 832 suppliers winning in every year of the window. The "
        "between-province share moves within a narrow band and shows no "
        "trend, so the negative result holds with suppliers held constant as "
        "well as buyers.")

    B["S8"] = block(
        "S8", "The active categories: frequency, coordinates, contributions "
              "and squared cosines on the first three axes.",
        md_table("tab_c10_categorias.csv",
                 renames={"categoria": "Category", "n": "Suppliers",
                          "dim.1": "Axis 1", "dim.2": "Axis 2",
                          "dim.3": "Axis 3", "ctr.dim.1": "Ctr 1",
                          "ctr.dim.2": "Ctr 2", "ctr.dim.3": "Ctr 3",
                          "cos2.dim.1": "cos² 1", "cos2.dim.2": "cos² 2",
                          "cos2.dim.3": "cos² 3"},
                 digits={"n": 0, "dim.1": 2, "dim.2": 2, "dim.3": 2,
                         "ctr.dim.1": 1, "ctr.dim.2": 1, "ctr.dim.3": 1,
                         "cos2.dim.1": 3, "cos2.dim.2": 3, "cos2.dim.3": 3},
                 maps={"categoria": CAT},
                 sort=lambda r: float(r["dim.2"])),
        "Categories carried by fewer than 5 per cent of suppliers are "
        "passivated and take no part in the axes. Contributions are "
        "percentages of an axis's inertia.")

    B["S8b"] = block(
        "S8b", "Contributions aggregated by variable, per axis.",
        md_table("tab_c10_variables.csv",
                 renames={"variable": "Variable", "dim.1": "Axis 1",
                          "dim.2": "Axis 2", "dim.3": "Axis 3"},
                 digits={"dim.1": 1, "dim.2": 1, "dim.3": 1},
                 maps={"variable": VAR}),
        "Percentages of each axis's inertia.")

    B["S9"] = block(
        "S9", "Eigenvalues and modified rates.",
        md_table("tab_c10_benzecri.csv",
                 renames={"dim": "Axis", "eigenvalue": "Eigenvalue",
                          "pct_benzecri": "Modified rate (%)"},
                 digits={"dim": 0, "eigenvalue": 4, "pct_benzecri": 1}),
        "Modified rates follow Benzécri's correction.")

    B["S10"] = block(
        "S10", "Supplementary variables: coordinates and test values.",
        md_table("tab_c10_supvars.csv",
                 renames={"variable": "Variable", "categoria": "Category",
                          "dim.1": "Axis 1", "dim.2": "Axis 2",
                          "dim.3": "Axis 3", "typic.dim.1": "Test value 1",
                          "typic.dim.2": "Test value 2",
                          "typic.dim.3": "Test value 3"},
                 digits={"dim.1": 2, "dim.2": 2, "dim.3": 2,
                         "typic.dim.1": 1, "typic.dim.2": 1,
                         "typic.dim.3": 1},
                 maps={"variable": VAR})
        + "\n\n" + md_table("tab_c10_jurisdicciones.csv",
                            cols=["categoria", "dim.1", "dim.2",
                                  "typic.dim.1", "typic.dim.2"],
                            renames={"categoria": "Jurisdiction",
                                     "dim.1": "Axis 1", "dim.2": "Axis 2",
                                     "typic.dim.1": "Test value 1",
                                     "typic.dim.2": "Test value 2"},
                            digits={"dim.1": 2, "dim.2": 2,
                                    "typic.dim.1": 1, "typic.dim.2": 1},
                            sort=lambda r: -float(r["dim.2"])),
        "A supplementary element takes no part in building the axes; its test " "value states how far its position departs from the centre of the " "cloud. Coordinates are in the scale of the cloud of categories, the " "barycentre of a category's individuals dilated by the inverse square " "root of the axis eigenvalue; the stratum gap of 0.8 standard deviations " "quoted in the text is computed on the undilated barycentres, 0.09 for " "the core and −0.33 for the outer stratum, against the standard deviation " "of the individuals on the axis, 0.51.")

    B["S11"] = block(
        "S11", "The per-province solutions against the national "
               "configuration, axis by axis.",
        md_table("tab_congruencia_eje2.csv",
                 cols=["provincia", "estrato", "n_proveedores", "comunes",
                       "rv_nacional", "congr_eje1", "eje1_matchea",
                       "congr_eje1_vs_1", "congr_eje2", "eje2_matchea",
                       "congr_eje2_vs_2"],
                 renames={"provincia": "Province", "estrato": "Stratum",
                          "n_proveedores": "Suppliers",
                          "comunes": "Categories in common",
                          "rv_nacional": "RV",
                          "congr_eje1": "Axis 1, best match",
                          "eje1_matchea": "by its axis",
                          "congr_eje1_vs_1": "Axis 1 against axis 1",
                          "congr_eje2": "Axis 2, best match",
                          "eje2_matchea": "by its axis ",
                          "congr_eje2_vs_2": "Axis 2 against axis 2"},
                 digits={"n_proveedores": 0, "comunes": 0, "rv_nacional": 3,
                         "congr_eje1": 3, "eje1_matchea": 0,
                         "congr_eje1_vs_1": 3, "congr_eje2": 3,
                         "eje2_matchea": 0, "congr_eje2_vs_2": 3},
                 maps={"estrato": STRATUM},
                 sort=lambda r: -float(r["rv_nacional"])),
        "Every province with at least 150 adjudicated suppliers, analysed on "
        "its own. Congruence is the absolute correlation between a national "
        "axis and the province's category coordinates; the best match takes "
        "the highest over the province's first three axes and names the axis "
        "that achieves it, since the axes of a specific analysis reorder "
        "across subclouds. The national second axis, the axis of access, has "
        "a counterpart in every province, at a rank that varies. The RV "
        "coefficient compares configurations as wholes and does not register "
        "a change of rank; it also rises with the share of the national "
        "configuration a province contributes, which Table S13 controls.")

    B["S11b"] = block(
        "S11b", "The same comparison with each province passivating on its "
                "own threshold.",
        md_table("tab_pasivacion_provincial.csv",
                 cols=["provincia", "n_proveedores", "pasivadas_propias",
                       "activas_localmente_perdidas", "cuales"],
                 renames={"provincia": "Province",
                          "n_proveedores": "Suppliers",
                          "pasivadas_propias": "Passivated on its own count",
                          "activas_localmente_perdidas":
                              "Active locally, passivated nationally",
                          "cuales": "Which"},
                 digits={"n_proveedores": 0, "pasivadas_propias": 0,
                         "activas_localmente_perdidas": 0},
                 maps={"cuales": CAT},
                 sort=lambda r: -int(r["activas_localmente_perdidas"]))
        + "\n\n" + md_table("tab_congruencia_eje2.csv",
                            cols=["provincia", "estrato", "comunes",
                                  "rv_nacional", "comunes_prop",
                                  "rv_provincial", "congr_eje2_prop"],
                            renames={"provincia": "Province",
                                     "estrato": "Stratum",
                                     "comunes": "Common, national rule",
                                     "rv_nacional": "RV, national rule",
                                     "comunes_prop": "Common, own rule",
                                     "rv_provincial": "RV, own rule",
                                     "congr_eje2_prop":
                                         "Axis 2, best match, own rule"},
                            digits={"comunes": 0, "rv_nacional": 3,
                                    "comunes_prop": 0, "rv_provincial": 3,
                                    "congr_eje2_prop": 3},
                            maps={"estrato": STRATUM},
                            sort=lambda r: -float(r["rv_provincial"]))
        + "\n\n" + md_table("tab_pasivacion_size_control.csv",
                            renames={"provincia": "Province",
                                     "estrato": "Stratum",
                                     "replicas": "Replications",
                                     "rv_mediano": "RV median at *n* = 150",
                                     "ic_lo": "2.5th percentile",
                                     "ic_hi": "97.5th percentile",
                                     "eje2_mediano":
                                         "Axis 2, median best match"},
                            digits={"replicas": 0, "rv_mediano": 3,
                                    "ic_lo": 3, "ic_hi": 3,
                                    "eje2_mediano": 3},
                            maps={"estrato": STRATUM},
                            sort=lambda r: -float(r["rv_mediano"]))
        + "\n\n" + md_table("tab_pasivacion_kw.csv",
                            renames={"prueba": "Test",
                                     "estadistico": "Statistic", "p": "*p*",
                                     "provincias": "Provinces"},
                            digits={"estadistico": 2, "p": 3,
                                    "provincias": 0}),
        "The 5 per cent rule is applied to the national cloud, so the same "
        "categories are passivated in every province. A category common in "
        "one province but rare in the country is therefore absent from that "
        "province's solution: five of the twelve have at least one, the "
        "largest carried by 6.2 per cent of Chaco's suppliers. Refitting each "
        "province under its own threshold raises the outer stratum's mean "
        "coefficient from 0.47 to 0.73 and removes the ordering; the "
        "comparison then runs over fewer common categories, 21 to 24 against "
        "24 to 25, so part of the gain is a smaller configuration. Under the "
        "size control the strata do not separate either.")

    B["S11c"] = block(
        "S11c", "The same comparison on a category set fixed for every "
                "province.",
        md_table("tab_rv_categorias_ausentes.csv",
                 cols=["categoria", "suppliers_nacional", "pct_nacional",
                       "provincias_sin"],
                 renames={"categoria": "National active category",
                          "suppliers_nacional": "Suppliers",
                          "pct_nacional": "% of suppliers",
                          "provincias_sin": "Provinces carrying none"},
                 digits={"suppliers_nacional": 0, "pct_nacional": 2},
                 maps={"categoria": CAT})
        + "\n\n" + md_table("tab_rv_conjunto_fijo.csv",
                            cols=["provincia", "estrato", "n_proveedores",
                                  "rv_conjunto_variable", "rv_conjunto_fijo",
                                  "rv_fijo_n150", "ic_lo", "ic_hi"],
                            renames={"provincia": "Province",
                                     "estrato": "Stratum",
                                     "n_proveedores": "Suppliers",
                                     "rv_conjunto_variable": "RV, shared set",
                                     "rv_conjunto_fijo": "RV, fixed set",
                                     "rv_fijo_n150": "RV, fixed set at n = 150",
                                     "ic_lo": "95% lower",
                                     "ic_hi": "95% upper"},
                            digits={"n_proveedores": 0,
                                    "rv_conjunto_variable": 3,
                                    "rv_conjunto_fijo": 3,
                                    "rv_fijo_n150": 3, "ic_lo": 3,
                                    "ic_hi": 3},
                            maps={"estrato": STRATUM},
                            sort=lambda r: -float(r["rv_fijo_n150"])),
        "One national active category is absent rather than rare in two "
        "north-eastern provinces, so the shared set of Table S11 is 24 "
        "categories there and 25 elsewhere. The fixed set is the 24 every "
        "compared province carries; the national reference and each province "
        "are refitted on it and the coefficient is recomputed over all of it. "
        "The equalised column draws 150 suppliers from every province and "
        "refits, 300 times, as Table S13 does for the shared set. Fixing the "
        "set lifts the outer stratum from 0.465 to 0.556 and leaves the "
        "core-middle-outer ordering weaker than before (Kruskal-Wallis "
        "*p* = 0.27 against 0.10); at a common sample size the ordering "
        "inverts, with the core lowest, and the test is far from significance "
        "(*p* = 0.78). The rank correlation between a province's supplier "
        "count and its coefficient is 0.31 on the shared set and 0.22 on the "
        "fixed one, neither separable from zero at twelve jurisdictions. At the "
        "threshold of 100 the absence extends to Formosa, so three of the four "
        "north-eastern provinces hold no supplier whose modal client is a "
        "university or a science agency. That absence is not a residue of the "
        "comparison: the oppositions are the same everywhere, but a province "
        "at the anchored end can lack an entire end of one of them, and which "
        "poles are missing is part of what territory grades "
        "(Table S13).")

    B["S12"] = block(
        "S12", "The same comparison by stratum, with bootstrap intervals and "
               "the class-specific solutions.",
        md_table("tab_boot_congruencia_ci.csv",
                 renames={"estrato": "Stratum", "n": "Suppliers",
                          "congruencia_dim1": "Congruence, axis 1",
                          "cong_ic_lo": "95% lower",
                          "cong_ic_hi": "95% upper", "rv": "RV",
                          "rv_ic_lo": "RV, 95% lower",
                          "rv_ic_hi": "RV, 95% upper",
                          "replicas_validas": "Valid replications",
                          "B": "*B*"},
                 digits={"n": 0, "congruencia_dim1": 3, "cong_ic_lo": 3,
                         "cong_ic_hi": 3, "rv": 3, "rv_ic_lo": 3,
                         "rv_ic_hi": 3, "replicas_validas": 0, "B": 0},
                 maps={"estrato": STRATUM})
        + "\n\n" + md_table("tab_mca2_congruencia.csv",
                            renames={"estrato": "Stratum", "n": "Suppliers",
                                     "benzecri_dim1": "Modified rate, axis 1 (%)",
                                     "congruencia_dim1_bestmatch":
                                         "Best match to the core, axis 1",
                                     "dim_que_matchea": "Matching axis",
                                     "congruencia_dim1_vs_dim1":
                                         "Axis 1 against axis 1",
                                     "congruencia_dim2_bestmatch":
                                         "Best match to the core, axis 2"},
                            digits={"n": 0, "benzecri_dim1": 1,
                                    "congruencia_dim1_bestmatch": 2,
                                    "dim_que_matchea": 0,
                                    "congruencia_dim1_vs_dim1": 2,
                                    "congruencia_dim2_bestmatch": 2},
                            maps={"estrato": STRATUM}),
        "The second panel is the class-specific analysis: each stratum's "
        "subcloud analysed within the reference space, matched against the "
        "core structure. The axes of a specific analysis can reorder across "
        "subclouds, so the best absolute correlation is reported with the "
        "axis that achieves it.")

    B["S13"] = block(
        "S13", "The size control: every province drawn down to 150 "
               "suppliers.",
        md_table("tab_rv_size_control.csv",
                 renames={"provincia": "Province", "estrato": "Stratum",
                          "n_proveedores": "Suppliers",
                          "rv_observado": "RV observed",
                          "replicas": "Replications",
                          "rv_mediano_n150": "RV median at *n* = 150",
                          "ic_lo": "2.5th percentile",
                          "ic_hi": "97.5th percentile",
                          "comunes_medianas": "Categories in common, median"},
                 digits={"n_proveedores": 0, "rv_observado": 3,
                         "replicas": 0, "rv_mediano_n150": 3, "ic_lo": 3,
                         "ic_hi": 3, "comunes_medianas": 0},
                 maps={"estrato": STRATUM},
                 sort=lambda r: -float(r["n_proveedores"]))
        + "\n\n" + md_table("tab_rv_size_control_estrato.csv",
                            renames={"estrato": "Stratum",
                                     "provincias": "Provinces",
                                     "replicas": "Replications",
                                     "rv_mediano_n150": "RV median at *n* = 150",
                                     "ic_lo": "2.5th percentile",
                                     "ic_hi": "97.5th percentile",
                                     "rv_observado_medio": "RV observed, mean",
                                     "n_min": "Smallest province",
                                     "n_max": "Largest province"},
                            digits={"provincias": 0, "replicas": 0,
                                    "rv_mediano_n150": 3, "ic_lo": 3,
                                    "ic_hi": 3, "rv_observado_medio": 3,
                                    "n_min": 0, "n_max": 0},
                            maps={"estrato": STRATUM})
        + "\n\n" + md_table("tab_rv_size_control_kw.csv",
                            renames={"prueba": "Test",
                                     "estadistico": "Statistic", "p": "*p*",
                                     "provincias": "Provinces"},
                            digits={"estadistico": 2, "p": 3,
                                    "provincias": 0})
        + "\n\n" + md_table("tab_rv_mde.csv",
                            renames={"delta_rv_outer":
                                         "Displacement of the outer stratum",
                                     "potencia": "Power"},
                            digits={"delta_rv_outer": 2, "potencia": 2}),
        "The last panel is the detectable displacement: shifting the outer "
        "provinces' replicate distributions downward and resampling each "
        "province's median, with 1,000 simulations per shift.")

    B["S13b"] = block(
        "S13b", "What the coefficient looks like when the structure is the "
                "same by construction.",
        md_table("tab_rv_nulo_referencia.csv",
                 renames={"distribucion": "Reference distribution",
                          "replicas": "Replications",
                          "p2.5": "2.5th percentile", "mediana": "Median",
                          "p97.5": "97.5th percentile",
                          "comunes_medianas": "Categories in common, median"},
                 digits={"replicas": 0, "p2.5": 3, "mediana": 3, "p97.5": 3,
                         "comunes_medianas": 0})
        + "\n\n" + md_table("tab_rv_nulo_provincias.csv",
                            cols=["provincia", "estrato", "n_proveedores",
                                  "rv_observado", "rv_mediano_n150",
                                  "percentil_en_el_nulo", "posicion"],
                            renames={"provincia": "Province",
                                     "estrato": "Stratum",
                                     "n_proveedores": "Suppliers",
                                     "rv_observado": "RV observed",
                                     "rv_mediano_n150": "RV at *n* = 150",
                                     "percentil_en_el_nulo":
                                         "Its percentile in the null",
                                     "posicion": "Position"},
                            digits={"n_proveedores": 0, "rv_observado": 3,
                                    "rv_mediano_n150": 3,
                                    "percentil_en_el_nulo": 3},
                            maps={"estrato": STRATUM,
                                  "posicion": {"inside": "inside the band",
                                               "below": "below the band",
                                               "above": "above the band"}},
                            sort=lambda r: float(r["rv_mediano_n150"]))
        + "\n\n" + md_table("tab_rv_nulo_estratos.csv",
                            renames={"estrato": "Stratum",
                                     "provincias": "Provinces",
                                     "dentro": "Inside the null band",
                                     "percentil_mediano":
                                         "Median percentile in the null"},
                            digits={"provincias": 0, "dentro": 0,
                                    "percentil_mediano": 3},
                            maps={"estrato": STRATUM}),
        "Overlapping intervals say that two coefficients cannot be told "
        "apart; they do not say what the coefficient would be if the "
        "structures were identical. The first panel builds that reference "
        "twice. Slices of 150 individuals drawn at random from the whole "
        "cloud carry the national structure by construction, and their "
        "coefficients against the national configuration are the most a "
        "province of that size could reach; two halves of the cloud compared "
        "with each other give the coefficient's ceiling, and it is close to "
        "one, so the measure is not compressed. Every one of the twelve jurisdictions "
        "falls inside the central 95 per cent of the null, and the "
        "province lowest in it is the federal capital. The equivalence the "
        "survival model states through two one-sided tests (Table S17b) is "
        "stated here through this reference distribution.")

    B["S14"] = block(
        "S14", "The endowments-only model.",
        md_table("tab_mca_endowments_check.csv",
                 renames={"metrica": "Quantity", "modelo_E": "Endowments only",
                          "modelo_F": "Published space"},
                 digits={"modelo_E": 3, "modelo_F": 3},
                 maps={"metrica": ENDOW}),
        "The endowments model drops the client, the procedure and the scale, "
        "keeping legal form, sector and registration cohort. Its dominant "
        "axis is the axis of access, so the gradient is not an artefact of "
        "having admitted the relation among the active variables.")

    B["S15"] = block(
        "S15", "Kaplan-Meier estimates by region and by client anchoring at "
               "entry.",
        md_table("tab_km_survival.csv",
                 renames={"modelo": "Model", "grupo": "Group", "t": "Year",
                          "n_risk": "At risk", "n_event": "Exits",
                          "surv": "Survival", "lo": "95% lower",
                          "hi": "95% upper"},
                 digits={"t": 0, "n_risk": 0, "n_event": 0, "surv": 3,
                         "lo": 3, "hi": 3},
                 maps={"grupo": {**REGION, **STRATUM}})
        + "\n\n" + md_table("tab_km_logrank.csv",
                            renames={"prueba": "Test", "chisq": "χ²",
                                     "gl": "*df*", "p": "*p*"},
                            digits={"chisq": 2, "gl": 0, "p": 3}),
        "Spells run from a supplier's first award to its last, "
        "right-censored in 2025.")

    B["S16"] = block(
        "S16", "Five Cox specifications.",
        md_table("tab_cox_model.csv",
                 renames={"modelo": "Specification", "termino": "Term",
                          "HR": "Hazard ratio", "ic_lo": "95% lower",
                          "ic_hi": "95% upper", "z": "*z*", "p": "*p*"},
                 digits={"HR": 3, "ic_lo": 3, "ic_hi": 3, "z": 2, "p": 3}),
        "Additive; with a region-by-anchoring interaction; stratified by "
        "sector and entry cohort; additionally stratified by initial award "
        "volume; and with the anchoring effect estimated separately for the "
        "first three years and thereafter. The coefficient on anchoring is "
        "stable across all five.")

    B["S17"] = block(
        "S17", "Proportional-hazards tests.",
        md_table("tab_cox_ph_test.csv",
                 renames={"modelo": "Specification", "termino": "Term",
                          "chisq": "χ²", "gl": "*df*", "p": "*p*"},
                 digits={"chisq": 2, "gl": 0, "p": 3}),
        "The violation flagged for the control variables does not reach the "
        "term of interest.")

    B["S17b"] = block(
        "S17b", "The regional null stated as equivalence.",
        md_table("tab_tost_region.csv",
                 renames={"termino": "Term", "HR": "Hazard ratio",
                          "ic90_lo": "90% lower", "ic90_hi": "90% upper",
                          "ic95_lo": "95% lower", "ic95_hi": "95% upper",
                          "margen_equivalencia_conjunto":
                              "Joint equivalence margin"},
                 digits={"HR": 3, "ic90_lo": 3, "ic90_hi": 3, "ic95_lo": 3,
                         "ic95_hi": 3, "margen_equivalencia_conjunto": 2}),
        "Two one-sided tests at the 5 per cent level. Regional differences in "
        "exit beyond the joint margin are rejected.")

    B["S17c"] = block(
        "S17c", "A stricter definition of exit.",
        md_table("tab_salida_sensibilidad.csv",
                 renames={"definicion": "Definition of exit",
                          "HR": "Hazard ratio", "ic_lo": "95% lower",
                          "ic_hi": "95% upper", "eventos": "Events"},
                 digits={"HR": 3, "ic_lo": 3, "ic_hi": 3, "eventos": 0}),
        "Recoding as censored every supplier whose last award falls in "
        "2024-25 requires at least two silent years inside the window for an "
        "exit to count.")

    B["S18"] = block(
        "S18", "Scale of the exchange by region and legal form.",
        md_table("tab_sole_proprietor_scale.csv",
                 cols=["forma", "region", "n_proveedores", "monto_mediano",
                       "monto_mediano_usd", "valor_mediano_por_adj",
                       "valor_mediano_por_adj_usd", "adj_medianas",
                       "organismos_medianos", "rubros_medianos"],
                 renames={"forma": "Legal form", "region": "Region",
                          "n_proveedores": "Suppliers",
                          "monto_mediano": "Median total, constant 2024 pesos",
                          "monto_mediano_usd": "Median total, US dollars",
                          "valor_mediano_por_adj":
                              "Median per award, constant 2024 pesos",
                          "valor_mediano_por_adj_usd":
                              "Median per award, US dollars",
                          "adj_medianas": "Median awards",
                          "organismos_medianos": "Median buying organisms",
                          "rubros_medianos": "Median sectors"},
                 digits={"n_proveedores": 0, "monto_mediano": 0,
                         "monto_mediano_usd": 0, "valor_mediano_por_adj": 0,
                         "valor_mediano_por_adj_usd": 0, "adj_medianas": 0,
                         "organismos_medianos": 0, "rubros_medianos": 0},
                 maps={"forma": FORMA, "region": REGION}),
        "Dollar figures convert each award on its nominal peso amount at the "
        "average official rate of its own year, so a dollar column pooled "
        "across years is in current dollars of mixed years and is a different "
        "quantity from the constant-peso column beside it. Neither register "
        "carries a size variable, so what is measured is the scale of the " "exchange, not the size of the supplier. Counts are of suppliers with at " "least one peso award, on which a constant-peso median exists, so they " "fall slightly short of the totals of Table 2.")

    B["S18b"] = block(
        "S18b", "The same comparison inside sectors.",
        md_table("tab_escala_por_sector.csv",
                 cols=["forma", "rubro_principal", "n_adj_NEA", "n_adj_Metro",
                       "mediana_usd_NEA", "mediana_usd_Metro", "ratio_usd"],
                 renames={"forma": "Legal form",
                          "rubro_principal": "Sector",
                          "n_adj_NEA": "Awards, north-east",
                          "n_adj_Metro": "Awards, metropolitan",
                          "mediana_usd_NEA": "Median award, north-east (USD)",
                          "mediana_usd_Metro": "Median award, metropolitan (USD)",
                          "ratio_usd": "Ratio"},
                 digits={"n_adj_NEA": 0, "n_adj_Metro": 0,
                         "mediana_usd_NEA": 0, "mediana_usd_Metro": 0,
                         "ratio_usd": 3},
                 maps={"forma": FORMA})
        + "\n\n" + md_table("tab_escala_por_sector_resumen.csv",
                            renames={"forma": "Legal form",
                                     "sectores": "Sectors compared",
                                     "ratio_bruto": "Pooled ratio",
                                     "ratio_ajustado":
                                         "Reweighted to the metropolitan mix",
                                     "mediana_ratios": "Median sector ratio"},
                            digits={"sectores": 0, "ratio_bruto": 2,
                                    "ratio_ajustado": 2,
                                    "mediana_ratios": 2},
                            maps={"forma": FORMA}),
        "Award by award, for every cell with at least 30 awards. Part of the "
        "pooled gap is product mix; roughly half the metropolitan value "
        "remains once the sector is held.")

    B["S19"] = block(
        "S19", "Rank tests comparing north-eastern with metropolitan "
               "suppliers within each legal form.",
        md_table("tab_sole_proprietor_test.csv",
                 renames={"comparacion": "Comparison",
                          "n_nea": "*n*, north-east",
                          "n_metro": "*n*, metropolitan",
                          "mediana_nea": "Median, north-east",
                          "mediana_metro": "Median, metropolitan", "p": "*p*"},
                 digits={"n_nea": 0, "n_metro": 0, "mediana_nea": 0,
                         "mediana_metro": 0, "p": 4}),
        "Medians in constant 2024 pesos.")

    B["S20"] = block(
        "S20", "The balanced panel of 72 buyers, year by year.",
        md_table("tab_era_anual_panel.csv",
                 renames={"ejercicio": "Year", "n_adj": "Awards",
                          "pct_directa": "Direct contracting (%)",
                          "pct_anclado": "Anchored demand (%)",
                          "pct_salud_adj": "Health and social, awards (%)",
                          "pct_salud_monto": "Health and social, amount (%)",
                          "monto_real_bn":
                              "Spending, billions of constant 2024 pesos"},
                 digits={"ejercicio": 0, "n_adj": 0, "pct_directa": 1,
                         "pct_anclado": 1, "pct_salud_adj": 1,
                         "pct_salud_monto": 1, "monto_real_bn": 1}),
        "The 2020 rows show the health emergency arriving as contraction "
        "rather than surge: purchasing under Administrative Decision 409/2020 "
        "ran outside the ordinary regime and is not in the award record.")

    B["S21"] = block(
        "S21", "The composition of the panel's demand by presidency.",
        md_table("tab_era_composicion_panel.csv",
                 renames={"tipo_organismo": "Functional type",
                          "macri": "Macri (%)", "fernandez": "Fernández (%)",
                          "milei": "Milei (%)"},
                 digits={"macri": 1, "fernandez": 1, "milei": 1},
                 maps={"tipo_organismo": TIPO}),
        "Percentages of the panel's awards. The Macri presidency is "
        "represented by 2019 alone.")

    B["S22"] = block(
        "S22", "Batteries of the realised relation, and their axes against "
               "the published ones.",
        md_table("tab_bateria_volumen.csv",
                 renames={"bateria": "Battery", "dim": "Axis",
                          "mrate": "Modified rate (%)",
                          "mejor_eje": "Best-matching published axis",
                          "r_abs": "|*r*|"},
                 digits={"dim": 0, "mrate": 1, "mejor_eje": 0, "r_abs": 2})
        + "\n\n" + md_table("tab_robustez_bateria.csv",
                            renames={"eje_pub": "Published axis",
                                     "eje_rob": "Axis of the variant",
                                     "r_abs": "|*r*|",
                                     "mrate_rob": "Modified rate (%)"},
                            digits={"eje_pub": 0, "eje_rob": 0, "r_abs": 2,
                                    "mrate_rob": 1}),
        "Eleven variables of the realised relation collapse onto a single "
        "axis of volume, the size effect that follows from mixing several "
        "quantities.")

    B["S22b"] = block(
        "S22b", "The declared-offer space.",
        md_table("tab_oferta_declarada_benzecri.csv",
                 renames={"bateria": "Battery", "dim": "Axis",
                          "mrate": "Modified rate (%)"},
                 digits={"dim": 0, "mrate": 1})
        + "\n\n" + md_table("tab_oferta_declarada_estratos.csv",
                            renames={"bateria": "Battery",
                                     "estrato": "Stratum",
                                     "dim.1": "Axis 1", "dim.2": "Axis 2",
                                     "dim.3": "Axis 3",
                                     "typic.dim.1": "Test value 1",
                                     "typic.dim.2": "Test value 2",
                                     "typic.dim.3": "Test value 3"},
                            digits={"dim.1": 2, "dim.2": 2, "dim.3": 2,
                                    "typic.dim.1": 1, "typic.dim.2": 1,
                                    "typic.dim.3": 1},
                            maps={"estrato": STRATUM})
        + "\n\n" + md_table("tab_oferta_declarada_r.csv",
                            renames={"bateria": "Battery",
                                     "eje_pub": "Published axis",
                                     "mejor_eje": "Best-matching axis",
                                     "r_abs": "|*r*|"},
                            digits={"eje_pub": 0, "mejor_eje": 0,
                                    "r_abs": 2}),
        "The eight binary families of capacity declared at registration, "
        "added to the legal form, the sector and the modal client. The "
        "content axis survives; the axis of access has no counterpart in what "
        "suppliers declare.")

    B["S23"] = block(
        "S23", "Checks on the access gradient.",
        md_table("tab_gradiente_loo.csv",
                 renames={"medida": "Measure", "rho": "*ρ*", "p": "*p*"},
                 digits={"rho": 3, "p": 4})
        + "\n\n" + md_table("tab_control_politico.csv",
                            renames={"prueba": "Test", "n": "*n*",
                                     "rho": "*ρ*", "p": "*p*"},
                            digits={"n": 0, "rho": 2, "p": 3})
        + "\n\n" + md_table("tab_control_politico_sens.csv",
                            renames={"prueba": "Recoding", "rho": "*ρ*",
                                     "p": "*p*"},
                            digits={"rho": 2, "p": 3}),
        "The first panel removes the leading supplier of every jurisdiction "
        "at once. The second gives the political controls; the third recodes "
        "the five debatable alignments as a block, in the direction that "
        "would strengthen an alignment reading.")

    B["S23c"] = block(
        "S23c", "The gradient with controls.",
        md_table("tab_gradiente_controles.csv",
                 renames={"especificacion": "Specification",
                          "valor": "Coefficient"},
                 digits={"valor": 3}),
        "Partial rank correlations across the 24 jurisdictions. The "
        "association is attenuated by company density, not explained away.")

    B["S23d"] = block(
        "S23d", "Registration with the national system, by jurisdiction.",
        md_table("tab_legibilidad_gradiente.csv",
                 renames={"provincia": "Jurisdiction", "grupo": "Region",
                          "share_anclado": "Anchored demand (%)",
                          "registrados": "Registrants",
                          "registrados_sociedades": "Registrants, companies",
                          "stock_sociedades": "Company stock",
                          "legibilidad_total_x1000":
                              "Registrants per 1,000 companies",
                          "legibilidad_soc_x1000":
                              "Registered companies per 1,000"},
                 pct=("share_anclado",),
                 digits={"share_anclado": 1, "registrados": 0,
                         "registrados_sociedades": 0, "stock_sociedades": 0,
                         "legibilidad_total_x1000": 1,
                         "legibilidad_soc_x1000": 1})
        + "\n\n" + md_table("tab_legibilidad_cor.csv",
                            renames={"prueba": "Test", "rho": "*ρ*",
                                     "p": "*p*"},
                            digits={"rho": 3, "p": 3}),
        "Ordered by anchored demand. Registrants are the suppliers domiciled "
        "in the jurisdiction that appear in the national register, whether or "
        "not they have won an award; the denominator is the provincial company "
        "stock in both rates, since the tax register carries no natural "
        "persons.")

    B["S24"] = block(
        "S24", "The demand-composition counterfactual.",
        md_table("tab_contrafactual_composicion.csv",
                 renames={"provincia": "Jurisdiction",
                          "p_obs": "Observed, awards to natural persons (%)",
                          "p_cf": "Counterfactual (%)",
                          "tipos_presentes": "Types of buyer present",
                          "anclado": "Anchored demand (%)"},
                 pct=("p_obs", "p_cf", "anclado"),
                 digits={"p_obs": 1, "p_cf": 1, "tipos_presentes": 0,
                         "anclado": 1},
                 sort=lambda r: -float(r["anclado"]))
        + "\n\n" + md_table("tab_contrafactual_pi_anclaje.csv",
                            renames={"provincia": "Jurisdiction",
                                     "n_anclado": "Awards, anchored organs",
                                     "n_no_anclado": "Awards, other organs",
                                     "pi_anclado":
                                         "Anchored organs buying personally (%)",
                                     "pi_no_anclado":
                                         "Other organs buying personally (%)",
                                     "anclado": "Anchored demand (%)"},
                            pct=("pi_anclado", "pi_no_anclado", "anclado"),
                            digits={"n_anclado": 0, "n_no_anclado": 0,
                                    "pi_anclado": 1, "pi_no_anclado": 1,
                                    "anclado": 1},
                            sort=lambda r: -float(r["anclado"]))
        + "\n\n" + md_table("tab_contrafactual_resumen.csv",
                            renames={"medida": "Quantity", "valor": "Value"},
                            digits={"valor": 3}),
        "Each jurisdiction's share of awards to natural persons is recomputed "
        "with its own response rates by type of buyer but the national mix of "
        "types. The unit here is the award, so levels differ by construction "
        "from the supplier-based shares of Table 2.")

    B["S25"] = block(
        "S25", "The fall in direct contracting, decomposed.",
        md_table("tab_shiftshare_directa.csv",
                 renames={"componente": "Component", "valor": "Value"},
                 digits={"valor": 1})
        + "\n\n" + md_table("tab_shiftshare_directa_base.csv",
                            renames={"ejercicio": "Year", "tipo": "Organs",
                                     "n": "Awards",
                                     "s": "Direct contracting (%)",
                                     "w": "Share of awards (%)"},
                            pct=("s", "w"),
                            digits={"ejercicio": 0, "n": 0, "s": 1, "w": 1},
                            maps={"tipo": {"anclado": "Anchored",
                                           "no_anclado": "Other"}}),
        "Period-mean weights, so the composition and within components sum "
        "exactly to the total change.")

    B["S26"] = block(
        "S26", "The distance of the exchange.",
        md_table("tab_distancia_categorias.csv",
                 renames={"variable": "Variable", "categoria": "Category",
                          "n_adj": "Awards", "km_mediana": "Median km",
                          "km_p25": "25th percentile",
                          "km_p75": "75th percentile"},
                 digits={"n_adj": 0, "km_mediana": 0, "km_p25": 0,
                         "km_p75": 0},
                 maps={"variable": VAR, "categoria": CAT})
        + "\n\n" + md_table("tab_distancia_organos.csv",
                            renames={"variable": "Variable",
                                     "categoria": "Category",
                                     "n_adj": "Awards",
                                     "km_mediana": "Median km",
                                     "km_p25": "25th percentile",
                                     "km_p75": "75th percentile"},
                            digits={"n_adj": 0, "km_mediana": 0,
                                    "km_p25": 0, "km_p75": 0},
                            maps={"categoria": {**TIPO, **ANCLA},
                                  "variable": {"anclado": "Anchoring",
                                               "tipo_organismo":
                                                   "Type of buying organ"}})
        + "\n\n" + md_table("tab_distancia_estratos.csv",
                            renames={"variable": "Variable",
                                     "categoria": "Category",
                                     "n_adj": "Awards",
                                     "km_mediana": "Median km",
                                     "km_p25": "25th percentile",
                                     "km_p75": "75th percentile"},
                            digits={"n_adj": 0, "km_mediana": 0,
                                    "km_p25": 0, "km_p75": 0},
                            maps={"categoria": STRATUM})
        + "\n\n" + md_table("tab_distancia_puerta_local.csv",
                            renames={"anclado": "Organs",
                                     "n_adj": "Awards",
                                     "share_local_25km": "Within 25 km (%)",
                                     "share_100km": "Within 100 km (%)",
                                     "km_mediana": "Median km"},
                            pct=("share_local_25km", "share_100km"),
                            digits={"n_adj": 0, "share_local_25km": 1,
                                    "share_100km": 1, "km_mediana": 0},
                            maps={"anclado": {"anclado": "Anchored",
                                              "no_anclado": "Other"}}),
        "Great-circle distance between the seat of the purchasing unit and "
        "the supplier's registered locality. Distance at zero is co-location "
        "at the grain of the locality. Both biases of the measure — "
        "centralised purchasing offices and centralised fiscal registration — "
        "pull the corporate pole towards the metropolis.")

    B["S27"] = block(
        "S27", "Trajectories in the space.",
        md_table("tab_trayectorias_grupos.csv",
                 renames={"grupo_ancla": "Client at entry",
                          "grupo_llega": "Observed under the third presidency",
                          "n": "Suppliers",
                          "abs_d2_mediana": "Median |displacement|, axis 2",
                          "d2_mediana": "Median signed displacement",
                          "hacia_corporativo":
                              "Moving towards the corporate pole (%)"},
                 pct=("hacia_corporativo",),
                 digits={"n": 0, "abs_d2_mediana": 3, "d2_mediana": 3,
                         "hacia_corporativo": 1},
                 maps={"grupo_ancla": {"anclado": "Anchored organ",
                                       "no_anclado": "Other"},
                       "grupo_llega": {"llega_a_milei": "Yes",
                                       "no_llega": "No"}})
        + "\n\n" + md_table("tab_trayectorias_estrato.csv",
                            renames={"estrato": "Stratum", "n": "Suppliers",
                                     "abs_d2_mediana":
                                         "Median |displacement|, axis 2",
                                     "d2_mediana":
                                         "Median signed displacement"},
                            digits={"n": 0, "abs_d2_mediana": 3,
                                    "d2_mediana": 3},
                            maps={"estrato": STRATUM})
        + "\n\n" + md_table("tab_trayectorias_quietud.csv",
                            renames={"grupo_ancla": "Client at entry",
                                     "n": "Suppliers",
                                     "quieto_025sd":
                                         "Within a quarter of an axis SD (%)"},
                            pct=("quieto_025sd",),
                            digits={"n": 0, "quieto_025sd": 1},
                            maps={"grupo_ancla": {
                                "anclado": "Anchored organ",
                                "no_anclado": "Other"}})
        + "\n\n" + md_table("tab_trayectorias_flechas.csv",
                            renames={"grupo": "Group", "n": "Suppliers",
                                     "x0": "Axis 1, first presidency",
                                     "y0": "Axis 2, first presidency",
                                     "x1": "Axis 1, last presidency",
                                     "y1": "Axis 2, last presidency"},
                            digits={"n": 0, "x0": 3, "y0": 3, "x1": 3,
                                    "y1": 3}),
        "Each supplier active in two or more presidencies is projected as a "
        "supplementary individual from its era-specific profile. The axis "
        "standard deviation is 0.51. The last panel gives the mean positions "
        "behind the arrows of Figure 5.")

    B["S28"] = block(
        "S28", "The ground of the exception, by the anchoring of the buying "
        "organ and the legal form of the supplier.",
        md_table("tab_consagracion_2x2.csv",
                 renames={"comprador": "Buying organ",
                          "proveedor": "Supplier",
                          "n": "Awards carrying a ground",
                          "pct_cualitativo": "On a qualitative ground (%)",
                          "lo": "95% CI lower", "hi": "95% CI upper"},
                 digits={"n": 0, "pct_cualitativo": 2, "lo": 2, "hi": 2},
                 maps={"comprador": {"anclado": "Territorially anchored",
                                     "programatico": "Programmatic"},
                       "proveedor": {"persona_fisica": "Natural person",
                                     "sociedad": "Company"}}),
        "A qualitative ground is exclusivity or specialty, where the state "
        "declares the supplier the only one able to supply or its competence "
        "the one required; the alternative in almost every remaining case is "
        "the amount, an exemption settled by a threshold. Percentages are of "
        "the awards in the cell that carry any ground, since the field exists "
        "only where open competition was departed from. Intervals are a "
        "cluster bootstrap resampling suppliers (*B* = 1,000), because awards "
        "are nested in suppliers and the cells are dominated by a few "
        "high-volume ones.")

    FAMILIA = {"cualitativo": "Exclusivity or specialty",
               "cuantitativo": "The amount",
               "interadministrativo": "Another public body",
               "otros": "Residue"}
    B["S28a"] = block(
        "S28a", "The grounds of the exception as recorded, and their "
        "grouping into families.",
        md_table("tab_apartado_crosswalk.csv",
                 renames={"apartado_raw": "Label as recorded",
                          "apartado_num": "Clause",
                          "familia": "Family",
                          "n_filas": "Rows in the record"},
                 digits={"n_filas": 0},
                 maps={"familia": FAMILIA},
                 sort=lambda r: (int(r["apartado_num"]), r["apartado_raw"])),
        "Rows count the full record in both vocabularies; the lettered labels "
        "are the older vocabulary and reproduce the numbered ones. Ten clauses "
        "of the procurement decree appear, under fourteen distinct labels in "
        "the window once case is ignored. The residue gathers urgency, "
        "emergency, property leases, dismantling or prior inspection, a failed "
        "or deserted tender, and the register of local development and "
        "social-economy producers.")

    B["S28b"] = block(
        "S28b", "The ground of the exception by family, by the anchoring of "
        "the buying organ.",
        md_table("tab_consagracion_familia_anclaje.csv",
                 renames={"comprador": "Buying organ", "familia": "Family",
                          "n": "Awards", "pct": "Share of exempt awards (%)"},
                 digits={"n": 0, "pct": 2},
                 maps={"comprador": {"anclado": "Territorially anchored",
                                     "programatico": "Programmatic"},
                       "familia": FAMILIA}),
        "Percentages are of the awards carrying a ground within each kind of "
        "organ. Anchored organs settle 91.7 per cent of their exempt awards on "
        "the amount alone, programmatic ones 67.7 per cent.")

    B["S29"] = block(
        "S29", "The same comparison within each sector.",
        md_table("tab_consagracion_rubro.csv",
                 cols=["rubro_principal", "n", "pct_pf", "pct_soc"],
                 renames={"rubro_principal": "Sector",
                          "n": "Awards carrying a ground",
                          "pct_pf": "Natural persons (%)",
                          "pct_soc": "Companies (%)"},
                 digits={"n": 0, "pct_pf": 2, "pct_soc": 2},
                 sort=lambda r: -float(r["n"])),
        "Sectors with at least 300 awards carrying a ground. Companies carry "
        "the qualitative ground more often than natural persons in 21 of the "
        "26, so the ordering of Table S28 is not an artefact of what each "
        "kind of supplier sells. Professional services is the one sector "
        "where the two are level.")

    B["S30"] = block(
        "S30", "Suppliers the state has named, by jurisdiction.",
        md_table("tab_consagracion_jurisdiccion.csv",
                 renames={"provincia": "Jurisdiction",
                          "proveedores": "Suppliers",
                          "pct_consagrados": "Ever named (%)",
                          "anclado": "Anchored demand"},
                 digits={"proveedores": 0, "pct_consagrados": 2,
                         "anclado": 3},
                 sort=lambda r: float(r["anclado"])),
        "A supplier counts as named if at least one of its awards was granted "
        "on exclusivity or specialty. Spearman's ρ against anchored demand is "
        "−0.557 (*p* = 0.005) over the 24 jurisdictions, and −0.556 "
        "(*p* = 0.006) with each jurisdiction's leading supplier removed.")

    B["S31"] = block(
        "S31", "The demand-composition counterfactual for the qualitative "
        "ground.",
        md_table("tab_consagracion_contrafactual.csv",
                 renames={"provincia": "Jurisdiction",
                          "p_obs": "Observed (%)",
                          "p_cf": "At the national mix of buyer types (%)",
                          "tipos_presentes": "Buyer types present",
                          "anclado": "Anchored demand"},
                 digits={"p_obs": 2, "p_cf": 2, "tipos_presentes": 0,
                         "anclado": 3},
                 sort=lambda r: float(r["anclado"])),
        "Each jurisdiction keeps its own response within each type of buying "
        "organ and receives the national mix of types, following the "
        "procedure of Table S24. Spearman's ρ against anchored demand falls "
        "from −0.397 observed to 0.002 counterfactual, so the territorial "
        "gradient in the qualitative ground is composition. The gradient in "
        "the legal form of the supplier behaves in the opposite way and "
        "survives the same reweighting.")

    B["S32"] = block(
        "S32", "The register of justification across the three presidencies.",
        md_table("tab_consagracion_era.csv",
                 renames={"era": "Presidency",
                          "n": "Awards carrying a ground",
                          "pct_cualitativo": "Qualitative ground (%)",
                          "pct_aritmetico": "Ground of the amount (%)",
                          "pct_interadmin": "Another public body (%)",
                          "pct_con_fundamento":
                              "Awards carrying any ground (%)"},
                 digits={"n": 0, "pct_cualitativo": 2, "pct_aritmetico": 2,
                         "pct_interadmin": 2, "pct_con_fundamento": 2},
                 maps={"era": {"macri": "Macri (2019)",
                               "fernandez": "Fernández (2020–23)",
                               "milei": "Milei (2024–25)"}}),
        "The first three columns are percentages of the awards carrying a "
        "ground in that period; the last is the percentage of all of the "
        "period's awards that carry one. The Macri period is represented by "
        "2019 alone, for the reason given in Section 3.")

    B["S33"] = block(
        "S33", "The per-province comparison at a threshold of 100 suppliers.",
        md_table("tab_rv_size_control_n100.csv",
                 cols=["provincia", "estrato", "n_proveedores", "rv_observado",
                       "rv_mediano_n150", "ic_lo", "ic_hi"],
                 renames={"provincia": "Province", "estrato": "Stratum",
                          "n_proveedores": "Suppliers",
                          "rv_observado": "RV observed",
                          "rv_mediano_n150": "RV at n = 100",
                          "ic_lo": "95% CI lower", "ic_hi": "95% CI upper"},
                 digits={"n_proveedores": 0, "rv_observado": 3,
                         "rv_mediano_n150": 3, "ic_lo": 3, "ic_hi": 3},
                 maps={"estrato": {"core": "Core", "middle": "Middle",
                                   "outer": "Outer"}},
                 sort=lambda r: -float(r["rv_observado"])),
        "The comparison of Tables S11 and S13 repeated with provinces admitted "
        "at 100 adjudicated suppliers rather than 150, which raises the count "
        "from twelve to eighteen and the peripheral ones from three to six. "
        "The observed coefficients still order by stratum and still track "
        "size (Spearman's ρ = 0.525, *p* = 0.027); drawing every province down "
        "to 100 removes both the ordering (Kruskal-Wallis *p* = 0.496) and the "
        "association with size (ρ = −0.211). The canonical run stays at 150, "
        "since equalising at 100 weakens the estimate for every province.")

    B["S34"] = block(
        "S34", "The weight of the axes in each provincial subspace.",
        md_table("tab_mca_por_provincia.csv",
                 cols=["provincia", "estrato", "n_proveedores",
                       "benzecri_dim1", "benzecri_dim2", "dim_que_matchea"],
                 renames={"provincia": "Province", "estrato": "Stratum",
                          "n_proveedores": "Suppliers",
                          "benzecri_dim1": "First axis (%)",
                          "benzecri_dim2": "Second axis (%)",
                          "dim_que_matchea":
                              "Province axis matching the national first"},
                 digits={"n_proveedores": 0, "benzecri_dim1": 1,
                         "benzecri_dim2": 1, "dim_que_matchea": 0},
                 maps={"estrato": {"core": "Core", "middle": "Middle",
                                   "outer": "Outer"}},
                 sort=lambda r: -float(r["benzecri_dim1"])),
        "Modified rates, each province refitted on its own. The two axes carry "
        "different weights from one province to the next: the ratio of the "
        "first rate to the second runs from 1.26 in Córdoba to 3.35 in "
        "Mendoza. That variation does not follow the territorial gradient. "
        "The core spans almost the whole range (1.26 to 3.35) and the outer "
        "stratum sits inside it (1.44 in Corrientes to 2.95 in Misiones), so "
        "the hierarchy of the axes varies between provinces without ordering "
        "them. The last column gives which of a province's own axes the "
        "national first axis matches: the first in eleven of the twelve, and "
        "the second only in the federal capital.")

    B["S35"] = block(
        "S35", "The return on the legal form, within sector and stratum.",
        md_table("tab_retorno_forma.csv",
                 cols=["A_rubro", "estrato", "n_soc", "n_pf", "med_soc",
                       "med_pf", "cociente"],
                 renames={"A_rubro": "Sector", "estrato": "Stratum",
                          "n_soc": "Companies", "n_pf": "Sole proprietors",
                          "med_soc": "Median award, companies",
                          "med_pf": "Median award, sole proprietors",
                          "cociente": "Ratio"},
                 digits={"n_soc": 0, "n_pf": 0, "med_soc": 0, "med_pf": 0,
                         "cociente": 2},
                 maps={"estrato": {"core": "Core", "middle": "Middle",
                                   "outer": "Outer"}}),
        "Median award in constant 2024 pesos, for the cells holding at least "
        "fifteen suppliers of each legal form. Companies transact above sole "
        "proprietors in 21 of the 22 cells, at a median ratio of 2.29, and the "
        "return is larger at the core (3.50) than in the middle (1.92) or "
        "outer (1.51) stratum. The comparison holds the sector and the "
        "stratum fixed, so neither the sectoral composition of supply nor the "
        "territorial gradient produces it. This is the measured return that "
        "warrants calling the legal form a capital rather than a coordinate: "
        "differentiating positions cannot be the criterion, since every active "
        "variable of a geometric analysis does that by construction.")

    return B


# ---------------------------------------------------------------------------
# Injection: strip old blocks, put each one after the paragraph that first
# names its table. Idempotent by construction.
# ---------------------------------------------------------------------------
BLOCK_RE = re.compile(
    r"\n*<!-- TABLE:[^>]*? -->.*?<!-- /TABLE:[^>]*? -->\n*", re.S)
PARA_SEP = "\n\n"


def _natural(key):
    """S6b -> (6, 'b'), so tables sort the way a reader numbers them."""
    m = re.fullmatch(r"S(\d+)([a-z]*)", key)
    return (int(m.group(1)), m.group(2)) if m else (999, key)


def inject(text, blocks):
    text = BLOCK_RE.sub(PARA_SEP, text)
    paras = text.split(PARA_SEP)
    # Anchors are resolved against the original paragraph list and only then
    # inserted, all in one pass. Inserting as we went would renumber the list
    # under the later lookups and scramble the order inside a section.
    anchors, missing = {}, []
    for key in sorted(blocks, key=lambda k: (-len(k), k)):
        pat = re.compile(r"\bTables?\s+" + key + r"\b")
        hit = None
        for i, para in enumerate(paras):
            s = para.lstrip()
            # the S0 sources table names Table S3 and Table S26 in its cells,
            # and a table row is not a paragraph that introduces a table
            if (s.startswith("<!-- TABLE:") or s.startswith("|")
                    or s.startswith("**Table")):
                continue
            if pat.search(para):
                hit = i
                break
        if hit is None:
            missing.append(key)
        else:
            anchors[key] = hit
    out, placed = [], set()
    pending = sorted(anchors.items(), key=lambda kv: (kv[1], _natural(kv[0])))
    for i, para in enumerate(paras):
        out.append(para)
        for key, idx in pending:
            if idx == i:
                out.append(blocks[key])
                placed.add(key)
    return PARA_SEP.join(out), placed, missing


if __name__ == "__main__":
    blocks = build()
    text = SI.read_text(encoding="utf-8")
    new, placed, missing = inject(text, blocks)
    SI.write_text(new, encoding="utf-8")
    print(f"tablas escritas: {len(placed)} de {len(blocks)}")
    if missing:
        print("SIN ANCLA en la prosa del SI:", ", ".join(sorted(missing)))
