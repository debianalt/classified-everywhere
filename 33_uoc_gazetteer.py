"""33 — Gacetero nacional de sedes de unidades compradoras (29 ago 2026).

Ubica cada unidad operativa de contrataciones (UOC) en su sede física, para
el panel de puntos de la Figura 2. Pipeline híbrido, cada capa archivada:

  1. prefill DeepSeek (deepseek-chat, temperatura 0) sobre los pares
     UOC x organismo 2019-2025, en tandas -> data/raw/uoc_sedes_deepseek.json
  2. coordenadas oficiales del gacetero de localidades georef
     (apis.datos.gob.ar) -> data/raw/georef_localidades.json
  3. Nominatim/OSM (1 req/s, politica de uso) solo para lo que 1-2 no
     resuelven -> data/raw/uoc_sedes_nominatim.json
  4. correcciones y verificaciones manuales, con fuente citada, en
     data/raw/covariates/uoc_sedes_manual.csv (se aplican al final; el
     crudo de las APIs nunca se toca a mano)

Salida: data/raw/uoc_gazetteer.csv (uoc, saf, tipo_organismo, n_adj,
localidad, provincia, lat, lon, confianza, fuente, verificado).

CAVEAT (va tambien en el caption): la sede de la UOC es donde el Estado
ejerce la compra, no el punto de entrega; una jefatura centralizada compra
para destinos de todo el pais.

Run: python 33_uoc_gazetteer.py
"""
import io
import json
import os
import re
import sys
import time
import unicodedata
import urllib.parse
import urllib.request
import winreg
from pathlib import Path

import pandas as pd

PROJECT = Path(__file__).resolve().parent
RAW = PROJECT / "data" / "raw"
PROC = PROJECT / "data" / "processed"
COV = RAW / "covariates"
WIN0, WIN1 = 2019, 2025
UA = {"User-Agent": "Mozilla/5.0 (academic research; procurement gazetteer)"}


def get_key():
    k = os.environ.get("DEEPSEEK_API_KEY")
    if k:
        return k
    try:
        with winreg.OpenKey(winreg.HKEY_CURRENT_USER, "Environment") as h:
            return winreg.QueryValueEx(h, "DEEPSEEK_API_KEY")[0]
    except OSError:
        pass
    f = Path.home() / ".deepseek_key"
    if f.exists():
        return f.read_text(encoding="utf-8").strip()
    # a .env beside the scripts, as a last resort
    env = PROJECT / ".env"
    if env.exists():
        for line in env.read_text(encoding="utf-8").splitlines():
            if line.startswith("DEEPSEEK_API_KEY="):
                return line.split("=", 1)[1].strip()
    sys.exit("DEEPSEEK_API_KEY no encontrada")


def norm(s):
    s = "".join(c for c in unicodedata.normalize("NFD", str(s))
                if unicodedata.category(c) != "Mn")
    return re.sub(r"\s+", " ", s).upper().strip()


# ── universo: pares UOC x SAF con volumen y tipo de organismo ────────────────
raw = pd.read_csv(RAW / "comprar_adjudicaciones_2016_2026.csv", dtype=str,
                  usecols=["Descripcion_UOC", "Descripcion_SAF", "Ejercicio"])
raw = raw[raw["Ejercicio"].astype(float).between(WIN0, WIN1)]
u = (raw.groupby(["Descripcion_UOC", "Descripcion_SAF"]).size()
        .reset_index(name="n_adj")
        .sort_values("n_adj", ascending=False).reset_index(drop=True))

tipos = (pd.read_parquet(PROC / "adjudicaciones_tipo.parquet",
                         columns=["organismo", "tipo_organismo"])
         .drop_duplicates())
u = u.merge(tipos, left_on="Descripcion_SAF", right_on="organismo",
            how="left").drop(columns=["organismo"])
print(f"pares UOC x SAF: {len(u)} | adjudicaciones: {u['n_adj'].sum():,} | "
      f"sin tipo: {int(u['tipo_organismo'].isna().sum())}")

# ── 1. prefill DeepSeek, en tandas, cacheado ─────────────────────────────────
DS_PATH = RAW / "uoc_sedes_deepseek.json"
SYSTEM = ("Sos un experto en administracion publica argentina y en la "
          "estructura territorial de sus fuerzas armadas y organismos. Se te "
          "pide la SEDE FISICA (localidad y provincia) de unidades operativas "
          "de contrataciones del estado nacional. Respondes SOLO un array "
          "JSON, un objeto por unidad: {\"i\": numero, \"localidad\": str, "
          "\"provincia\": str, \"confianza\": \"alta\"|\"media\"|\"baja\", "
          "\"base\": str corta}. Si no sabes, localidad null y confianza "
          "baja. CABA se escribe \"CABA\". No inventes: una sede plausible "
          "pero no confirmada es confianza media o baja.")

if DS_PATH.exists():
    seats = json.loads(DS_PATH.read_text(encoding="utf-8"))
    print(f"prefill DeepSeek: cache ({len(seats)} sedes)")
else:
    seats, B = [], 90
    for lo in range(0, len(u), B):
        chunk = u.iloc[lo:lo + B]
        listado = "\n".join(
            f"{i + 1}. [{r['n_adj']}] {r['Descripcion_UOC']} | organismo: "
            f"{r['Descripcion_SAF']}"
            for i, (_, r) in zip(range(lo, lo + len(chunk)),
                                 chunk.iterrows()))
        payload = {"model": "deepseek-chat",
                   "messages": [
                       {"role": "system", "content": SYSTEM},
                       {"role": "user", "content":
                        "Sedes fisicas de estas unidades operativas de "
                        "contrataciones (numero entre corchetes = "
                        "adjudicaciones 2019-2025):\n\n" + listado}],
                   "temperature": 0, "max_tokens": 8000}
        req = urllib.request.Request(
            "https://api.deepseek.com/chat/completions",
            data=json.dumps(payload).encode("utf-8"),
            headers={"Content-Type": "application/json",
                     "Authorization": f"Bearer {get_key()}"})
        print(f"  deepseek tanda {lo // B + 1} "
              f"({lo + 1}-{lo + len(chunk)})...", flush=True)
        with urllib.request.urlopen(req, timeout=900) as r:
            resp = json.load(r)
        txt = resp["choices"][0]["message"]["content"].strip().strip("`")
        if txt.startswith("json"):
            txt = txt[4:]
        seats.extend(json.loads(txt))
    DS_PATH.write_text(json.dumps(seats, ensure_ascii=False),
                       encoding="utf-8")
    print(f"prefill DeepSeek: {len(seats)} sedes -> {DS_PATH.name}")

ds = {s["i"]: s for s in seats}

# ── 2. gacetero georef de localidades ────────────────────────────────────────
GEO_PATH = RAW / "georef_localidades.json"
if GEO_PATH.exists():
    locs = json.loads(GEO_PATH.read_text(encoding="utf-8"))
else:
    url = ("https://apis.datos.gob.ar/georef/api/localidades"
           "?campos=nombre,provincia.nombre,centroide.lat,centroide.lon"
           "&max=5000")
    with urllib.request.urlopen(urllib.request.Request(url, headers=UA),
                                timeout=120) as r:
        locs = json.load(r)["localidades"]
    GEO_PATH.write_text(json.dumps(locs, ensure_ascii=False),
                        encoding="utf-8")
print(f"georef localidades: {len(locs)}")

geo = {}
for l in locs:
    key = (norm(l["nombre"]), norm(l["provincia"]["nombre"]))
    geo.setdefault(key, (l["centroide"]["lat"], l["centroide"]["lon"]))

PROV_FIX = {"CABA": "CIUDAD AUTONOMA DE BUENOS AIRES",
            "CIUDAD DE BUENOS AIRES": "CIUDAD AUTONOMA DE BUENOS AIRES",
            "CAPITAL FEDERAL": "CIUDAD AUTONOMA DE BUENOS AIRES",
            "TIERRA DEL FUEGO":
            "TIERRA DEL FUEGO, ANTARTIDA E ISLAS DEL ATLANTICO SUR"}
CABA_LATLON = (-34.6075, -58.4371)


def lookup_georef(localidad, provincia):
    if not localidad:
        return None
    lp, pp = norm(localidad), norm(provincia or "")
    pp = PROV_FIX.get(pp, pp)
    if lp == "CABA" or pp == "CIUDAD AUTONOMA DE BUENOS AIRES":
        return CABA_LATLON
    hit = geo.get((lp, pp))
    if hit:
        return hit
    cands = [(k, v) for k, v in geo.items() if k[0] == lp]
    if len(cands) == 1:
        return cands[0][1]
    pref = [(k, v) for k, v in cands if k[0] == lp and pp and pp in k[1]]
    return pref[0][1] if len(pref) == 1 else None


# ── 3. Nominatim para lo no resuelto ─────────────────────────────────────────
NOM_PATH = RAW / "uoc_sedes_nominatim.json"
nom_cache = (json.loads(NOM_PATH.read_text(encoding="utf-8"))
             if NOM_PATH.exists() else {})


def nominatim(q):
    if q in nom_cache:
        return nom_cache[q]
    url = ("https://nominatim.openstreetmap.org/search?format=json&limit=1"
           "&countrycodes=ar&q=" + urllib.parse.quote(q))
    try:
        with urllib.request.urlopen(urllib.request.Request(url, headers=UA),
                                    timeout=30) as r:
            d = json.load(r)
        nom_cache[q] = ([d[0]["lat"], d[0]["lon"], d[0]["display_name"]]
                        if d else None)
    except Exception:
        nom_cache[q] = None
    time.sleep(1.1)
    return nom_cache[q]


# ── ensamblado ───────────────────────────────────────────────────────────────
rows = []
for i, (_, r) in enumerate(u.iterrows(), start=1):
    s = ds.get(i, {})
    loc, prov = s.get("localidad"), s.get("provincia")
    conf, fuente = s.get("confianza", "baja"), "deepseek+georef"
    latlon = lookup_georef(loc, prov)
    if latlon is None and loc:
        hit = nominatim(f"{loc}, {prov}, Argentina")
        if hit:
            latlon, fuente = (float(hit[0]), float(hit[1])), "deepseek+nominatim"
    if latlon is None:
        hit = nominatim(f"{r['Descripcion_UOC']}, Argentina")
        if hit:
            latlon, fuente, conf = ((float(hit[0]), float(hit[1])),
                                    "nominatim", "media")
    rows.append({
        "uoc": r["Descripcion_UOC"], "saf": r["Descripcion_SAF"],
        "tipo_organismo": r["tipo_organismo"], "n_adj": r["n_adj"],
        "localidad": loc, "provincia": prov,
        "lat": latlon[0] if latlon else None,
        "lon": latlon[1] if latlon else None,
        "confianza": conf, "fuente": fuente if latlon else "sin_resolver",
        "verificado": 0})
NOM_PATH.write_text(json.dumps(nom_cache, ensure_ascii=False),
                    encoding="utf-8")

g = pd.DataFrame(rows)

# ── 3b. regla de denominacion: la unidad nombrada por su propia sede ─────────
# "7° Santa Fe - DNV", "INTENDENCIA NAVAL MAR DEL PLATA": cuando el nombre de
# la UOC contiene la localidad asignada (5+ caracteres), la sede queda
# verificada por la denominacion misma.
den = (g["localidad"].notna()
       & (g["localidad"].map(lambda s: len(norm(s))) >= 5)
       & g.apply(lambda r: norm(r["localidad"]) in norm(r["uoc"]), axis=1))
g.loc[den, "verificado"] = 1
g.loc[den, "confianza"] = "alta"
g.loc[den & (g["fuente"] != "manual"), "fuente"] = "denominacion"
print(f"regla de denominacion: {int(den.sum())} pares verificados")

# ── 4. correcciones y verificaciones manuales ────────────────────────────────
MAN_PATH = COV / "uoc_sedes_manual.csv"
if MAN_PATH.exists():
    man = pd.read_csv(MAN_PATH, dtype=str)
    n_fix = 0
    for _, m in man.iterrows():
        sel = g["uoc"].map(norm) == norm(m["uoc"])
        if not sel.any():
            print(f"  AVISO manual sin match: {m['uoc'][:50]}")
            continue
        if isinstance(m.get("localidad"), str) and m["localidad"]:
            g.loc[sel, "localidad"] = m["localidad"]
            g.loc[sel, "provincia"] = m["provincia"]
            latlon = lookup_georef(m["localidad"], m["provincia"])
            if latlon:
                g.loc[sel, ["lat", "lon"]] = latlon
            g.loc[sel, "fuente"] = "manual"
        g.loc[sel, "verificado"] = 1
        g.loc[sel, "confianza"] = "alta"
        n_fix += sel.sum()
    print(f"manual: {len(man)} filas aplicadas sobre {n_fix} pares")

# ── 5. chequeo: ¿cada sede está donde georef pone la localidad que declara? ──
# 3 sep 2026: el Escuadrón 1 "Roque Sáenz Peña" llevaba el nombre corto del
# prefill; georef no lo resolvía, Nominatim devolvió un punto al lado de
# Resistencia (161 km de Presidencia Roque Sáenz Peña) y la regla de
# denominación lo marcó verificado. Toda sede a más de 20 km del centroide
# georef de su localidad (por nombre exacto, o por nombre contenido si es único
# en la provincia) se lista acá; la lista tiene que quedar vacía o explicada en
# el archivo manual.
def _hav(a, b):
    from math import radians, sin, cos, asin, sqrt
    la1, lo1, la2, lo2 = map(radians, (a[0], a[1], b[0], b[1]))
    h = sin((la2 - la1) / 2) ** 2 + cos(la1) * cos(la2) * sin((lo2 - lo1) / 2) ** 2
    return 2 * 6371 * asin(sqrt(h))


def _ref_georef(localidad, provincia):
    hit = lookup_georef(localidad, provincia)
    if hit:
        return hit
    lp, pp = norm(localidad), norm(provincia or "")
    pp = PROV_FIX.get(pp, pp)
    if len(lp) < 5:
        return None
    cands = [v for k, v in geo.items() if lp in k[0] and (not pp or pp in k[1])]
    return cands[0] if len(cands) == 1 else None


lejos, sin_ref = [], []
for _, r in g[g["lat"].notna() & g["localidad"].notna()].iterrows():
    ref = _ref_georef(r["localidad"], r["provincia"])
    if ref is None:
        sin_ref.append(f"{r['localidad']} ({r['provincia']})")
        continue
    d = _hav((float(r["lat"]), float(r["lon"])), (float(ref[0]), float(ref[1])))
    if d > 20:
        lejos.append((r["uoc"][:48], r["localidad"], r["provincia"], round(d)))
print(f"\nchequeo sede vs georef de su localidad: {len(lejos)} a más de 20 km, "
      f"{len(sin_ref)} sin referencia georef")
for t in lejos:
    print("  ", *t)
if sin_ref:
    print("  sin referencia georef (Nominatim o nombre no estándar):",
          "; ".join(sorted(set(sin_ref))))

g.to_csv(RAW / "uoc_gazetteer.csv", index=False, encoding="utf-8")

tot = g["n_adj"].sum()
con = g[g["lat"].notna()]
ver = g[g["verificado"] == 1]
caba = con[(con["localidad"].fillna("").map(norm) == "CABA")]
print(f"\ngazetteer: {len(g)} pares | con sede: {len(con)} "
      f"({con['n_adj'].sum() / tot:.1%} de adjudicaciones)")
print(f"  verificado a mano: {len(ver)} ({ver['n_adj'].sum() / tot:.1%})")
print(f"  en CABA: {len(caba)} pares ({caba['n_adj'].sum() / tot:.1%})")
print(f"  sin resolver: {int(g['lat'].isna().sum())} "
      f"({g.loc[g['lat'].isna(), 'n_adj'].sum() / tot:.1%})")
print("\nconfianza (pares):")
print(g.groupby("confianza")["n_adj"].agg(["count", "sum"]))
