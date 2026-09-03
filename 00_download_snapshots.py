"""Archive procurement open-data snapshots to data/raw/.

COMPR.AR updates are semi-annual and discretionary; CONTRAT.AR, Mapa
Inversiones and Vialidad OCDS were already discontinued — archive now,
don't assume availability in 2027. URLs verified alive 27-08-2026.
"""
import json
import sys
import urllib.request
from pathlib import Path

RAW = Path(__file__).parent / "data" / "raw"
RAW.mkdir(parents=True, exist_ok=True)

DIRECT = {
    "comprar_convocatorias_2016_2026.csv":
        "https://infra.datos.gob.ar/catalog/jgm/dataset/4/distribution/4.21/download/Convocatorias.csv",
    "comprar_adjudicaciones_2016_2026.csv":
        "https://infra.datos.gob.ar/catalog/jgm/dataset/4/distribution/4.22/download/Adjudicaciones.csv",
    "sipro_proveedores.csv":
        "https://infra.datos.gob.ar/catalog/jgm/dataset/4/distribution/4.23/download/SiPRO.csv",
    "arca_padron_denominacion.zip":
        "https://www.afip.gob.ar/genericos/cInscripcion/archivos/apellidoNombreDenominacion.zip",
    "mapainversiones_obras.csv":
        "https://mapainversiones.obraspublicas.gob.ar/opendata/dataset_mop.csv",
    "mapainversiones_proyectos.csv":
        "https://mapainversiones.obraspublicas.gob.ar/opendata/dataset_proyectosmop.csv",
    # province polygons (georef/IGN service) for the choropleth figure; the
    # modernizacion 7.6 distribution that used to serve these returns 404
    "georef_provincias.geojson":
        "https://infra.datos.gob.ar/georef/provincias.geojson",
}

CKAN = "https://datos.gob.ar/api/3/action/package_show?id={}"
CONTRATAR_DATASETS = [
    "procesos-de-contratacion-de-la-obra-publica-gestionados-en-la-plataforma-contratar",
    "contratar-historico",
]

UA = {"User-Agent": "Mozilla/5.0 (research archival; CONICET)"}


def fetch(url: str, dest: Path) -> None:
    if dest.exists() and dest.stat().st_size > 0:
        print(f"skip (exists): {dest.name}")
        return
    req = urllib.request.Request(url, headers=UA)
    with urllib.request.urlopen(req, timeout=300) as r, open(dest, "wb") as f:
        while chunk := r.read(1 << 20):
            f.write(chunk)
    print(f"ok: {dest.name} ({dest.stat().st_size / 1e6:.1f} MB)")


failures = []
for name, url in DIRECT.items():
    try:
        fetch(url, RAW / name)
    except Exception as e:
        failures.append((name, str(e)))
        print(f"FAIL: {name}: {e}", file=sys.stderr)

for ds in CONTRATAR_DATASETS:
    try:
        req = urllib.request.Request(CKAN.format(ds), headers=UA)
        pkg = json.load(urllib.request.urlopen(req, timeout=60))
        for res in pkg["result"]["resources"]:
            if res.get("format", "").upper() != "CSV":
                continue
            url = res["url"]
            name = "contratar_" + url.rsplit("/", 1)[-1]
            try:
                fetch(url, RAW / name)
            except Exception as e:
                failures.append((name, str(e)))
                print(f"FAIL: {name}: {e}", file=sys.stderr)
    except Exception as e:
        failures.append((ds, str(e)))
        print(f"FAIL (CKAN): {ds}: {e}", file=sys.stderr)

print(f"\ndone. {len(failures)} failure(s).")
for name, err in failures:
    print(f"  {name}: {err}")
