# Build the replication folder for GitHub. Mirrors the project layout, because
# every script resolves its paths relative to the project root; moving them into
# subfolders would break the chain.
import shutil, pathlib, re, sys

SRC = pathlib.Path(__file__).resolve().parent
DST = SRC.parent / "classified"

def _wipe(root):
    # OneDrive holds a handle on a folder it has just synced, so rmtree can
    # fail on the directory even when every file inside it is gone. Delete the
    # files, then remove what directories will go, and carry on.
    for f in sorted(root.rglob("*"), key=lambda q: -len(q.parts)):
        # never touch the git metadata: an earlier version of this wipe
        # destroyed the local repository on the first rebuild after init
        if ".git" in f.parts:
            continue
        try:
            f.unlink() if f.is_file() else f.rmdir()
        except OSError:
            pass


KEEP = ("README.md", "LICENSE", "LICENSE-DATA.md", ".gitignore",
        "requirements.txt", "R-packages.txt", "data/README.md")
if DST.exists():
    saved = {k: (DST / k).read_bytes() for k in KEEP if (DST / k).exists()}
    _wipe(DST)
else:
    saved = {}
for d in ("data/raw/covariates", "data/processed", "tables", "figures",
          "validation"):
    (DST / d).mkdir(parents=True, exist_ok=True)

# ── code: every script, flat, as in the project ──────────────────────────────
n_code = 0
# generate_docx.py stays out: it holds the author's name, ORCID, email and
# postal address in constants and cannot run without the manuscript sources
SKIP_CODE = {"generate_docx.py"}
for p in sorted(SRC.glob("*.R")) + sorted(SRC.glob("*.py")):
    if p.name in SKIP_CODE:
        continue
    shutil.copy2(p, DST / p.name)
    n_code += 1

# ── raw inputs that are public, small and not personal ───────────────────────
RAW_OK = ["georef_localidades.json", "georef_provincias.geojson",
          "uoc_gazetteer.csv", "uoc_sedes_deepseek.json",
          "uoc_sedes_nominatim.json", "tipo_cambio_bna_anual.csv",
          "covid_emergencia_cgonc9y10.csv",
          "dnap_ocupacion_salarios_1987_2024.xlsx"]
# The two CONTRAT.AR extracts that script 07 reads are NOT distributed: they
# carry contratista_cuit and contratista_razon_social. The analysis reported in
# the paper does not use them (Section S0 lists CONTRAT.AR among the sources
# consulted and not used); data/README.md says where to download them.
n_raw = 0
for name in RAW_OK:
    p = SRC / "data" / "raw" / name
    if p.exists():
        shutil.copy2(p, DST / "data" / "raw" / name)
        n_raw += 1
    else:
        print("MISSING raw:", name)
for p in sorted((SRC / "data" / "raw" / "covariates").glob("*")):
    shutil.copy2(p, DST / "data" / "raw" / "covariates" / p.name)
    n_raw += 1

# ── canonical outputs ────────────────────────────────────────────────────────
n_tab = 0
for p in sorted((SRC / "tables").glob("*.csv")):
    shutil.copy2(p, DST / "tables" / p.name)
    n_tab += 1
n_fig = 0
for p in sorted((SRC / "figures").glob("*.png")):
    shutil.copy2(p, DST / "figures" / p.name)
    n_fig += 1

# ── the frozen Python pass and its cross-validation report ───────────────────
n_val = 0
for p in sorted((SRC / "validation").rglob("*")):
    # the frozen Python passes and the report only: the adversarial-review runs
    # in this folder are working notes on a manuscript under review
    if p.is_file() and (p.parent.name.startswith("python_pass")
                        or p.name == "crossval_report.csv"):
        rel = p.relative_to(SRC / "validation")
        out = DST / "validation" / rel
        out.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(p, out)
        n_val += 1

n_anon = 0
for p in sorted((SRC / "data" / "anonymised").glob("*")):
    out = DST / "data" / "anonymised"
    out.mkdir(parents=True, exist_ok=True)
    shutil.copy2(p, out / p.name)
    n_anon += 1
print("de-identified release files:", n_anon)

(DST / "data" / "processed" / ".gitkeep").write_text(
    "# The parquets of the data layer are rebuilt here by scripts 00-02, 09,\n"
    "# 33, 34 and 43. They are not distributed: they carry supplier names and\n"
    "# taxpayer identifiers. See data/README.md.\n", encoding="utf-8")

for k, v in saved.items():
    p = DST / k
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_bytes(v)
print("hand-written docs restored:", len(saved))

print(f"code {n_code} | raw {n_raw} | tables {n_tab} | figures {n_fig} | "
      f"validation {n_val}")

# ── safety net: nothing personal may reach the repo ──────────────────────────
# An identifier is eleven digits standing alone: not touching another digit and
# not touching a decimal point, or the decimal expansion of a latitude or of an
# employment rate reads as one.
# Only quoted cells: R and pandas write a character column quoted, so an
# identifier that mattered would be quoted, whereas an award amount of eleven
# digits is an unquoted number. Without the quotes this fired on a 30.8-billion
# peso award and on the decimal expansion of a latitude.
cuit = re.compile(r"\"(?:20|23|24|27|30|33|34)\d{9}\"")
bad = []
for p in DST.rglob("*"):
    if not p.is_file() or p.suffix.lower() in (".png", ".xlsx", ".zip", ".gz"):
        continue
    try:
        t = p.read_text(encoding="utf-8", errors="replace")
    except Exception:
        continue
    if cuit.search(t):
        bad.append(str(p.relative_to(DST)))
print("files carrying a taxpayer identifier:", bad if bad else "none")
if bad:
    sys.exit("refusing to leave personal identifiers in the replication folder")
size = sum(f.stat().st_size for f in DST.rglob("*") if f.is_file())
print(f"total {size/1048576:.1f} MB, {sum(1 for f in DST.rglob('*') if f.is_file())} files")
