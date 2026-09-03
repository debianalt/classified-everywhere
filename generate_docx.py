"""
generate_docx.py — build the submission files for The British Journal of
Sociology from the markdown sections.

Copy-adapted from articles_2_/2026_3_TS_sent/r2_reenvio_2026-09_envio, with the
house guards this tree requires and the journal's own rules:

  * double-anonymous review, so author details live only on the title page
    (guidelines p. 3);
  * "tables and figures should appear on separate sheets with self-explanatory
    titles", and "the position in the text of each table and figure should be
    clearly indicated" (guideline 15) — the generator lifts each table and each
    figure-with-caption out of the running text onto its own sheet and leaves a
    position marker behind;
  * tables carry "the minimal number of lines with no boxes" (guideline 15), so
    the shaded header and the inner rules of the source template are stripped;
  * double-line spacing (guideline 4);
  * word length on the title page counts abstract, text, notes, bibliography
    and appendices (guideline 6) — the figure and table apparatus does not, and
    99_wordcount.py is the counter.

Two guards from the tree's CLAUDE.md, both mandatory:

  * pandoc's markdown reader replaces the space after "et al.", "No." and a
    handful of other abbreviations with U+00A0, which Word draws as a raised
    circle. It is stripped here, but ONLY on runs that contain it: assigning
    Run.text replaces the run's children wholesale, so an unconditional
    assignment deletes the <w:drawing> of every image run and silently strips
    the figures.
  * a "---" line becomes a horizontal bar in the DOCX, so any block that is
    only dashes is dropped.

Run: python generate_docx.py
"""
import re
import subprocess
import tempfile
from datetime import date
from pathlib import Path

import pypandoc
from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH, WD_BREAK
from docx.oxml import parse_xml
from docx.oxml.ns import nsdecls, qn
from docx.shared import Cm, Pt, RGBColor

BASE = Path(__file__).resolve().parent
SECTIONS = BASE / "sections"
FIGURES = BASE / "figures"
OUT = BASE / "submission"

BODY_ORDER = [
    "00_abstract.md", "01_introduction.md", "02_framework.md",
    "04_data_methods.md", "05_results.md", "06_discussion.md",
    "07_conclusions.md", "08_references.md",
]

AUTHOR = "Raimundo Elías Gómez"
ORCID = "0000-0002-4468-9618"
EMAIL = "elias.gomez@conicet.gov.ar"
AFFILIATION = ("Consejo Nacional de Investigaciones Científicas y Técnicas "
               "(CONICET) / Facultad de Humanidades y Ciencias Sociales, "
               "Universidad Nacional de Misiones (UNaM), Argentina")
ADDRESS = "Tucumán 1605, 1° Piso, CP 3300, Posadas, Misiones, Argentina"

FIG_WIDTH_IN = 6.2   # A4 with 2.54 cm margins leaves 6.26 in of text width

# pandoc's docx writer silently drops a bare LaTeX page-break command, so the
# break travels as a marker paragraph and becomes a real page break in
# post-processing.
PAGEBREAK = "[[PAGEBREAK]]"


# ---------------------------------------------------------------------------
# 1. Reading and splitting
# ---------------------------------------------------------------------------
def read_section(name):
    return (SECTIONS / name).read_text(encoding="utf-8").strip()


def blocks_of(text):
    return [b for b in text.split("\n\n") if b.strip()]


# Point 15 of the guidelines asks for Roman numerals, but the journal as
# typeset numbers tables in Arabic — verified on Branson et al. (2024) and
# Rossier et al. (2022), both published versions — and the same point says
# to refer to editions of the Journal for sample layouts.
ROMAN = re.compile(r"^\*\*Table (\d+)\.\*\*")
FIG_EMBED = re.compile(r"^!\[\[(Fig[^\].]+)\.png\]\]$")
FIG_CAP = re.compile(r"^\*\*Figure (\w+)\.\*\*")


def split_apparatus(body_blocks):
    """Lift tables and figures out of the running text.

    Returns the body with position markers, and the two lists of sheets. A
    figure is an embed followed by its caption; a table is a bold title,
    the pipe table, and an optional note. Everything else passes through.
    """
    out, figures, tables = [], [], []
    i = 0
    while i < len(body_blocks):
        b = body_blocks[i].strip()

        if re.fullmatch(r"-{3,}", b):        # never a horizontal bar
            i += 1
            continue

        m = FIG_EMBED.match(b)
        if m:
            tag = m.group(1)
            caption = ""
            if i + 1 < len(body_blocks) and FIG_CAP.match(body_blocks[i + 1].strip()):
                caption = body_blocks[i + 1].strip()
                i += 1
            number = FIG_CAP.match(caption).group(1) if caption else tag
            figures.append((tag, caption))
            out.append(f"[Figure {number} about here]")
            i += 1
            continue

        m = ROMAN.match(b)
        if m:
            number = m.group(1)
            parts = [b]
            j = i + 1
            while j < len(body_blocks):
                nxt = body_blocks[j].strip()
                if nxt.startswith("|") or nxt.startswith("*Note*"):
                    parts.append(nxt)
                    j += 1
                else:
                    break
            tables.append((number, "\n\n".join(parts)))
            out.append(f"[Table {number} about here]")
            i = j
            continue

        out.append(b)
        i += 1
    return out, figures, tables


# ---------------------------------------------------------------------------
# 2. Assembly
# ---------------------------------------------------------------------------
def assemble_manuscript():
    body = []
    for name in BODY_ORDER:
        body += blocks_of(read_section(name))
    text_blocks, figures, tables = split_apparatus(body)
    # Figures are numbered in order of appearance, so first mention and sheet
    # order agree; the sort keeps them agreeing if a figure is ever added.
    figures.sort(key=lambda f: int(re.sub(r"\D", "", f[0]) or 0))

    parts = ["\n\n".join(text_blocks)]

    if tables:
        parts.append(PAGEBREAK)
        for k, (number, md) in enumerate(tables):
            if k:
                parts.append(PAGEBREAK)
            parts.append(md)

    for tag, caption in figures:
        parts.append(PAGEBREAK)
        path = (FIGURES / f"{tag}.png").resolve()
        if not path.exists():
            raise SystemExit(f"missing figure: {path}")
        parts.append(f"![]({str(path).replace(chr(92), '/')})"
                     f"{{width={FIG_WIDTH_IN}in}}")
        if caption:
            parts.append(caption)

    return "\n\n".join(parts), len(figures), len(tables)


def wordcount():
    """The journal counts abstract, text, notes, bibliography and appendices;
    99_wordcount.py is the canonical counter and already excludes the figure
    and table apparatus."""
    r = subprocess.run(["python", str(BASE / "99_wordcount.py")],
                       capture_output=True, text=True, cwd=str(BASE))
    m = re.search(r"TOTAL\s+([\d,]+)", r.stdout)
    return f"{int(m.group(1).replace(',', '')):,}" if m else "—"


def assemble_title_page():
    title = read_section("00_abstract.md").split("\n")[0].lstrip("# ").strip()
    today = date.today().strftime("%d %B %Y")
    L = [
        f"# {title}",
        "",
        f"**{AUTHOR}**",
        "",
        f"ORCID: {ORCID}",
        "",
        AFFILIATION,
        "",
        "## Corresponding author",
        "",
        f"{AUTHOR}, {EMAIL}",
        "",
        ADDRESS,
        "",
        "## Word length",
        "",
        f"{wordcount()} words, counting the abstract, the text, the notes, "
        "the bibliography and the appendices. Tables and figures are on "
        "separate sheets and are not counted. Supporting Information is "
        "submitted as a separate file.",
        "",
        "## Date of submission",
        "",
        today,
        "",
        "## Data availability statement",
        "",
        "The analysis uses public administrative records. The award and call "
        "records of COMPR.AR and the supplier register (SIPRO) are published "
        "as open data by the Oficina Nacional de Contrataciones; the taxpayer "
        "register by the Agencia de Recaudación y Control Aduanero; the "
        "consumer price index and the exchange rate series by the national "
        "statistics and series APIs; and the provincial geometry by "
        "infra.datos.gob.ar. The archived snapshots, the hand-coded buyer "
        "typology, the gazetteer of purchasing-unit seats and the analysis "
        "code are available from the author on request and will be deposited "
        "in a public repository on acceptance. Section S0 of the Supporting "
        "Information lists every source with the date of its snapshot.",
        "",
        "## Funding statement",
        "",
        "This research received no specific grant from any funding agency in "
        "the public, commercial or not-for-profit sectors.",
        "",
        "## Conflict of interest disclosure",
        "",
        "The author declares no conflict of interest.",
        "",
        "## Ethics approval statement",
        "",
        "The study analyses public administrative records of transactions "
        "between organisations and the national state. It involves no human "
        "participants and no personal data beyond what the state publishes as "
        "open data, so ethics approval was not required. Suppliers who are "
        "natural persons are never identified: no taxpayer identifier and no "
        "name appears in the manuscript, the Supporting Information, the "
        "tables, the figures or the code, and the flow map is drawn above an "
        "anonymity floor.",
        "",
        "## Patient consent statement",
        "",
        "Not applicable; the study involves no patients.",
        "",
        "## Permission to reproduce material from other sources",
        "",
        "Not applicable; all figures and tables are the author's own, built "
        "from public data.",
    ]
    return "\n".join(L)


def assemble_supplementary():
    text = read_section("S_supplementary.md")
    blocks = []
    for b in blocks_of(text):
        s = b.strip()
        if re.fullmatch(r"-{3,}", s):
            continue
        m = FIG_EMBED.match(s)
        if m:
            path = (FIGURES / f"{m.group(1)}.png").resolve()
            if not path.exists():
                raise SystemExit(f"missing figure: {path}")
            blocks.append(f"![]({str(path).replace(chr(92), '/')})"
                          f"{{width={FIG_WIDTH_IN}in}}")
            continue
        blocks.append(s)
    return "\n\n".join(blocks)


def assemble_cover_letter():
    path = BASE / "submission" / "cover_letter.md"
    if not path.exists():
        return None
    return path.read_text(encoding="utf-8").strip()


# ---------------------------------------------------------------------------
# 3. Conversion
# ---------------------------------------------------------------------------
COMMENT = re.compile(r"<!--.*?-->", re.S)


def preprocess(md):
    """Strip the table markers of build_si_tables.py, which are for Obsidian
    and the builder, not for the journal."""
    md = COMMENT.sub("", md)
    return re.sub(r"\n{3,}", "\n\n", md).strip()


def convert(md, out_path):
    tmp = Path(tempfile.mktemp(suffix=".md"))
    tmp.write_text(preprocess(md), encoding="utf-8")
    try:
        pypandoc.convert_file(str(tmp), "docx", format="markdown",
                              outputfile=str(out_path))
    finally:
        tmp.unlink(missing_ok=True)


# ---------------------------------------------------------------------------
# 4. Post-processing
# ---------------------------------------------------------------------------
FONT = "Times New Roman"


def _strip_nbsp(doc):
    """Pandoc leaves U+00A0 after 'et al.' and friends; Word draws it as a
    raised circle. The guard is not optional: assigning Run.text replaces the
    run's children, which deletes the <w:drawing> of every image run."""
    n = 0
    for para in doc.paragraphs:
        for run in para.runs:
            if "\xa0" in run.text:
                run.text = run.text.replace("\xa0", " ")
                n += 1
    for table in doc.tables:
        for row in table.rows:
            for cell in row.cells:
                for para in cell.paragraphs:
                    for run in para.runs:
                        if "\xa0" in run.text:
                            run.text = run.text.replace("\xa0", " ")
                            n += 1
    return n


def _plain_table(table):
    """Guideline 15: the minimal number of lines, no boxes. A rule under the
    header and nothing else."""
    tbl = table._tbl
    tblPr = tbl.tblPr
    for old in tblPr.findall(qn("w:tblBorders")):
        tblPr.remove(old)
    tblPr.append(parse_xml(
        f'<w:tblBorders {nsdecls("w")}>'
        '<w:top w:val="single" w:sz="6" w:color="000000"/>'
        '<w:bottom w:val="single" w:sz="6" w:color="000000"/>'
        '<w:left w:val="none" w:sz="0" w:color="auto"/>'
        '<w:right w:val="none" w:sz="0" w:color="auto"/>'
        '<w:insideH w:val="none" w:sz="0" w:color="auto"/>'
        '<w:insideV w:val="none" w:sz="0" w:color="auto"/>'
        "</w:tblBorders>"))
    for old in tblPr.findall(qn("w:tblW")):
        tblPr.remove(old)
    tblPr.append(parse_xml(f'<w:tblW {nsdecls("w")} w:w="5000" w:type="pct"/>'))

    size = Pt(8) if len(table.columns) > 7 else Pt(9)
    for r, row in enumerate(table.rows):
        for c, cell in enumerate(row.cells):
            # no shading anywhere: the journal asks for no boxes
            tcPr = cell._tc.get_or_add_tcPr()
            for shd in tcPr.findall(qn("w:shd")):
                tcPr.remove(shd)
            for para in cell.paragraphs:
                para.paragraph_format.line_spacing = 1.0
                para.paragraph_format.space_before = Pt(1)
                para.paragraph_format.space_after = Pt(1)
                para.paragraph_format.alignment = (
                    WD_ALIGN_PARAGRAPH.LEFT if c == 0
                    else WD_ALIGN_PARAGRAPH.RIGHT)
                for run in para.runs:
                    run.font.size = size
                    run.font.name = FONT
                    run.bold = (r == 0)
    if table.rows:                       # rule under the header row
        trPr = table.rows[0]._tr.get_or_add_trPr()
        if trPr.find(qn("w:tblHeader")) is None:
            trPr.append(parse_xml(f'<w:tblHeader {nsdecls("w")} w:val="true"/>'))
        for cell in table.rows[0].cells:
            tcPr = cell._tc.get_or_add_tcPr()
            for old in tcPr.findall(qn("w:tcBorders")):
                tcPr.remove(old)
            tcPr.append(parse_xml(
                f'<w:tcBorders {nsdecls("w")}>'
                '<w:bottom w:val="single" w:sz="6" w:color="000000"/>'
                "</w:tcBorders>"))


def postprocess(path, double_spaced=True, hanging_bibliography=False,
                justified=False):
    doc = Document(str(path))

    for section in doc.sections:
        section.top_margin = Cm(2.54)
        section.bottom_margin = Cm(2.54)
        section.left_margin = Cm(2.54)
        section.right_margin = Cm(2.54)

    style = doc.styles["Normal"]
    style.font.name = FONT
    style.font.size = Pt(12)
    # Justified running prose (author's instruction, 31 Aug). The
    # guidelines are silent on alignment; they ask only for double spacing.
    # Everything that is not running prose stays ragged right below:
    # headings, captions, table notes, the position markers and the
    # bibliography, where a hanging indent plus justification opens gaps
    # around long titles and DOIs.
    style.paragraph_format.alignment = (WD_ALIGN_PARAGRAPH.JUSTIFY
                                        if justified
                                        else WD_ALIGN_PARAGRAPH.LEFT)
    style.paragraph_format.space_after = Pt(6)
    style.paragraph_format.line_spacing = 2.0 if double_spaced else 1.15
    rpr = style.element.get_or_add_rPr()
    rfonts = rpr.find(qn("w:rFonts"))
    if rfonts is None:
        rfonts = parse_xml(f"<w:rFonts {nsdecls('w')}/>")
        rpr.append(rfonts)
    rfonts.set(qn("w:eastAsia"), FONT)
    rfonts.set(qn("w:cs"), FONT)

    for level, size in {1: 14, 2: 12, 3: 12}.items():
        try:
            hs = doc.styles[f"Heading {level}"]
            hs.font.name = FONT
            hs.font.size = Pt(size)
            hs.font.bold = True
            hs.font.italic = (level >= 2)
            hs.font.color.rgb = RGBColor(0, 0, 0)
            hs.paragraph_format.space_before = Pt(12)
            hs.paragraph_format.space_after = Pt(6)
            hs.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
        except KeyError:
            pass

    in_bibliography = False
    for para in doc.paragraphs:
        txt = para.text.strip()
        if para.style.name.startswith("Heading"):
            in_bibliography = txt.lower().startswith("bibliography")
            continue
        if hanging_bibliography and in_bibliography and txt:
            para.paragraph_format.left_indent = Cm(1.27)
            para.paragraph_format.first_line_indent = Cm(-1.27)
            para.paragraph_format.line_spacing = 2.0
            para.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
        # a figure or a caption sits tight to its image, never double spaced
        if txt.startswith(("Figure ", "Table ")) or not txt:
            para.paragraph_format.line_spacing = 1.15
            para.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
        if txt.startswith(("Note:", "Source:")) or (
                txt.startswith("[") and txt.endswith("about here]")):
            para.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
        for run in para.runs:
            if run.font.name != FONT:
                run.font.name = FONT

    for table in doc.tables:
        _plain_table(table)

    swapped = _strip_nbsp(doc)
    doc.save(str(path))
    return swapped


def page_break_before(path, markers):
    """Turn the marker paragraphs into real page breaks, so every table and
    every figure starts a sheet of its own."""
    doc = Document(str(path))
    breaks = 0
    for para in doc.paragraphs:
        t = para.text.strip()
        if t == PAGEBREAK:
            for run in para.runs:
                run.text = ""
            para.add_run().add_break(WD_BREAK.PAGE)
            para.paragraph_format.space_after = Pt(0)
            para.paragraph_format.line_spacing = 1.0
            breaks += 1
        elif markers and any(t.startswith(m) for m in markers):
            para.paragraph_format.page_break_before = True
    doc.save(str(path))
    return breaks


# ---------------------------------------------------------------------------
# 5. Verification
# ---------------------------------------------------------------------------
def verify(path):
    import zipfile
    with zipfile.ZipFile(path) as z:
        xml = z.read("word/document.xml").decode("utf-8")
    return {
        "nbsp": xml.count(" "),
        "rules": xml.count("v:rect"),
        "images": xml.count("<a:blip"),
    }


ANON = ["Gómez", "Gomez", "CONICET", "UNaM", "elias.gomez", ORCID,
        "Universidad Nacional de Misiones"]


def anonymity(path):
    """Where an identifying token appears matters. A third-person
    self-citation in the bibliography is ordinary practice under double-
    anonymous review; the same token in the body is a leak."""
    doc = Document(str(path))
    body, biblio, in_biblio = set(), set(), False
    for para in doc.paragraphs:
        t = para.text
        if para.style.name.startswith("Heading"):
            in_biblio = t.strip().lower().startswith("bibliography")
            continue
        for w in ANON:
            if w in t:
                (biblio if in_biblio else body).add(w)
    return sorted(body), sorted(biblio)


if __name__ == "__main__":
    OUT.mkdir(exist_ok=True)

    ms_md, n_fig, n_tab = assemble_manuscript()
    jobs = [
        ("manuscript_anonymous.docx", ms_md,
         dict(hanging_bibliography=True, justified=True),
         ["[Table ", "[Figure "]),
        # the title page is a list of fields, not running prose
        ("title_page.docx", assemble_title_page(), dict(double_spaced=False),
         None),
        ("supporting_information.docx", assemble_supplementary(),
         dict(double_spaced=False, justified=True), None),
    ]
    cl = assemble_cover_letter()
    if cl:
        jobs.append(("cover_letter.docx", cl,
                     dict(double_spaced=False, justified=True), None))

    for name, md, kw, markers in jobs:
        path = OUT / name
        convert(md, path)
        swapped = postprocess(path, **kw)
        breaks = page_break_before(path, None)
        v = verify(path)
        print(f"{name}: nbsp={v['nbsp']} rules={v['rules']} "
              f"images={v['images']} saltos={breaks} "
              f"(nbsp corregidos: {swapped})")
        if name == "manuscript_anonymous.docx":
            body, biblio = anonymity(path)
            if body:
                print("   ANONIMATO ROTO en el cuerpo: " + ", ".join(body))
            else:
                print("   anonimato: cuerpo limpio")
            if biblio:
                print("   en la bibliografía (autocita en tercera persona, "
                      "decisión editorial): " + ", ".join(biblio))

    print(f"\nmanuscrito: {n_tab} tablas y {n_fig} figuras en hojas aparte, "
          f"con marcadores de posición en el texto.")
