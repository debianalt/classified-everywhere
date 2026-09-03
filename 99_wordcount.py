import re
from pathlib import Path

SEC = Path(r"C:\Users\ant\OneDrive\articles_3\2026_2\sections")
FILES = ["00_abstract.md", "01_introduction.md", "02_framework.md",
         "04_data_methods.md", "05_results.md", "06_discussion.md",
         "07_conclusions.md", "08_references.md"]

# The BJS limit counts abstract + text + notes + bibliography; tables and
# figures sit on separate sheets and do NOT count. Excluded here: embeds,
# table rows, and the figure/table apparatus (captions **Figure ...,
# titles **Table ..., *Note* and *Source* paragraphs), which lives in the
# section files so it renders under each figure and table in Obsidian.
APPARATUS = ("**Figure ", "**Table ", "*Note*", "*Source*")

total = 0
for f in FILES:
    t = SEC.joinpath(f).read_text(encoding="utf-8")
    t = re.sub(r"<!--.*?-->", " ", t, flags=re.S)
    kept = []
    for para in re.split(r"\n\s*\n", t):
        if para.strip().startswith(APPARATUS):
            continue
        kept.append(para)
    lines = []
    for ln in "\n".join(kept).splitlines():
        s = ln.strip()
        if re.fullmatch(r"!\[\[.*\]\]", s):
            continue
        if s.startswith("|"):           # table rows and header separators
            continue
        if re.fullmatch(r"-{3,}", s):
            continue
        lines.append(ln)
    n = len(re.findall(r"\S+", " ".join(lines)))
    total += n
    print(f"{f:22s} {n:6d}")
print(f"{'TOTAL':22s} {total:6d}  (limit 8,000)")
