#################################################
########### Extract Clave Labels ################
#################################################

### REPLICATION FILE: extract_clave_labels.py
### PYTHON VERSION:   3.10+
### AUTHORS:          Matías Carrasco, Victor Ortega Le Hénanff
### DATE:             2026-05-04

### Parses data/source/docs/enigh-variable-documentation.txt to extract the
### gasto clave catalog (codes like 011131 = "Tortillas de maíz") used by
### the gastoshogar / gastospersona tables, and writes them to
### data/clean/enigh/clave_gasto_labels.csv with columns:
###   clave (str)
###   label (str)
###
### The CSV is consumed by enigh-hhlevel-exp-month.do via merge.

import re
import csv
from pathlib import Path

DOCS_PATH = Path(__file__).resolve().parent.parent.parent \
    / "data" / "source" / "docs" / "enigh-variable-documentation.txt"
OUT_PATH = Path(__file__).resolve().parent.parent.parent \
    / "data" / "clean" / "enigh" / "clave_gasto_labels.csv"

OUT_PATH.parent.mkdir(parents=True, exist_ok=True)


def extract_clave_labels(text: str) -> dict:
    ### Find blocks where the variable is `clave` inside the GASTOSHOGAR
    ### or GASTOSPERSONA tables, then read the value-label section that
    ### follows. Codes look like:  "      011131 = Tortillas de maíz".
    labels = {}
    in_target_table = False
    in_clave_block = False
    in_value_labels = False
    for raw in text.splitlines():
        line = raw.rstrip("\n")
        ### Table-header detection.
        m_tbl = re.match(r"\s*Table:\s*([A-Z_]+)", line)
        if m_tbl:
            tname = m_tbl.group(1).upper()
            in_target_table = tname in {"GASTOSHOGAR", "GASTOSPERSONA"}
            in_clave_block = False
            in_value_labels = False
            continue
        if not in_target_table:
            continue
        ### Variable-name detection.
        m_var = re.match(r"\s*Variable\s*#?\d*:\s*(\S+)", line)
        if m_var:
            in_clave_block = m_var.group(1).strip() == "clave"
            in_value_labels = False
            continue
        if not in_clave_block:
            continue
        ### Look for the "Value Labels:" section header.
        if "Value Labels" in line:
            in_value_labels = True
            continue
        if not in_value_labels:
            continue
        ### A blank line ends the value-label section.
        if line.strip() == "":
            in_value_labels = False
            in_clave_block = False
            continue
        ### Match "  011131 = Tortillas de maíz" or "  T901 = Alimentos ...".
        m = re.match(r"\s*([A-Z]?\d{3,6})\s*=\s*(.+)$", line)
        if m:
            code = m.group(1).strip()
            lab = m.group(2).strip()
            ### Drop trailing parenthetical notes if any.
            if code not in labels:
                labels[code] = lab
    return labels


def main() -> None:
    print(f"Reading: {DOCS_PATH}")
    text = DOCS_PATH.read_text(encoding="utf-8", errors="replace")
    labels = extract_clave_labels(text)
    print(f"Extracted {len(labels)} unique clave labels.")
    with OUT_PATH.open("w", encoding="utf-8", newline="") as f:
        w = csv.writer(f)
        w.writerow(["clave", "label"])
        for code in sorted(labels):
            w.writerow([code, labels[code]])
    print(f"Wrote: {OUT_PATH}")


if __name__ == "__main__":
    main()
