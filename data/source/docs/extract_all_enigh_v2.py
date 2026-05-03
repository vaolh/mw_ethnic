#!/usr/bin/env python3
"""
Extract structured variable documentation from ENIGH 2016–2024 PDFs.

Strategy:
  1. Extract every page of every PDF using position-aware parsing.
  2. For variable description pages: parse the two-column form layout
     using x-coordinates to associate fields correctly.
  3. For variable list pages: parse the two-column table layout.
  4. For catalogue pages: extract code-description pairs.
  5. Produce a single clean, structured txt file.

The 2016 PDF has broken font encoding → use OCR.
2018–2024 use direct positioned text extraction.
"""

import fitz
import os
import re
import sys
from collections import OrderedDict

DOCS_DIR = os.path.dirname(os.path.abspath(__file__))
OUTPUT_FILE = os.path.join(DOCS_DIR, "enigh-variable-documentation.txt")
YEARS = [2016, 2018, 2020, 2022, 2024]

# ---------------------------------------------------------------------------
# Utility: extract positioned lines from a page (2018-2024)
# ---------------------------------------------------------------------------
def get_positioned_lines(page):
    """Return list of (x, y, text) sorted by y then x."""
    lines = []
    blocks = page.get_text("dict")["blocks"]
    for b in blocks:
        if "lines" not in b:
            continue
        for line in b["lines"]:
            txt = "".join(s["text"] for s in line["spans"]).strip()
            if not txt:
                continue
            x0 = line["bbox"][0]
            y0 = line["bbox"][1]
            lines.append((x0, y0, txt))
    lines.sort(key=lambda t: (t[1], t[0]))
    return lines


# ---------------------------------------------------------------------------
# Parse a VARIABLE DESCRIPTION page into structured variable records
# ---------------------------------------------------------------------------
def parse_var_description_page(lines):
    """
    Parse positioned lines from a variable-description page.
    Returns list of variable dicts.
    """
    # Filter out INEGI footer and page numbers
    lines = [l for l in lines if not l[2].startswith("INEGI. Encuesta Nacional")]
    # Filter standalone page numbers
    lines = [l for l in lines if not re.match(r'^\d{1,3}$', l[2])]

    variables = []
    current_var = None

    # Group lines by Y coordinate (within tolerance)
    def y_group(lines, tol=3):
        """Group lines that share approximately the same y coordinate."""
        if not lines:
            return []
        groups = []
        current_group = [lines[0]]
        for l in lines[1:]:
            if abs(l[1] - current_group[-1][1]) <= tol:
                current_group.append(l)
            else:
                groups.append(current_group)
                current_group = [l]
        groups.append(current_group)
        return groups

    grouped = y_group(lines)

    # Collect deferred text (definition text, question text that appears below)
    i = 0
    while i < len(grouped):
        row = grouped[i]
        row_texts = [(l[0], l[2]) for l in row]

        # Check if this row starts a new variable entry: "#N  varname:"
        first_text = row[0][2] if row else ""
        var_match = re.match(r'^#(\d+)\s+(\w+)\s*:\s*(.*)', first_text)
        if var_match:
            if current_var:
                variables.append(current_var)
            current_var = OrderedDict()
            current_var['number'] = var_match.group(1)
            current_var['name'] = var_match.group(2)
            current_var['label'] = var_match.group(3).strip()
            current_var['type'] = ''
            current_var['range'] = ''
            current_var['cuestionario'] = ''
            current_var['seccion'] = ''
            current_var['definicion'] = ''
            current_var['num_pregunta'] = ''
            current_var['pregunta'] = ''
            current_var['values'] = []
            current_var['notas'] = []
            i += 1
            continue

        if current_var is None:
            i += 1
            continue

        # Parse field rows (left side x < 260, right side x >= 260)
        left_items = [(x, t) for x, t in row_texts if x < 260]
        right_items = [(x, t) for x, t in row_texts if x >= 260]

        # Process left side
        for x, t in left_items:
            if t.startswith('Tipo') and x < 60:
                # Next item on same row should be the type value
                type_vals = [tt for xx, tt in left_items if xx > 60 and xx < 200]
                if type_vals:
                    current_var['type'] = type_vals[0]
            elif t.startswith('Rango') and x < 60:
                range_vals = [tt for xx, tt in left_items if xx > 60 and xx < 260]
                if range_vals:
                    current_var['range'] = range_vals[0]
            elif t.startswith('Definici') and x < 60:
                def_vals = [tt for xx, tt in left_items if xx > 60]
                if def_vals:
                    current_var['definicion'] = ' '.join(def_vals)
            elif t == 'Valor':
                pass  # header row for value labels
            elif t == '(Continúa)' or t == '(Continuación)':
                pass
            elif re.match(r'^Nota:', t):
                current_var['notas'].append(t)
            elif x > 50 and x < 80 and re.match(r'^[\d&]+$', t):
                # This is a value code
                label_parts = [tt for xx, tt in left_items if xx > 80]
                label = ' '.join(label_parts)
                current_var['values'].append((t, label))

        # Process right side
        for x, t in right_items:
            if t.startswith('Cuestionario'):
                quest_vals = [tt for xx, tt in right_items if xx > x + 50]
                if quest_vals:
                    current_var['cuestionario'] = quest_vals[0]
            elif 'apartado' in t.lower():
                sec_vals = [tt for xx, tt in right_items if xx > x + 50]
                if sec_vals:
                    current_var['seccion'] = sec_vals[0]
            elif t.startswith('Número de pregunta') or t.startswith('Numero de pregunta'):
                num_vals = [tt for xx, tt in right_items if xx > x + 50]
                if num_vals:
                    current_var['num_pregunta'] = num_vals[0]
            elif t.startswith('Pregunta textual'):
                pass  # the actual text usually appears on a displaced line below

        # Check for definition/question continuation text
        # These are lines that aren't field headers
        if not var_match:
            for x, t in left_items:
                if (x > 80 and not t.startswith('Tipo') and not t.startswith('Rango')
                    and not t.startswith('Definici') and not t == 'Valor'
                    and not t == 'Etiqueta' and not t.startswith('Nota:')
                    and not re.match(r'^[\d&]+$', t)
                    and not t.startswith('(Contin')):
                    # Could be continuation of definition
                    if current_var['definicion'] and not any(
                        t == v[1] for v in current_var['values']
                    ):
                        current_var['definicion'] += ' ' + t
            for x, t in right_items:
                if (x > 350 and not t.startswith('Cuestionario')
                    and 'apartado' not in t.lower()
                    and not t.startswith('Número de preg')
                    and not t.startswith('Numero de preg')
                    and not t.startswith('Pregunta text')
                    and not t.startswith('INEGI')
                    and not re.match(r'^Secci[oó]n', t)
                    and not re.match(r'^Hogares', t)):
                    if current_var['pregunta']:
                        current_var['pregunta'] += ' ' + t
                    else:
                        current_var['pregunta'] = t

        i += 1

    if current_var:
        variables.append(current_var)
    return variables


# ---------------------------------------------------------------------------
# Parse a VARIABLE LIST page (two-column table)
# ---------------------------------------------------------------------------
def parse_var_list_page(lines):
    """Parse variable list table. Returns list of (num, name, label, type)."""
    lines = [l for l in lines if not l[2].startswith("INEGI. Encuesta Nacional")]
    lines = [l for l in lines if not re.match(r'^\d{1,3}$', l[2])]

    entries = []
    current_table = None

    # Detect table headers
    for x, y, t in lines:
        m = re.match(r'^Tabla\s+(\w+)', t)
        if m:
            current_table = m.group(1)

    # Group by Y
    from itertools import groupby
    lines_sorted = sorted(lines, key=lambda l: l[1])
    rows = []
    current_row = []
    last_y = None
    for x, y, t in lines_sorted:
        if last_y is not None and abs(y - last_y) > 3:
            if current_row:
                rows.append(current_row)
            current_row = []
        current_row.append((x, t))
        last_y = y
    if current_row:
        rows.append(current_row)

    for row in rows:
        # Left column: x < 300; Right column: x >= 300
        for side_items in [
            [(x, t) for x, t in row if x < 300],
            [(x, t) for x, t in row if x >= 300]
        ]:
            if not side_items:
                continue
            side_items.sort(key=lambda it: it[0])
            # Pattern: num, varname, label, type
            num_items = [t for x, t in side_items if re.match(r'^\d+$', t) and x < side_items[0][0] + 30]
            if num_items:
                # Collect remaining
                rest = [t for x, t in side_items if not re.match(r'^\d+$', t) or x > side_items[0][0] + 30]
                # First non-number is likely the variable name
                if rest:
                    varname = rest[0]
                    # Type is usually last and matches C(n) or N(n) pattern
                    type_val = ''
                    label_parts = []
                    for r in rest[1:]:
                        if re.match(r'^[CN]\s*\(', r):
                            type_val = r
                        else:
                            label_parts.append(r)
                    label = ' '.join(label_parts)
                    entries.append((num_items[0], varname, label, type_val))

    return current_table, entries


# ---------------------------------------------------------------------------
# Parse catalogue pages
# ---------------------------------------------------------------------------
def parse_catalogue_page_text(text):
    """Just return cleaned text for catalogue pages."""
    # Remove INEGI footer
    text = re.sub(r'INEGI\.\s*Encuesta Nacional.*?\d{4}\s*$', '', text, flags=re.MULTILINE)
    # Remove standalone page numbers
    text = re.sub(r'^\d{1,3}\s*$', '', text, flags=re.MULTILINE)
    return text.strip()


# ---------------------------------------------------------------------------
# OCR extraction for 2016
# ---------------------------------------------------------------------------
def ocr_page(page, zoom=3):
    """OCR a single page, return text."""
    import pytesseract
    from PIL import Image
    import io
    mat = fitz.Matrix(zoom, zoom)
    pix = page.get_pixmap(matrix=mat)
    img = Image.open(io.BytesIO(pix.tobytes("png")))
    return pytesseract.image_to_string(img, lang='eng')


def parse_ocr_var_description(text):
    """Parse OCR'd text from a variable description page into records."""
    variables = []
    # Split on variable headers
    parts = re.split(r'(#\d+\s+\w+\s*:)', text)

    i = 0
    while i < len(parts):
        header_match = re.match(r'#(\d+)\s+(\w+)\s*:', parts[i])
        if header_match and i + 1 < len(parts):
            var = OrderedDict()
            var['number'] = header_match.group(1)
            var['name'] = header_match.group(2)

            body = parts[i + 1]
            # Extract label (text before first field)
            label_match = re.match(r'\s*(.+?)(?:\n|Tipo\b)', body)
            var['label'] = label_match.group(1).strip() if label_match else ''

            # Type
            m = re.search(r'Tipo\s+([CN]\s*\([^)]+\))', body)
            var['type'] = m.group(1) if m else ''

            # Range
            m = re.search(r'Rango\s+(\{[^}]+\})', body)
            var['range'] = m.group(1) if m else ''

            # Cuestionario
            m = re.search(r'Cuestionario\s+(.+?)(?:\n|Secci)', body)
            var['cuestionario'] = m.group(1).strip() if m else ''

            # Seccion
            m = re.search(r'Secci[oó]n y/o apartado\s+(.+?)(?:\n)', body)
            var['seccion'] = m.group(1).strip() if m else ''

            # Definicion
            m = re.search(r'Definici[oó]n\s+(.+?)(?:Numero|Valor|Nota:|$)', body, re.DOTALL)
            var['definicion'] = ' '.join(m.group(1).split()) if m else ''

            # Numero de pregunta
            m = re.search(r'Numero de pregunta\s+(\S+)', body)
            var['num_pregunta'] = m.group(1) if m else ''

            # Pregunta textual
            m = re.search(r'Pregunta textual\s+(.+?)(?:Valor|Nota:|$)', body, re.DOTALL)
            var['pregunta'] = ' '.join(m.group(1).split()) if m else ''

            # Value labels
            var['values'] = []
            val_section = re.search(r'Valor\s+Etiqueta\s*\n(.*?)(?:Nota:|#\d+|$)', body, re.DOTALL)
            if val_section:
                for vm in re.finditer(r'(\d+|&)\s+(.+)', val_section.group(1)):
                    var['values'].append((vm.group(1), vm.group(2).strip()))

            # Notes
            var['notas'] = re.findall(r'(Nota:.+?)(?:\n|$)', body)

            variables.append(var)
            i += 2
        else:
            i += 1

    return variables


# ---------------------------------------------------------------------------
# Format a variable record as clean structured text
# ---------------------------------------------------------------------------
def format_variable(var, table_name=""):
    """Format a single variable record into clean text."""
    lines = []
    lines.append(f"  Variable #{var['number']}: {var['name']}")
    lines.append(f"    Label:        {var.get('label', '')}")
    lines.append(f"    Type:         {var.get('type', '')}")
    if var.get('range'):
        lines.append(f"    Range:        {var['range']}")
    if var.get('cuestionario'):
        lines.append(f"    Cuestionario: {var['cuestionario']}")
    if var.get('seccion'):
        lines.append(f"    Sección:      {var['seccion']}")
    if var.get('definicion'):
        lines.append(f"    Definición:   {var['definicion']}")
    if var.get('num_pregunta'):
        lines.append(f"    Pregunta #:   {var['num_pregunta']}")
    if var.get('pregunta'):
        lines.append(f"    Pregunta:     {var['pregunta']}")
    if var.get('values'):
        lines.append(f"    Value Labels:")
        for code, label in var['values']:
            lines.append(f"      {code:>6} = {label}")
    if var.get('notas'):
        for n in var['notas']:
            lines.append(f"    {n}")
    return '\n'.join(lines)


# ---------------------------------------------------------------------------
# Process one year
# ---------------------------------------------------------------------------
def process_year(f, year, pdf_path):
    """Process one PDF and write structured output."""
    doc = fitz.open(pdf_path)
    n_pages = doc.page_count
    use_ocr = (year == 2016)

    f.write(f"\n{'='*100}\n")
    f.write(f"  ENIGH {year} — {n_pages} PAGES\n")
    f.write(f"{'='*100}\n\n")

    # -------------------------------------------------------------------
    # PHASE 1: Extract raw text from EVERY page
    # -------------------------------------------------------------------
    print(f"  Phase 1: Extracting all {n_pages} pages...")
    all_pages_text = []
    all_pages_lines = []  # positioned lines for 2018+
    for i in range(n_pages):
        if i % 50 == 0:
            print(f"    Page {i+1}/{n_pages}...", flush=True)
        page = doc[i]
        if use_ocr:
            text = ocr_page(page)
            all_pages_text.append(text)
            all_pages_lines.append(None)
        else:
            text = page.get_text()
            positioned = get_positioned_lines(page)
            all_pages_text.append(text)
            all_pages_lines.append(positioned)
    print(f"    Extraction complete.")

    # -------------------------------------------------------------------
    # PHASE 2: Classify each page
    # -------------------------------------------------------------------
    print(f"  Phase 2: Classifying pages...")

    page_types = []
    for i, text in enumerate(all_pages_text):
        t = text.strip()
        if not t or len(t) < 20:
            page_types.append('empty')
        elif re.search(r'Descripci[oó]n de las tablas', t) and i < 20:
            page_types.append('table_desc')
        elif re.search(r'Lista de variables', t) and 'Tabla' in t:
            page_types.append('var_list')
        elif re.search(r'Tabla\s+\w+', t) and '#' in t and ('Variable' in t or 'Etiqueta' in t):
            page_types.append('var_list')
        elif re.search(r'#\d+\s+\w+\s*:', t) and ('Tipo' in t or 'tipo' in t.lower()):
            page_types.append('var_desc')
        elif re.search(r'(Continuaci[oó]n|Continu)', t) and ('Tipo' in t or 'Valor' in t or 'Etiqueta' in t):
            page_types.append('var_desc')
        elif re.search(r'Cat[aá]logo', t, re.IGNORECASE):
            page_types.append('catalogue')
        elif re.search(r'^C[oó]digo', t, re.MULTILINE):
            page_types.append('catalogue')
        elif i > 0 and page_types[-1] == 'catalogue':
            # Continuation of catalogue section
            page_types.append('catalogue')
        elif i > 0 and page_types[-1] == 'var_desc' and not re.search(r'Cat[aá]logo|Lista de variables', t):
            page_types.append('var_desc')
        elif i > 0 and page_types[-1] == 'var_list' and not re.search(r'#\d+\s+\w+\s*:|Cat[aá]logo', t):
            if 'Variable' in t or 'Etiqueta' in t or re.search(r'Tabla\s+\w+', t):
                page_types.append('var_list')
            else:
                page_types.append('other')
        else:
            page_types.append('other')

    # Print page classification summary
    from collections import Counter
    counts = Counter(page_types)
    print(f"    Classification: {dict(counts)}")

    # -------------------------------------------------------------------
    # PHASE 3: Write each page's content, structured by type
    # -------------------------------------------------------------------
    print(f"  Phase 3: Writing structured output...")

    # -- Introductory / table description pages --
    f.write(f"  --- INTRODUCTORY & TABLE DESCRIPTION PAGES ---\n\n")
    for i, (ptype, text) in enumerate(zip(page_types, all_pages_text)):
        if ptype in ('other', 'table_desc', 'empty'):
            cleaned = text.strip()
            cleaned = re.sub(r'INEGI\.\s*Encuesta Nacional.*?\d{4}\s*$', '', cleaned, flags=re.MULTILINE)
            cleaned = cleaned.strip()
            if cleaned and len(cleaned) > 20:
                f.write(f"  [Page {i+1}]\n")
                f.write(cleaned + "\n\n")

    # -- Variable list pages --
    f.write(f"\n{'─'*80}\n")
    f.write(f"  VARIABLE LIST BY TABLE — ENIGH {year}\n")
    f.write(f"  (Quick reference: every variable in every table)\n")
    f.write(f"{'─'*80}\n\n")

    current_table = None
    for i, ptype in enumerate(page_types):
        if ptype != 'var_list':
            continue
        text = all_pages_text[i]
        # Find table headers in this page
        for m in re.finditer(r'Tabla\s+(\w+)', text):
            new_table = m.group(1)
            if new_table != current_table:
                current_table = new_table
                f.write(f"\n  ┌─ Table: {current_table}\n")
                f.write(f"  │  {'#':>4}  {'Variable':<20} {'Label':<45} {'Type':<10}\n")
                f.write(f"  │  {'─'*4}  {'─'*20} {'─'*45} {'─'*10}\n")

        # Extract entries from positioned lines if available
        if not use_ocr and all_pages_lines[i]:
            plines = all_pages_lines[i]
            plines = [l for l in plines if not l[2].startswith("INEGI. Encuesta Nacional")]
            plines = [l for l in plines if not re.match(r'^\d{1,3}$', l[2]) or l[0] < 70]

            # Group by Y
            rows = []
            current_row = []
            last_y = None
            for x, y, t in plines:
                if last_y is not None and abs(y - last_y) > 3:
                    if current_row:
                        rows.append(current_row)
                    current_row = []
                current_row.append((x, y, t))
                last_y = y
            if current_row:
                rows.append(current_row)

            for row in rows:
                # Skip header rows and table name rows
                texts = [t for _, _, t in row]
                if any(t in ('Variable', 'Etiqueta', '#', 'Tipo') for t in texts):
                    continue
                if any(t.startswith('Tabla') for t in texts):
                    continue
                if any(t.startswith('(Contin') for t in texts):
                    continue

                # Process left half (x < 300) and right half (x >= 300) separately
                for half in [
                    sorted([(x, t) for x, _, t in row if x < 300], key=lambda p: p[0]),
                    sorted([(x, t) for x, _, t in row if x >= 300], key=lambda p: p[0])
                ]:
                    if not half:
                        continue
                    # First item should be number if x < 80
                    num = ''
                    varname = ''
                    label = ''
                    typ = ''
                    for x, t in half:
                        if re.match(r'^\d+$', t) and not num:
                            num = t
                        elif re.match(r'^[a-z_]\w*$', t, re.IGNORECASE) and not varname:
                            varname = t
                        elif re.match(r'^[CN]\s*\(', t):
                            typ = t
                        elif varname and not typ:
                            label = (label + ' ' + t).strip() if label else t
                    if varname:
                        f.write(f"  │  {num:>4}  {varname:<20} {label:<45} {typ:<10}\n")
        else:
            # OCR: simpler parsing
            for line in text.split('\n'):
                line = line.strip()
                if not line or line.startswith('INEGI') or line == '(Continua)':
                    continue
                m = re.match(r'^(\d+)\s+(\w+)\s+(.+?)\s+([CN]\s*\(.+?\))\s*$', line)
                if m:
                    f.write(f"  │  {m.group(1):>4}  {m.group(2):<20} {m.group(3):<45} {m.group(4):<10}\n")
                    continue
                # Two-column OCR line
                m = re.match(r'^(\d+)\s+(\w+)\s+(.+?)\s{2,}(\d+)\s+(\w+)\s+(.+?)$', line)
                if m:
                    f.write(f"  │  {m.group(1):>4}  {m.group(2):<20} {m.group(3):<45}\n")
                    f.write(f"  │  {m.group(4):>4}  {m.group(5):<20} {m.group(6):<45}\n")
                    continue

    f.write(f"  └{'─'*79}\n\n")

    # -- Variable description pages --
    f.write(f"\n{'─'*80}\n")
    f.write(f"  DETAILED VARIABLE DESCRIPTIONS — ENIGH {year}\n")
    f.write(f"  (Every variable with its label, type, range, definition,\n")
    f.write(f"   question text, and value labels)\n")
    f.write(f"{'─'*80}\n\n")

    current_table = None
    all_variables = []

    for i, ptype in enumerate(page_types):
        if ptype != 'var_desc':
            continue

        text = all_pages_text[i]

        # Check for table headers
        for m in re.finditer(r'Tabla\s+(\w+)', text):
            new_table = m.group(1)
            if new_table != current_table and new_table.isupper():
                current_table = new_table
                f.write(f"\n  ══════════════════════════════════════════════════\n")
                f.write(f"  Table: {current_table}\n")
                f.write(f"  ══════════════════════════════════════════════════\n\n")

        # Parse variables from this page
        if use_ocr:
            page_vars = parse_ocr_var_description(text)
        else:
            if all_pages_lines[i]:
                page_vars = parse_var_description_page(all_pages_lines[i])
            else:
                page_vars = parse_ocr_var_description(text)

        for var in page_vars:
            f.write(format_variable(var) + "\n\n")
            all_variables.append(var)

    print(f"    Wrote {len(all_variables)} variable descriptions")

    # -- Catalogue pages --
    f.write(f"\n{'─'*80}\n")
    f.write(f"  CODE CATALOGUES — ENIGH {year}\n")
    f.write(f"  (Code → description mappings for categorical variables)\n")
    f.write(f"{'─'*80}\n\n")

    for i, ptype in enumerate(page_types):
        if ptype != 'catalogue':
            continue
        text = all_pages_text[i]
        cleaned = parse_catalogue_page_text(text)
        if cleaned:
            f.write(f"  [Page {i+1}]\n")
            f.write(cleaned + "\n\n")

    doc.close()
    return len(all_variables)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
def main():
    with open(OUTPUT_FILE, "w", encoding="utf-8") as f:
        f.write(f"{'#'*100}\n")
        f.write(f"""
  ENIGH VARIABLE DOCUMENTATION — COMPLETE REFERENCE
  ==================================================
  Encuesta Nacional de Ingresos y Gastos de los Hogares
  (National Household Income and Expenditure Survey — Mexico, INEGI)
  Years: 2016, 2018, 2020, 2022, 2024

  For each variable:
    - Variable name (as in .dta files)
    - Label (Spanish description)
    - Type: C(n) = character width n, N(n) = numeric width n
    - Range of valid values
    - Definition
    - Survey question
    - Value labels: code → description

  Common conventions:
    1 = Sí, 2 = No (for yes/no variables)
    & = not specified
    folioviv = housing unit ID, foliohog = household ID
    numren = person ID, factor = expansion weight
    entidad = state code (01-32)

""")
        f.write(f"{'#'*100}\n\n")

        total_vars = 0
        for year in YEARS:
            pdf_path = os.path.join(DOCS_DIR, f"ENIGH{year}.pdf")
            if not os.path.exists(pdf_path):
                f.write(f"\n  *** PDF NOT FOUND: ENIGH{year}.pdf ***\n\n")
                continue
            print(f"\nProcessing ENIGH {year}...")
            n = process_year(f, year, pdf_path)
            total_vars += n

        f.write(f"\n{'#'*100}\n")
        f.write(f"  END — {total_vars} total variable descriptions extracted\n")
        f.write(f"{'#'*100}\n")

    size = os.path.getsize(OUTPUT_FILE)
    print(f"\nDone! {OUTPUT_FILE}")
    print(f"Size: {size / 1024 / 1024:.1f} MB, {total_vars} variables")


if __name__ == "__main__":
    main()
