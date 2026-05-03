#!/usr/bin/env python3
"""
Extract EVERY page of EVERY ENIGH PDF (2016–2024) and produce a single
structured text file documenting every variable with its label, type,
range, definition, question, and value labels.

2016: OCR (broken font encoding)
2018–2024: position-aware text extraction
"""

import fitz
import os
import re
import sys
from collections import OrderedDict

DOCS_DIR = os.path.dirname(os.path.abspath(__file__))
OUTPUT = os.path.join(DOCS_DIR, "enigh-variable-documentation-raw.txt")
YEARS = [2016, 2018, 2020, 2022, 2024]
LEFT_BOUNDARY = 265  # x-coordinate separating left/right columns


# ═══════════════════════════════════════════════════════════════════════
# Text extraction helpers
# ═══════════════════════════════════════════════════════════════════════

def positioned_lines(page):
    """Extract (x, y, text) tuples sorted by y then x."""
    out = []
    for b in page.get_text("dict")["blocks"]:
        if "lines" not in b:
            continue
        for ln in b["lines"]:
            t = "".join(s["text"] for s in ln["spans"]).strip()
            if t:
                out.append((ln["bbox"][0], ln["bbox"][1], t))
    out.sort(key=lambda r: (r[1], r[0]))
    return out


def ocr_page(page, zoom=3):
    import pytesseract
    from PIL import Image
    import io
    pix = page.get_pixmap(matrix=fitz.Matrix(zoom, zoom))
    img = Image.open(io.BytesIO(pix.tobytes("png")))
    return pytesseract.image_to_string(img, lang='eng')


def ocr_positioned_lines(page, zoom=3):
    """OCR a page and return positioned (x, y, text) lines using word
    bounding boxes from tesseract.  This preserves column layout because
    each word gets its own x,y coordinate — no column-jumbling."""
    import pytesseract
    from PIL import Image
    import io

    mat = fitz.Matrix(zoom, zoom)
    pix = page.get_pixmap(matrix=mat)
    img = Image.open(io.BytesIO(pix.tobytes("png")))
    data = pytesseract.image_to_data(img, lang='eng',
                                     output_type=pytesseract.Output.DICT)

    lines = []
    cur_words = []
    cur_key = None
    n = len(data['text'])
    for i in range(n):
        txt = data['text'][i].strip()
        key = (data['block_num'][i], data['par_num'][i], data['line_num'][i])
        if key != cur_key:
            if cur_words:
                full_text = ' '.join(w[2] for w in cur_words)
                if full_text.strip():
                    lines.append((cur_words[0][0] / zoom,
                                  cur_words[0][1] / zoom,
                                  full_text.strip()))
            cur_words = []
            cur_key = key
        if txt:
            cur_words.append((data['left'][i], data['top'][i], txt))
    if cur_words:
        full_text = ' '.join(w[2] for w in cur_words)
        if full_text.strip():
            lines.append((cur_words[0][0] / zoom,
                          cur_words[0][1] / zoom,
                          full_text.strip()))

    lines.sort(key=lambda r: (r[1], r[0]))
    return lines


def clean_footer(text):
    return re.sub(
        r'INEGI\.\s*Encuesta Nacional de Ingresos y Gastos de los Hogares.*?(?:\d{4})\s*$',
        '', text, flags=re.MULTILINE
    )


# ═══════════════════════════════════════════════════════════════════════
# Parse variable descriptions from positioned lines (2018-2024)
# ═══════════════════════════════════════════════════════════════════════

def parse_positioned_var_page(raw_lines):
    """
    Given positioned lines from a variable-description page,
    return a list of structured variable dicts.
    """
    # Filter noise
    lines = [
        (x, y, t) for x, y, t in raw_lines
        if not t.startswith("INEGI. Encuesta Nacional")
        and not (re.match(r'^\d{1,3}$', t) and y > 700)  # page numbers at bottom
    ]

    # Separate into left/right columns
    left = [(x, y, t) for x, y, t in lines if x < LEFT_BOUNDARY]
    right = [(x, y, t) for x, y, t in lines if x >= LEFT_BOUNDARY]

    # Find variable entry start positions (from left column)
    var_headers = []
    for x, y, t in left:
        m = re.match(r'^#(\d+)\s+(\w+)\s*:\s*(.*)', t)
        if m:
            var_headers.append((y, m.group(1), m.group(2), m.group(3).strip()))

    if not var_headers:
        return []

    # Build y-ranges for each variable
    variables = []
    for vi in range(len(var_headers)):
        start_y = var_headers[vi][0]
        end_y = var_headers[vi + 1][0] if vi + 1 < len(var_headers) else 9999

        var = OrderedDict()
        var['number'] = var_headers[vi][1]
        var['name'] = var_headers[vi][2]
        var['label'] = var_headers[vi][3]
        var['type'] = ''
        var['range'] = ''
        var['cuestionario'] = ''
        var['seccion'] = ''
        var['definicion'] = ''
        var['num_pregunta'] = ''
        var['pregunta'] = ''
        var['values'] = []
        var['notas'] = []

        # Gather left and right lines for this variable's y-range
        vl = [(x, y, t) for x, y, t in left if start_y < y < end_y]
        vr = [(x, y, t) for x, y, t in right if start_y <= y < end_y]

        # --- Parse LEFT column ---
        # Group left lines by similar y (tolerance 4)
        vl_rows = _group_by_y(vl, tol=4)

        in_values = False
        def_fragments = []
        for row in vl_rows:
            row.sort(key=lambda r: r[0])
            first_x, first_y, first_t = row[0]
            rest_texts = [t for x, _, t in row[1:]]

            if first_t.startswith('Tipo') and first_x < 60:
                # Handle both "Tipo" + separate value and "Tipo C (1)" merged
                tipo_val = re.sub(r'^Tipo\s*', '', first_t).strip()
                all_vals = ([tipo_val] if tipo_val else []) + rest_texts
                var['type'] = ' '.join(all_vals).strip()
                in_values = False
            elif first_t.startswith('Rango') and first_x < 60:
                rango_val = re.sub(r'^Rango\s*', '', first_t).strip()
                all_vals = ([rango_val] if rango_val else []) + rest_texts
                var['range'] = ' '.join(all_vals).strip()
                in_values = False
            elif first_t.startswith('Definici') and first_x < 60:
                # Handle "Definición" + separate text and "Definición text..." merged
                def_val = re.sub(r'^Definici[oó]n\s*[-—_]*\s*', '', first_t).strip()
                all_vals = ([def_val] if def_val else []) + rest_texts
                if any(v for v in all_vals):
                    def_fragments.extend(v for v in all_vals if v)
                in_values = False
            elif first_t == 'Valor' or first_t == 'Etiqueta':
                in_values = True
            elif first_t.startswith('Valor') and 'Etiqueta' in first_t:
                in_values = True
            elif first_t.startswith('Nota:'):
                var['notas'].append(first_t + (' ' + ' '.join(rest_texts) if rest_texts else ''))
                in_values = False
            elif first_t in ('(Continúa)', '(Continuación)', '(Continua)'):
                pass
            elif in_values and first_x > 55 and first_x < 90:
                # value code + label — handle both separate and merged forms
                code = first_t
                label = ' '.join(rest_texts)
                if not label:
                    # OCR merged code and label: "1 Tierra" or "01 Material de desecho"
                    vm = re.match(r'^(\d+|&)\s+(.+)', first_t)
                    if vm:
                        code = vm.group(1)
                        label = vm.group(2)
                if label:
                    var['values'].append((code, label))
            elif first_x > 80 and first_x < 200 and not in_values:
                # Likely a continuation of the definition text
                row_text = ' '.join([t for _, _, t in row])
                def_fragments.append(row_text)

        var['definicion'] = ' '.join(def_fragments).strip()

        # --- Parse RIGHT column ---
        vr_rows = _group_by_y(vr, tol=4)
        pregunta_fragments = []
        for row in vr_rows:
            row.sort(key=lambda r: r[0])
            first_x, first_y, first_t = row[0]
            rest_texts = [t for x, _, t in row[1:]]

            if first_t.startswith('Cuestionario'):
                # Rest might be on same line or next line at x > 350
                val = ' '.join(rest_texts).strip()
                if val:
                    var['cuestionario'] = val
            elif 'apartado' in first_t.lower() or first_t.startswith('Seccion y/o') or first_t.startswith('Sección y/o'):
                val = ' '.join(rest_texts).strip()
                if val:
                    var['seccion'] = val
            elif first_t.startswith('Número de pregunta') or first_t.startswith('Numero de pregunta'):
                var['num_pregunta'] = ' '.join(rest_texts)
            elif first_t.startswith('Pregunta textual') or first_t.startswith('Pregunta Textual'):
                pass  # the text is usually on a separate line
            elif first_x > 350:
                # Could be question number or question text
                # If it's a bare number, treat as pregunta number
                if re.match(r'^\d+(\.\d+)?$', first_t) and not var['num_pregunta']:
                    var['num_pregunta'] = first_t
                else:
                    pregunta_fragments.append(first_t + (' ' + ' '.join(rest_texts) if rest_texts else ''))

        var['pregunta'] = ' '.join(pregunta_fragments).strip()

        variables.append(var)

    return variables


def _group_by_y(lines, tol=4):
    """Group lines by approximate y-coordinate."""
    if not lines:
        return []
    lines = sorted(lines, key=lambda l: (l[1], l[0]))
    groups = [[lines[0]]]
    for l in lines[1:]:
        if abs(l[1] - groups[-1][-1][1]) <= tol:
            groups[-1].append(l)
        else:
            groups.append([l])
    return groups


# ═══════════════════════════════════════════════════════════════════════
# Parse variable descriptions from OCR text (2016)
# ═══════════════════════════════════════════════════════════════════════

def parse_ocr_var_page(text):
    """Parse OCR'd text into structured variable records."""
    variables = []
    # Split on variable headers
    parts = re.split(r'(#\d+\s+\w+\s*:)', text)

    i = 0
    while i < len(parts):
        m = re.match(r'#(\d+)\s+(\w+)\s*:', parts[i])
        if m and i + 1 < len(parts):
            var = OrderedDict()
            var['number'] = m.group(1)
            var['name'] = m.group(2)
            body = parts[i + 1]

            # Label: text before first field keyword
            lm = re.match(r'\s*(.+?)(?:\n|Tipo\b|$)', body)
            var['label'] = lm.group(1).strip() if lm else ''

            # Type
            tm = re.search(r'Tipo\s+([CN]\s*\([^)]+\))', body)
            var['type'] = tm.group(1).strip() if tm else ''

            # Range
            rm = re.search(r'Rango\s+(\{[^}]+\})', body)
            var['range'] = rm.group(1) if rm else ''

            # Cuestionario
            cm = re.search(r'Cuestionario\s+(.+?)(?:\n|Secci)', body)
            var['cuestionario'] = cm.group(1).strip() if cm else ''

            # Seccion
            sm = re.search(r'Secci[oó]n y/o apartado\s+(.+?)(?:\n)', body)
            var['seccion'] = sm.group(1).strip() if sm else ''

            # Definicion
            dm = re.search(r'Definici[oó]n\s+(.+?)(?:Numero|Valor|Nota:|$)', body, re.DOTALL)
            var['definicion'] = ' '.join(dm.group(1).split()).strip() if dm else ''

            # Num pregunta
            nm = re.search(r'Numero de pregunta\s+(\S+)', body)
            var['num_pregunta'] = nm.group(1) if nm else ''

            # Pregunta
            pm = re.search(r'Pregunta textual\s+(.+?)(?:Valor|Nota:|$)', body, re.DOTALL)
            var['pregunta'] = ' '.join(pm.group(1).split()).strip() if pm else ''

            # Values
            var['values'] = []
            vs = re.search(r'Valor\s+Etiqueta\s*\n(.*?)(?:Nota:|#\d+|$)', body, re.DOTALL)
            if vs:
                for vm in re.finditer(r'(\d+|&)\s+(.+)', vs.group(1)):
                    var['values'].append((vm.group(1), vm.group(2).strip()))

            # Notes
            var['notas'] = re.findall(r'(Nota:.+?)(?:\n|$)', body)

            variables.append(var)
            i += 2
        else:
            i += 1
    return variables


# ═══════════════════════════════════════════════════════════════════════
# Format a variable record
# ═══════════════════════════════════════════════════════════════════════

def fmt_var(v):
    """Format one variable record as clean structured text."""
    out = []
    out.append(f"  Variable #{v['number']}: {v['name']}")
    out.append(f"    Label:          {v.get('label', '')}")
    if v.get('type'):
        out.append(f"    Type:           {v['type']}")
    if v.get('range'):
        out.append(f"    Range:          {v['range']}")
    if v.get('cuestionario'):
        out.append(f"    Cuestionario:   {v['cuestionario']}")
    if v.get('seccion'):
        out.append(f"    Sección:        {v['seccion']}")
    if v.get('definicion'):
        out.append(f"    Definición:     {v['definicion']}")
    if v.get('num_pregunta'):
        out.append(f"    Pregunta #:     {v['num_pregunta']}")
    if v.get('pregunta'):
        out.append(f"    Pregunta:       {v['pregunta']}")
    if v.get('values'):
        out.append(f"    Value Labels:")
        for code, label in v['values']:
            out.append(f"      {code:>6} = {label}")
    for n in v.get('notas', []):
        out.append(f"    {n}")
    return '\n'.join(out)


# ═══════════════════════════════════════════════════════════════════════
# Process one year
# ═══════════════════════════════════════════════════════════════════════

def process_year(f, year, pdf_path):
    doc = fitz.open(pdf_path)
    n_pages = doc.page_count
    use_ocr = (year == 2016)

    f.write(f"\n{'='*100}\n")
    f.write(f"  ENIGH {year}  ({n_pages} pages)\n")
    f.write(f"{'='*100}\n\n")

    # ── Extract every single page ──
    print(f"  Extracting {n_pages} pages {'(OCR)' if use_ocr else '(text)'}...")
    raw_texts = []
    pos_lines = []
    for i in range(n_pages):
        if use_ocr and i % 20 == 0:
            print(f"    OCR page {i+1}/{n_pages}...", flush=True)
        page = doc[i]
        if use_ocr:
            # OCR left/right halves separately to preserve column layout
            olines = ocr_positioned_lines(page)
            ocr_text = '\n'.join(t for _, _, t in olines)
            raw_texts.append(ocr_text)
            pos_lines.append(olines)
        else:
            raw_texts.append(page.get_text())
            pos_lines.append(positioned_lines(page))

    # ── Write every page, classifying as we go ──
    total_vars = 0
    current_table = None
    in_var_desc_section = False
    in_catalogue_section = False
    in_var_list_section = False

    for i in range(n_pages):
        text = raw_texts[i]
        plines = pos_lines[i]
        cleaned = clean_footer(text).strip()
        if not cleaned or len(cleaned) < 10:
            continue

        # Detect section transitions
        is_var_list = bool(re.search(r'Lista de variables', text)) and ('Tabla' in text or 'Variable' in text)
        is_var_desc_start = bool(re.search(r'Descripci[oó]n de las variables', text)) and bool(re.search(r'#\d+\s+\w+\s*:', text))
        is_catalogue_start = bool(re.search(r'Cat[aá]logos?\s+de\s+c[oó]digos', text))
        has_var_entries = bool(re.search(r'#\d+\s+\w+\s*:', text))

        # Track table names
        for tm in re.finditer(r'Tabla\s+([A-Z][A-Z_]+)', text):
            new_table = tm.group(1)
            if new_table != current_table:
                current_table = new_table
                f.write(f"\n  {'═'*60}\n")
                f.write(f"  Table: {current_table}\n")
                f.write(f"  {'═'*60}\n\n")

        if is_catalogue_start:
            in_catalogue_section = True
            in_var_desc_section = False
            in_var_list_section = False
            f.write(f"\n{'─'*80}\n")
            f.write(f"  CODE CATALOGUES — ENIGH {year}\n")
            f.write(f"{'─'*80}\n\n")

        if is_var_desc_start:
            in_var_desc_section = True
            in_var_list_section = False
            in_catalogue_section = False

        if is_var_list and not in_var_desc_section and not in_catalogue_section:
            in_var_list_section = True

        # ── CATALOGUE PAGES: output cleaned text ──
        if in_catalogue_section:
            f.write(f"  [Page {i+1}]\n")
            f.write(clean_footer(text).strip() + "\n\n")
            continue

        # ── VARIABLE DESCRIPTION PAGES: parse structured records ──
        if (in_var_desc_section or has_var_entries) and not in_catalogue_section:
            if not in_var_desc_section and has_var_entries:
                in_var_desc_section = True

            if use_ocr:
                page_vars = parse_positioned_var_page(plines) if plines else []
            elif plines:
                page_vars = parse_positioned_var_page(plines)
            else:
                page_vars = []

            if page_vars:
                f.write(f"  [Page {i+1}]\n")
                for v in page_vars:
                    f.write(fmt_var(v) + "\n\n")
                total_vars += len(page_vars)
            else:
                # Page is a continuation or has content we couldn't parse as vars
                # Output the raw text so nothing is lost
                f.write(f"  [Page {i+1}]\n")
                f.write(clean_footer(text).strip() + "\n\n")
            continue

        # ── VARIABLE LIST PAGES: parse tabular format ──
        if in_var_list_section:
            f.write(f"  [Page {i+1}]\n")
            if not use_ocr and plines:
                _write_var_list_positioned(f, plines)
            else:
                f.write(clean_footer(text).strip() + "\n\n")
            continue

        # ── ALL OTHER PAGES (intro, table desc, etc): output raw ──
        f.write(f"  [Page {i+1}]\n")
        f.write(clean_footer(text).strip() + "\n\n")

    doc.close()
    print(f"    {total_vars} structured variable records written")
    return total_vars


def _write_var_list_positioned(f, raw_lines):
    """Write a variable-list page from positioned lines."""
    lines = [
        (x, y, t) for x, y, t in raw_lines
        if not t.startswith("INEGI. Encuesta Nacional")
        and not (re.match(r'^\d{1,3}$', t) and x > 500)
    ]

    rows = _group_by_y(lines, tol=3)

    for row in rows:
        row.sort(key=lambda r: r[0])
        texts = [t for _, _, t in row]

        # Skip pure header rows
        if all(t in ('#', 'Variable', 'Etiqueta', 'Tipo') for t in texts):
            continue
        # Skip table name rows (already handled)
        if any(re.match(r'^Tabla\s+', t) for t in texts):
            tname = [t for t in texts if re.match(r'^Tabla\s+', t)]
            f.write(f"\n  Table: {tname[0]}\n")
            continue
        if any(t.startswith('(Contin') for t in texts):
            continue

        # Parse each half (left < 300, right >= 300) as potential entries
        for half in [
            sorted([(x, t) for x, _, t in row if x < 300], key=lambda p: p[0]),
            sorted([(x, t) for x, _, t in row if x >= 300], key=lambda p: p[0])
        ]:
            if not half:
                continue
            num = varname = label = typ = ''
            for x, t in half:
                if re.match(r'^\d+$', t) and not num:
                    num = t
                elif re.match(r'^[a-z_]\w*$', t, re.IGNORECASE) and not varname and len(t) > 1:
                    varname = t
                elif re.match(r'^[CN]\s*\(', t):
                    typ = t
                elif varname and not typ:
                    label = (label + ' ' + t).strip() if label else t
            if varname:
                f.write(f"    {num:>4}  {varname:<20} {label:<50} {typ}\n")

    f.write("\n")


# ═══════════════════════════════════════════════════════════════════════
# Main
# ═══════════════════════════════════════════════════════════════════════

def main():
    with open(OUTPUT, "w", encoding="utf-8") as f:
        f.write(f"{'#'*100}\n")
        f.write("""
  ENIGH VARIABLE DOCUMENTATION — COMPLETE REFERENCE
  ==================================================
  Encuesta Nacional de Ingresos y Gastos de los Hogares
  (National Household Income and Expenditure Survey — Mexico, INEGI)
  Years: 2016, 2018, 2020, 2022, 2024

  Every page of every PDF has been extracted and included below.
  Variable descriptions are parsed into structured records.
  Catalogue pages are included as-is.

  For each variable:
    - Variable name (as in .dta files)
    - Label (Spanish description)
    - Type: C(n) = character width n, N(n) = numeric width n
    - Range of valid values
    - Definición (definition)
    - Pregunta textual (survey question)
    - Value labels: code = description

  Conventions:
    1 = Sí, 2 = No (yes/no variables)
    & = no especificado (not specified)
    folioviv = housing unit ID, foliohog = household ID
    numren = person ID, factor = expansion weight
    entidad = state code (01-32)

""")
        f.write(f"{'#'*100}\n\n")

        total = 0
        for year in YEARS:
            pdf = os.path.join(DOCS_DIR, f"ENIGH{year}.pdf")
            if not os.path.exists(pdf):
                f.write(f"\n  *** ENIGH{year}.pdf NOT FOUND ***\n\n")
                continue
            print(f"\n{'='*60}")
            print(f"ENIGH {year}")
            print(f"{'='*60}")
            total += process_year(f, year, pdf)

        f.write(f"\n{'#'*100}\n")
        f.write(f"  Total: {total} structured variable records\n")
        f.write(f"  Source: ENIGH 2016–2024 official INEGI documentation PDFs\n")
        f.write(f"{'#'*100}\n")

    sz = os.path.getsize(OUTPUT)
    print(f"\nDone → {OUTPUT}")
    print(f"  {sz/1024/1024:.1f} MB, {total} variables")


if __name__ == "__main__":
    main()
