#!/usr/bin/env python3
"""
Clean the enigh-variable-documentation.txt file:
1. Remove all non-dictionary pages (intro, TOC, variable lists, catalogues, errata)
2. Fix 2016 OCR character errors
3. Fix P00 code letter-O-for-zero errors
4. Clean garbled Definición fields
5. Remove "Fe de erratas" artifacts from field text
6. Remove column-header bleed from fields
"""

import re
import os

DOCS_DIR = os.path.dirname(os.path.abspath(__file__))
RAW_INPUT = os.path.join(DOCS_DIR, "enigh-variable-documentation-raw.txt")
OUTPUT = os.path.join(DOCS_DIR, "enigh-variable-documentation.txt")


def main():
    with open(RAW_INPUT, "r", encoding="utf-8") as f:
        text = f.read()

    lines = text.split('\n')
    total = len(lines)
    print(f"Input: {total} lines")

    # ── Phase 1: Parse into blocks ──
    # Identify year boundaries, table headers, variable records, and junk
    blocks = parse_blocks(lines)

    # ── Phase 1b: Extract catalogues from catalogue blocks ──
    catalogues_by_year = extract_catalogues(lines, blocks)

    # ── Phase 2: Filter to keep only variable records + structure ──
    kept = filter_blocks(blocks)

    # ── Phase 2b: Inject catalogue value labels into matching variables ──
    kept = inject_catalogues(kept, catalogues_by_year)

    # ── Phase 3: Fix OCR and formatting issues in each variable block ──
    fixed = fix_all_blocks(kept)

    # ── Phase 4: Write output ──
    write_output(fixed)


def parse_blocks(lines):
    """Parse the file into typed blocks."""
    blocks = []
    i = 0
    n = len(lines)

    while i < n:
        line = lines[i]

        # File header (####... block at top)
        if line.startswith('####') and i < 5:
            block_lines = []
            while i < n and not (lines[i].startswith('====') and 'ENIGH' in lines[min(i+1, n-1)]):
                block_lines.append(lines[i])
                i += 1
            blocks.append(('header', block_lines))
            continue

        # Year header
        if line.startswith('====') and i + 1 < n and re.match(r'\s*ENIGH \d{4}', lines[i + 1]):
            block_lines = [line, lines[i + 1]]
            i += 2
            if i < n and lines[i].startswith('===='):
                block_lines.append(lines[i])
                i += 1
            blocks.append(('year_header', block_lines))
            continue

        # Table header (═══)
        if '═' * 10 in line:
            block_lines = [line]
            i += 1
            while i < n and '═' * 10 not in lines[i]:
                block_lines.append(lines[i])
                i += 1
            if i < n:
                block_lines.append(lines[i])
                i += 1
            blocks.append(('table_header', block_lines))
            continue

        # Variable record
        if re.match(r'\s*Variable #\d+:', line):
            block_lines = [line]
            i += 1
            while i < n:
                next_line = lines[i]
                # End conditions: next variable, table header, year header, page marker, catalogue
                if re.match(r'\s*Variable #\d+:', next_line):
                    break
                if '═' * 10 in next_line:
                    break
                if next_line.startswith('====') and i + 1 < n and 'ENIGH' in lines[min(i + 1, n - 1)]:
                    break
                if next_line.startswith('####'):
                    break
                if '─' * 10 in next_line:
                    break
                # [Page N] marker starts a new context
                if re.match(r'\s*\[Page \d+\]', next_line):
                    # Peek: if next non-empty line is a variable, this page marker is fine
                    j = i + 1
                    while j < n and lines[j].strip() == '':
                        j += 1
                    if j < n and re.match(r'\s*Variable #\d+:', lines[j]):
                        break  # let the page marker be picked up as 'other'
                    else:
                        break
                block_lines.append(next_line)
                i += 1
            blocks.append(('variable', block_lines))
            continue

        # [Page N] marker
        if re.match(r'\s*\[Page \d+\]', line):
            blocks.append(('page_marker', [line]))
            i += 1
            continue

        # Catalogue section header
        if '─' * 10 in line and i + 1 < n and 'CATALOGUE' in lines[min(i + 1, n - 1)].upper():
            block_lines = [line]
            i += 1
            while i < n and not (re.match(r'\s*Variable #\d+:', lines[i]) or
                                 lines[i].startswith('====') or
                                 '═' * 10 in lines[i]):
                block_lines.append(lines[i])
                i += 1
            blocks.append(('catalogue', block_lines))
            continue

        # Everything else (intro, TOC, variable lists, raw text, etc.)
        block_lines = [line]
        i += 1
        while i < n:
            next_line = lines[i]
            if re.match(r'\s*Variable #\d+:', next_line):
                break
            if '═' * 10 in next_line:
                break
            if next_line.startswith('====') and i + 1 < n and 'ENIGH' in lines[min(i + 1, n - 1)]:
                break
            if re.match(r'\s*\[Page \d+\]', next_line):
                break
            if '─' * 10 in next_line:
                break
            if next_line.startswith('####'):
                break
            block_lines.append(next_line)
            i += 1
        blocks.append(('other', block_lines))
        continue

    print(f"Parsed {len(blocks)} blocks")
    counts = {}
    for typ, _ in blocks:
        counts[typ] = counts.get(typ, 0) + 1
    for typ, cnt in sorted(counts.items()):
        print(f"  {typ}: {cnt}")
    return blocks


def filter_blocks(blocks):
    """Keep only file header, year headers, table headers, and variable records.
    Remove table headers that have no variables after them (orphaned from var-list pages)."""
    # First pass: identify which table headers have variables after them
    kept = []
    for typ, block_lines in blocks:
        if typ in ('header', 'year_header', 'table_header', 'variable'):
            kept.append((typ, block_lines))
        # Skip: 'other', 'page_marker', 'catalogue'

    # Second pass: remove table headers that are followed by another table header
    # or a year header (i.e., no variables between them)
    final = []
    for i, (typ, block_lines) in enumerate(kept):
        if typ == 'table_header':
            # Look ahead: is there a variable before the next table_header or year_header?
            has_var = False
            for j in range(i + 1, len(kept)):
                if kept[j][0] == 'variable':
                    has_var = True
                    break
                if kept[j][0] in ('table_header', 'year_header'):
                    break
            if has_var:
                final.append((typ, block_lines))
            # else: skip orphaned table header
        else:
            final.append((typ, block_lines))

    print(f"Kept {len(final)} blocks (removed {len(blocks) - len(final)})")
    return final


# ═══════════════════════════════════════════════════════════════════════
# Catalogue extraction and injection
# ═══════════════════════════════════════════════════════════════════════

# Known catalogue header patterns → canonical name
CATALOGUE_NAMES = [
    ('parentesco',              'parentesco'),
    ('residencia',              'residencia'),
    ('entidades federativas',   'entidades'),
    ('lengua ind',              'lengua'),
    ('gastos con tarjeta',      'gastos_tarjeta'),
    ('gastos de negocios agropecuarios', 'gastos_agropecuarios'),
    ('gastos de negocios industriales',  'gastos_industriales'),
    ('gastos de negocios',      'gastos_negocios'),
    ('gastos',                  'gastos'),
    ('ingresos',                'ingresos'),
    ('productos agr',           'productos_agricolas'),
    ('unidades de medida',      'unidades'),
    ('cantidades',              'cantidades'),
    ('rubros de gasto',         'rubros'),
    ('mes_dia',                 'mes_dia'),
    ('fecha_adqu',              'fechas'),
]

# Map Range field text → catalogue canonical name
# Order matters: more specific patterns first
RANGE_TO_CATALOGUE = [
    ('gastos con tarjeta',       'gastos_tarjeta'),
    ('gastos de negocios agropecuarios', 'gastos_negocios'),
    ('gastos de negocios industriales',  'gastos_industriales'),
    ('negocios agropecuarios',   'gastos_negocios'),
    ('negocios industriales',    'gastos_industriales'),
    ('parentesco',               'parentesco'),
    ('lengua',                   'lengua'),
    ('residencia',               'residencia'),
    ('gastos',                   'gastos'),
    ('ingresos',                 'ingresos'),
    ('entidades',                'entidades'),
    ('productos',                'productos_agricolas'),
    ('cantidades',               'cantidades'),
    ('unidades',                 'unidades'),
    ('mes y d',                  'mes_dia'),
    ('mes_dia',                  'mes_dia'),
    ('fecha',                    'fechas'),  # catches all fecha variants
]

# Code patterns for identifying catalogue entry codes
CODE_PATTERNS = [
    re.compile(r'^[A-Z]\d{3}$'),           # A001, T916, etc.
    re.compile(r'^P\d{3}$'),               # P001
    re.compile(r'^P1\d{2}$'),              # P101-P108
    re.compile(r'^T[BR]\d{2}$'),           # TB01, TR15
    re.compile(r'^\d{2,3}$'),              # 01-999 (parentesco, residencia, estados)
    re.compile(r'^\d{4}$'),                # 0000-9999 (mes_dia, fechas)
    re.compile(r'^\d{6}$'),                # 011112 (2024 gastos 6-digit)
    re.compile(r'^[A-F]\d{2}$'),           # B00-F19 (negocios agropecuarios)
]


def is_code(text):
    """Check if text looks like a catalogue code."""
    text = text.strip()
    for pat in CODE_PATTERNS:
        if pat.match(text):
            return True
    return False


def classify_catalogue(header_text):
    """Classify a catalogue header into a canonical name."""
    ht = header_text.lower()
    for pattern, name in CATALOGUE_NAMES:
        if pattern in ht:
            return name
    return None


def extract_catalogues(lines, blocks):
    """Extract catalogue code→description mappings from the raw file.
    Returns: {year: {catalogue_name: [(code, description), ...]}}
    """
    catalogues = {}  # year → {name → [(code, desc)]}

    # Find year boundaries from the raw lines
    year_boundaries = []  # [(year, start_line)]
    for i, line in enumerate(lines):
        if line.startswith('====') and i + 1 < len(lines):
            ym = re.search(r'ENIGH (\d{4})', lines[i + 1])
            if ym:
                year_boundaries.append((int(ym.group(1)), i))

    # Build year → (start, end) ranges
    year_ranges = {}
    for idx, (year, start) in enumerate(year_boundaries):
        if idx + 1 < len(year_boundaries):
            end = year_boundaries[idx + 1][1] - 1
        else:
            end = len(lines) - 1
        year_ranges[year] = (start, end)

    # For each year, find standalone "Catálogo de X" headers and parse entries
    # Standalone = not indented (no leading spaces) or starts with "2.4"
    # Also match plural "Catálogos de ..." which is a section header
    cat_header_any_re = re.compile(
        r'^(?:\d+\.\d+(?:\.\d+(?:\.\d+)?)?\s+)?Cat[aá]logos?\s+(?:de\s+|para\s+)(.*)',
        re.IGNORECASE
    )

    for year, (yr_start, yr_end) in sorted(year_ranges.items()):
        catalogues[year] = {}

        # Find ALL catalogue-like header positions within this year (for boundaries)
        all_cat_lines = []  # [(line_idx, canonical_name_or_None)]
        for i in range(yr_start, yr_end + 1):
            line = lines[i].strip()
            # Skip Range: fields (indented lines)
            if lines[i].startswith('    '):
                continue
            m = cat_header_any_re.match(line)
            if m:
                cat_name_text = m.group(1).strip()
                # Skip if it's SINCO or SCIAN
                if any(skip in cat_name_text.upper() for skip in ['SINCO', 'SCIAN']):
                    continue
                canonical = classify_catalogue('catálogo de ' + cat_name_text)
                all_cat_lines.append((i, canonical))

        # Parse only the classified ones, using all headers as boundaries
        for idx, (cat_line, cat_name) in enumerate(all_cat_lines):
            if cat_name is None:
                continue  # skip unclassified headers but they serve as boundaries

            # Find next header (classified or not) for section boundary
            section_end = yr_end
            for next_idx in range(idx + 1, len(all_cat_lines)):
                section_end = all_cat_lines[next_idx][0] - 1
                break

            entries = parse_catalogue_entries(lines, cat_line + 1, section_end)
            if entries:
                if cat_name not in catalogues[year]:
                    catalogues[year][cat_name] = entries
                else:
                    # Keep the longer version (avoid duplicate from dual CODE CATALOGUES markers)
                    if len(entries) > len(catalogues[year][cat_name]):
                        catalogues[year][cat_name] = entries

    # Fill in MISSING catalogues from the nearest year that has them
    # Only for catalogues where a year has 0 entries (not to replace valid smaller sets)
    # Priority: nearest year first, then largest set
    all_years = sorted(catalogues.keys())
    all_cat_names = set()
    for cats in catalogues.values():
        all_cat_names.update(cats.keys())

    for year in all_years:
        for cat_name in all_cat_names:
            existing = catalogues[year].get(cat_name, [])
            if existing:
                continue  # Only fill in completely missing catalogues

            # Find nearest year with this catalogue
            best_donor = None
            best_dist = 999
            for other_year in all_years:
                if other_year == year:
                    continue
                other = catalogues[other_year].get(cat_name, [])
                if not other:
                    continue
                dist = abs(other_year - year)
                if dist < best_dist:
                    best_donor = other_year
                    best_dist = dist
            if best_donor is not None:
                donor_entries = catalogues[best_donor][cat_name]
                catalogues[year][cat_name] = donor_entries
                print(f"  {year} {cat_name}: using {best_donor} catalogue ({len(donor_entries)} entries)")

    # Special case: 2016 OCR catalogues with <80% of 2018 → use 2018
    if 2016 in catalogues and 2018 in catalogues:
        for cat_name in list(catalogues[2016].keys()):
            entries_2016 = catalogues[2016][cat_name]
            entries_2018 = catalogues[2018].get(cat_name, [])
            if entries_2018 and len(entries_2016) < len(entries_2018) * 0.8:
                catalogues[2016][cat_name] = entries_2018
                print(f"  2016 {cat_name}: OCR quality fix, using 2018 ({len(entries_2018)} entries, 2016 had {len(entries_2016)})")

    # Report
    for year in sorted(catalogues):
        cats = catalogues[year]
        if cats:
            print(f"  Catalogues for {year}: {', '.join(f'{k}({len(v)})' for k, v in sorted(cats.items()))}")
        else:
            print(f"  Catalogues for {year}: (none found)")

    return catalogues


def parse_catalogue_entries(lines, start, end):
    """Parse code→description pairs from a catalogue section.
    Handles both 2018+ (code then description) and 2016 OCR (mixed order).
    """
    entries = []

    # Noise patterns to skip
    noise_pats = [
        re.compile(r'^\s*\[Page \d+\]'),
        re.compile(r'^\s*Código\s*$', re.IGNORECASE),
        re.compile(r'^\s*Descripci[oó]n', re.IGNORECASE),
        re.compile(r'^\(Continúa\)', re.IGNORECASE),
        re.compile(r'^\s*INEGI\.'),
        re.compile(r'^\s*Título:'),
        re.compile(r'^\s*FE DE ERRATAS'),
        re.compile(r'^\s*1 Las descripciones'),
        re.compile(r'^\s*2 Las claves'),
        re.compile(r'programas de an[aá]lisis'),
        re.compile(r'^\s*$'),
        re.compile(r'^─{5,}'),
        re.compile(r'^={5,}'),
        re.compile(r'^\s*CODE CATALOGUES'),
    ]

    # Bare page number (but not codes like 101, 201 — only 1-digit numbers
    # that appear as standalone page numbers)

    i = start
    while i <= end:
        line = lines[i].strip()
        i += 1

        # Skip noise
        if any(p.match(line) for p in noise_pats):
            continue

        # Stop if we hit a Variable definition (we've left the catalogue)
        if re.match(r'\s*Variable #\d+:', line):
            break

        # Skip ALL-CAPS category sub-headers
        if line.isupper() and len(line) > 2 and not is_code(line):
            continue

        # Skip single-digit bare page numbers (1-9)
        if re.match(r'^\d$', line):
            continue

        if is_code(line):
            code = line
            # Look ahead for description
            found_desc = False
            while i <= end:
                desc_line = lines[i].strip()
                i += 1
                if any(p.match(desc_line) for p in noise_pats):
                    continue
                if desc_line.isupper() and len(desc_line) > 2 and not is_code(desc_line):
                    continue
                if re.match(r'\s*Variable #\d+:', desc_line):
                    entries.append((code, ''))
                    i -= 1  # back up
                    found_desc = True
                    break
                if is_code(desc_line):
                    entries.append((code, ''))
                    code = desc_line
                    continue
                # This is the description
                entries.append((code, desc_line))
                found_desc = True
                break
            if not found_desc:
                entries.append((code, ''))
            continue

    return entries


def match_range_to_catalogue(range_text):
    """Given a Range field value, return the catalogue canonical name or None."""
    if not range_text:
        return None
    rt = range_text.lower()
    if 'catalogo' not in rt and 'catálogo' not in rt:
        return None
    # Skip SINCO/SCIAN — not our catalogues
    if 'sinco' in rt or 'scian' in rt:
        return None
    for keyword, cat_name in RANGE_TO_CATALOGUE:
        if keyword in rt:
            return cat_name
    return None


def inject_catalogues(blocks, catalogues_by_year):
    """For each variable with Range: Catálogo de..., inject value labels."""
    current_year = None
    injected_count = 0

    for i, (typ, block_lines) in enumerate(blocks):
        if typ == 'year_header':
            m = re.search(r'ENIGH (\d{4})', '\n'.join(block_lines))
            if m:
                current_year = int(m.group(1))
            continue

        if typ != 'variable' or current_year is None:
            continue

        # Check if this variable has a Range: Catálogo field
        range_val = None
        has_value_labels = False
        for line in block_lines:
            rm = re.match(r'\s+Range:\s+(.*)', line)
            if rm:
                range_val = rm.group(1).strip()
            if 'Value Labels:' in line:
                has_value_labels = True

        if not range_val:
            continue

        cat_name = match_range_to_catalogue(range_val)
        if not cat_name:
            continue

        # Get the catalogue for this year
        year_cats = catalogues_by_year.get(current_year, {})
        entries = year_cats.get(cat_name, [])
        if not entries:
            continue

        # Skip if variable already has value labels with actual entries
        if has_value_labels:
            # Check if there are actual label entries (not just the header)
            has_entries = False
            for line in block_lines:
                if re.match(r'\s+\S+\s*=\s*', line):
                    has_entries = True
                    break
            if has_entries:
                continue

        # Build value label lines
        label_lines = ['    Value Labels:']
        for code, desc in entries:
            label_lines.append(f'           {code} = {desc}')

        # Find insertion point: after the last field line, before any Nota:
        insert_idx = len(block_lines)
        for j in range(len(block_lines) - 1, 0, -1):
            line = block_lines[j].strip()
            if line:
                insert_idx = j + 1
                break

        # Insert
        new_lines = block_lines[:insert_idx] + label_lines + block_lines[insert_idx:]
        blocks[i] = (typ, new_lines)
        injected_count += 1

    print(f"Injected catalogue value labels into {injected_count} variables")
    return blocks


def fix_all_blocks(blocks):
    """Apply fixes to all variable blocks."""
    current_year = None
    fixed = []
    for typ, block_lines in blocks:
        if typ == 'year_header':
            m = re.search(r'ENIGH (\d{4})', '\n'.join(block_lines))
            if m:
                current_year = int(m.group(1))
            fixed.append((typ, block_lines))
        elif typ == 'variable':
            fixed_lines = fix_variable_block(block_lines, current_year)
            fixed.append((typ, fixed_lines))
        else:
            fixed.append((typ, block_lines))
    return fixed


def fix_variable_block(block_lines, year):
    """Fix a single variable record block."""
    text = '\n'.join(block_lines)

    # ── Fix P00 code letter-O-for-zero (all years, but mainly 2016) ──
    text = fix_p00_codes(text)

    # ── Fix "Fe de erratas" artifacts ──
    text = re.sub(r'\s*Fe de erratas\s*', '', text)

    # ── 2016-specific OCR fixes ──
    if year == 2016:
        text = fix_2016_ocr(text)

    return text.split('\n')


def fix_p00_codes(text):
    """Fix P-code references where OCR turned 0 into O."""
    # PO14 → P014, POO9 → P009, PO67 → P067, etc.
    # Pattern: P followed by O (letter) then digits, or P followed by digits+O+digits
    def fix_pcode(m):
        code = m.group(0)
        # Replace letter O with digit 0 within P-codes
        fixed = code[0]  # 'P'
        for c in code[1:]:
            if c in ('O', 'o') and not code.endswith('o'):
                fixed += '0'
            else:
                fixed += c
        return fixed

    # Fix patterns like PO14, POO9, PO67, PO73, PO75, etc.
    text = re.sub(r'\bP[O0o][O0o]?\d{1,3}\b', fix_pcode, text)
    # Fix patterns like Pos0 → P080
    text = re.sub(r'\bPos0\b', 'P080', text)
    # Fix P0150 PO016 → P015 o P016
    text = re.sub(r'\bP0150\b', 'P015 o', text)
    text = re.sub(r'\bP0740\b', 'P074 o', text)
    text = re.sub(r'\bP0210\b', 'P021 o', text)
    # Fix "P022" that should be "P022" (already correct in some cases)
    # Fix "0" used for "o" (Spanish "or") between P-codes
    text = re.sub(r'(\bP\d{3})\s+0\s+(P\d{3})', r'\1 o \2', text)
    return text


def fix_2016_ocr(text):
    """Fix OCR character substitution errors in 2016 text."""

    # ── Character substitutions ──
    # 6 → ó (very common OCR error in 2016)
    # But only in known Spanish words, not in numbers or codes
    ocr_word_fixes = {
        'Seccién': 'Sección',
        'seccién': 'sección',
        'Definicién': 'Definición',
        'definicién': 'definición',
        'Definici6n': 'Definición',
        'definici6n': 'definición',
        'Definicidn': 'Definición',
        'Definicion': 'Definición',
        'habitacién': 'habitación',
        'seleccién': 'selección',
        'adquisicién': 'adquisición',
        'adquisicion': 'adquisición',
        'adquisicidn': 'adquisición',
        'descripcién': 'descripción',
        'traccién': 'tracción',
        'recoleccién': 'recolección',
        'disposicién': 'disposición',
        'estimacién': 'estimación',
        'remuneracién': 'remuneración',
        'alimentacién': 'alimentación',
        'educacién': 'educación',
        'poblacién': 'población',
        'informacién': 'información',
        'Clasificacién': 'Clasificación',
        'clasificacién': 'clasificación',
        'clasificacion': 'clasificación',
        'situacién': 'situación',
        'Situacién': 'Situación',
        'ocupacién': 'ocupación',
        'comunicacién': 'comunicación',
        'operacién': 'operación',
        'construccién': 'construcción',
        'produccién': 'producción',
        'proteccién': 'protección',
        'contribucién': 'contribución',
        'jubilacién': 'jubilación',
        'pensién': 'pensión',
        'condicién': 'condición',
        'prestacién': 'prestación',
        'indemnizacién': 'indemnización',
        'recaudacién': 'recaudación',
        'percepcién': 'percepción',
        'organizacién': 'organización',
        'instalacién': 'instalación',
        'importacién': 'importación',
        'exportacién': 'exportación',
        'distribucién': 'distribución',
        'institucién': 'institución',
        'participacién': 'participación',
        'posesién': 'posesión',
        'reforestacion': 'reforestación',
        'Construccion': '',  # column-header artifact in 2016 CONCENTRADOHOGAR
        'afios': 'años',
        'afio': 'año',
        'Afio': 'Año',
        'nifio': 'niño',
        'nifios': 'niños',
        'nifias': 'niñas',
        'Nifios': 'Niños',
        'ensefianza': 'enseñanza',
        'bafios': 'baños',
        'bafio': 'baño',
        'desempefio': 'desempeño',
        'compafifa': 'compañía',
        'compafifa': 'compañía',
        'Espafiol': 'Español',
        'espafiol': 'español',
        'Espafol': 'Español',
        'espafol': 'español',
        'tamafio': 'tamaño',
        'cddigo': 'código',
        'cédigo': 'código',
        'codigo': 'código',
        'Cédigo': 'Código',
        'Codigo': 'Código',
        'algtin': 'algún',
        'algun': 'algún',
        'segun': 'según',
        'segtin': 'según',
        'vehiculos': 'vehículos',
        'vehiculo': 'vehículo',
        'arboles': 'árboles',
        'publico': 'público',
        'Publico': 'Público',
        'electrica': 'eléctrica',
        'Electrica': 'Eléctrica',
        'doméstica': 'doméstica',
        'estéreos': 'estéreos',
        'estéreo': 'estéreo',
        'antigiiedad': 'antigüedad',
        'Antigiiedad': 'Antigüedad',
        'Antigliedad': 'Antigüedad',
        'antigliedad': 'antigüedad',
        'indigena': 'indígena',
        'indigenas': 'indígenas',
        'dialecto': 'dialecto',
        'biológico': 'biológico',
        'económico': 'económico',
        'económica': 'económica',
    }

    for wrong, right in ocr_word_fixes.items():
        text = text.replace(wrong, right)

    # ── General regex-based OCR fixes ──
    # "cién" → "ción" (covers ALL Spanish -ción words not in dict above)
    text = re.sub(r'cién\b', 'ción', text)
    text = re.sub(r'ci6n\b', 'ción', text)
    text = re.sub(r'cidn\b', 'ción', text)
    # "sién" → "sión" (covers versión, pensión, etc.)
    text = re.sub(r'sién\b', 'sión', text)
    text = re.sub(r'si6n\b', 'sión', text)

    # Remove "Construcción" that was created by the regex above from "Construccién"
    # (column-header bleed artifact in 2016 CONCENTRADOHOGAR)
    text = text.replace(' Construcción ', ' ')
    # "fi" → "ñ" in remaining common patterns
    text = re.sub(r'\bduefio\b', 'dueño', text)
    text = re.sub(r'\bpequefio\b', 'pequeño', text)
    text = re.sub(r'\bpequefios\b', 'pequeños', text)
    text = re.sub(r'\bsefior\b', 'señor', text)
    text = re.sub(r'\bsefiora\b', 'señora', text)
    text = re.sub(r'\bcompafiia\b', 'compañía', text)
    text = re.sub(r'\blefio\b', 'leño', text)
    text = re.sub(r'\blefios\b', 'leños', text)
    text = re.sub(r'\blefia\b', 'leña', text)
    text = re.sub(r'\bcafia\b', 'caña', text)
    text = re.sub(r'\bmufiecas\b', 'muñecas', text)
    text = re.sub(r'\bpafiuelo\b', 'pañuelo', text)
    text = re.sub(r'\bpafiuelos\b', 'pañuelos', text)
    text = re.sub(r'\bpafiales\b', 'pañales', text)
    text = re.sub(r'\bpafial\b', 'pañal', text)
    text = re.sub(r'\brifidn\b', 'riñón', text)
    # "ii" → "ü" in remaining patterns
    text = re.sub(r'giiedad\b', 'güedad', text)
    text = re.sub(r'giiismo\b', 'güismo', text)

    # ── Clean garbled Definición column-header bleed ──
    # The word "Definición" (sometimes with trailing —, _, |) appears mid-text
    # when the right column header leaked into the left column content during OCR.
    # Strategy: process line by line, remove "Definición" + optional dashes/underscores
    # when it appears INSIDE a line (not as the field label "Definición:").
    cleaned = []
    for line in text.split('\n'):
        # Don't touch lines that ARE the Definición field label
        if re.match(r'^\s+Definición:', line):
            # But clean "Definición" appearing AFTER the colon value
            # e.g., "    Definición: some text Definición —_ more text"
            field_match = re.match(r'^(\s+Definición:\s+)(.*)', line)
            if field_match:
                prefix = field_match.group(1)
                value = field_match.group(2)
                # Remove "Definición" + optional dashes/underscores from the value
                value = re.sub(r'\s*Definición\s*[-—_|]*\s*', ' ', value)
                value = value.strip()
                cleaned.append(prefix + value)
            else:
                cleaned.append(line)
        else:
            # For non-Definición lines, remove the leaked header
            line = re.sub(r'\s+Definición\s*[-—_|]+\s*', ' ', line)
            cleaned.append(line)
    text = '\n'.join(cleaned)

    # ── Remove column-header bleed ──
    # These are right-column headers that leaked into left-column text
    # "Numero de pregunta" appearing in the middle of definitions
    text = re.sub(r'\s+Numero de pregunta\s+', ' ', text)
    text = re.sub(r'\s+Pregunta textual\s*', ' ', text)
    text = re.sub(r'\s+Cuestionario Seccion y/o apartado\s*', ' ', text)
    text = re.sub(r'\s+Cuestionario Sección y/o apartado\s*', ' ', text)

    # ── Remove right-column bleed at end of Definición lines ──
    # e.g., "...tarjeta de crédito. Pregunta: Gastos del hogar, Gastos diarios"
    # These are section headers from the right column that leaked into left text
    cleaned2 = []
    for line in text.split('\n'):
        if re.match(r'^\s+Definición:', line):
            # Strip trailing right-column text: "Pregunta: <section name>"
            line = re.sub(
                r'\s+Pregunta:\s+(?:Gastos del hogar|Hogares y vivienda|Condiciones sociodemográficas|Condición de actividad|Ingresos|Trabajos|Erogaciones)',
                '', line)
        cleaned2.append(line)
    text = '\n'.join(cleaned2)

    # ── Fix "Cc" → "C" (OCR doubled the C in Type field) ──
    text = re.sub(r'(Type:\s+)Cc\b', r'\1C', text)

    # ── Fix truncated question marks ──
    text = text.replace('&é(', '¿(')
    text = text.replace('é(', '¿(')
    text = text.replace('éC', '¿C')
    text = text.replace('éD', '¿D')
    text = text.replace('éE', '¿E')
    text = text.replace('éQ', '¿Q')
    text = text.replace('éL', '¿L')
    text = text.replace('éA', '¿A')
    text = text.replace('éP', '¿P')
    text = text.replace('éN', '¿N')
    text = text.replace('éU', '¿U')
    text = text.replace('éS', '¿S')
    text = text.replace('éR', '¿R')

    # ── Fix "NOOR WN" OCR artifact in value labels ──
    # This is OCR misreading individual value code lines 2-7
    # Don't replace — the individual labels were lost in OCR

    # ── Clean up doubled spaces (only mid-line, preserve indentation) ──
    cleaned_lines = []
    for line in text.split('\n'):
        stripped = line.lstrip()
        indent = line[:len(line) - len(stripped)]
        cleaned_lines.append(indent + re.sub(r'  +', ' ', stripped))
    text = '\n'.join(cleaned_lines)

    # ── Fix "Sección |" → "Sección I" (OCR pipe for Roman I) ──
    text = re.sub(r'Sección\s+\|', 'Sección I', text)

    # ── Fix "Seccion |" → "Sección I" ──
    text = re.sub(r'Seccion\s+\|', 'Sección I', text)

    # ── Fix "Sección Ill" → "Sección III" (OCR lowercase L for I) ──
    text = text.replace('Sección Ill', 'Sección III')
    text = text.replace('Sección Il', 'Sección II')

    # ── Fix Sección that are just "Sección" without value → remove field ──
    # (happens when OCR couldn't read the section)

    # ── Fix «4 → ¿ (OCR artifact for question mark) ──
    text = re.sub(r'«\d*\s*', '¿', text)

    # ── Fix "0" used as "o" (Spanish "or") in common phrases ──
    text = re.sub(r'\b0 mas\b', 'o más', text)
    text = re.sub(r'\b0 menos\b', 'o menos', text)
    text = re.sub(r'\b0 dejo\b', 'o dejó', text)
    text = re.sub(r'\b0 recursos\b', 'o recursos', text)
    text = re.sub(r'\b0 dialecto\b', 'o dialecto', text)

    # ── Fix "construy6" → "construyó" etc. (6→ó in verbs) ──
    text = text.replace('construy6', 'construyó')
    text = text.replace('adquiri6', 'adquirió')
    text = text.replace('Pag6', 'Pagó')

    # Fix remaining artifacts: ". ." and ". . rn" (OCR noise between columns)
    text = re.sub(r'\s*ou,\s*\|\s*\|\s*', ' ', text)
    text = re.sub(r'\s*eee\s*', ' ', text)
    text = re.sub(r'\s*\. \. \.\s*', ' ', text)
    text = re.sub(r'\s*\. \.\s*', ' ', text)  # two-dot variant
    text = re.sub(r'\s*:\s*:\s*', ' ', text)  # ": :" artifact
    text = re.sub(r'\s*;\s*\|\s*\|?\s*', '; ', text)  # "; |" or "; | |" artifacts
    text = re.sub(r'\s*\.\s*\|\s*', '. ', text)  # ". |" artifacts
    # Remove stray pipes: "| text" at start of field values
    text = re.sub(r'(:\s+)\|\s+', r'\1', text)
    # Remove isolated pipes mid-text (surrounded by spaces or at word boundaries)
    text = re.sub(r'\s+\|\s+', ' ', text)
    text = re.sub(r'(\w)\|\s', r'\1 ', text)  # "e| hogar" → "e hogar"
    text = re.sub(r'\s\|\s*\.', ' ', text)  # " | ." artifacts

    return text


def write_output(blocks):
    """Write the cleaned file."""
    out_lines = []

    # Write header
    out_lines.append('#' * 100)
    out_lines.append('')
    out_lines.append('  ENIGH VARIABLE DOCUMENTATION — CLEAN DATA DICTIONARY')
    out_lines.append('  ====================================================')
    out_lines.append('  Encuesta Nacional de Ingresos y Gastos de los Hogares')
    out_lines.append('  (National Household Income and Expenditure Survey — Mexico, INEGI)')
    out_lines.append('  Years: 2016, 2018, 2020, 2022, 2024')
    out_lines.append('')
    out_lines.append('  Contains ONLY structured variable records (data dictionary).')
    out_lines.append('  All intro, TOC, variable lists, catalogues, and errata removed.')
    out_lines.append('')
    out_lines.append('  For each variable:')
    out_lines.append('    - Variable name (as in .dta files)')
    out_lines.append('    - Label (Spanish description)')
    out_lines.append('    - Type: C(n) = character width n, N(n) = numeric width n')
    out_lines.append('    - Range of valid values')
    out_lines.append('    - Definición (definition)')
    out_lines.append('    - Pregunta textual (survey question)')
    out_lines.append('    - Value labels: code = description')
    out_lines.append('')
    out_lines.append('  Conventions:')
    out_lines.append('    1 = Sí, 2 = No (yes/no variables)')
    out_lines.append('    & = no especificado (not specified)')
    out_lines.append('    folioviv = housing unit ID, foliohog = household ID')
    out_lines.append('    numren = person ID, factor = expansion weight')
    out_lines.append('    entidad = state code (01-32)')
    out_lines.append('')
    out_lines.append('  Note: 2016 was OCR-extracted; minor character artifacts may remain.')
    out_lines.append('')
    out_lines.append('#' * 100)
    out_lines.append('')

    var_count = 0
    for typ, block_lines in blocks:
        if typ == 'header':
            continue  # replaced by our header above
        elif typ == 'year_header':
            out_lines.append('')
            for line in block_lines:
                out_lines.append(line)
            out_lines.append('')
        elif typ == 'table_header':
            out_lines.append('')
            for line in block_lines:
                out_lines.append(line)
            out_lines.append('')
        elif typ == 'variable':
            for line in block_lines:
                out_lines.append(line)
            # Ensure blank line after variable
            if block_lines and block_lines[-1].strip():
                out_lines.append('')
            var_count += 1

    # Footer
    out_lines.append('')
    out_lines.append('#' * 100)
    out_lines.append(f'  Total: {var_count} variable records')
    out_lines.append('  Source: ENIGH 2016–2024 official INEGI documentation PDFs')
    out_lines.append('#' * 100)

    # Remove excessive blank lines (max 2 consecutive)
    final = []
    blank_count = 0
    for line in out_lines:
        if line.strip() == '':
            blank_count += 1
            if blank_count <= 2:
                final.append(line)
        else:
            blank_count = 0
            final.append(line)

    with open(OUTPUT, 'w', encoding='utf-8') as f:
        f.write('\n'.join(final))

    sz = os.path.getsize(OUTPUT)
    print(f"\nDone → {OUTPUT}")
    print(f"  {sz / 1024:.0f} KB, {len(final)} lines, {var_count} variables")


if __name__ == '__main__':
    main()
