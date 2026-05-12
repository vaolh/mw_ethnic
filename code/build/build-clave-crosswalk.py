#!/usr/bin/env python3
"""
REPLICATION FILE: code/build/build-clave-crosswalk.py
AUTHORS: Matías Carrasco, Victor Ortega Le Hénanff
DATE:    2026-05-10

Builds the unified clave→subgroup crosswalk used by both the pre-2024
numeric ENIGH coding (e.g., 011131 = Tortillas de maíz) and the 2024
alpha coding (e.g., A001 = Maíz en grano). The two coding schemes are
harmonized into a common ~60-subgroup partition so that capítulo-level
descriptives and DiD/event-study estimates can pool data across waves
2016-2024 without the wave-specific coding gap that previously sent every
2024 row into gasto_group = "other".

Inputs
------
  data/clean/enigh/clave_gasto_labels.csv
      Label dictionary, one row per (clave, label). Extracted from INEGI
      ENIGH PDF documentation 2016-2024 by extract_clave_labels.py.

  data/clean/enigh/clave_subgroups_seed.csv
      Hand-authored mapping rules, one row per [clave_start, clave_end]
      range with target (capitulo_norm, subgroup, subgroup_label).
      First-match-wins ordering — narrower rules must precede broader
      fallbacks. Both alpha and numeric ranges are supported; comparison
      is purely lexicographic since alpha codes sort after numeric ones
      under ASCII.

Output
------
  data/clean/enigh/clave_crosswalk.csv
      One row per clave with columns:
          clave, label, capitulo_norm, subgroup, subgroup_label

Coverage gate
-------------
Exits with status 2 if any clave in the input dictionary is unmatched by
the seed. The unmatched list is printed so the seed can be extended.
"""

from __future__ import annotations

import csv
import sys
from collections import Counter
from pathlib import Path

# Resolve paths relative to this script's location.
HERE = Path(__file__).resolve().parent          # code/build/
ROOT = HERE.parent.parent                       # project root
LABELS_PATH = ROOT / "data" / "clean" / "enigh" / "clave_gasto_labels.csv"
SEED_PATH = ROOT / "data" / "clean" / "enigh" / "clave_subgroups_seed.csv"
OUT_PATH = ROOT / "data" / "clean" / "enigh" / "clave_crosswalk.csv"


def load_labels(path: Path) -> list[tuple[str, str]]:
    """Return list of (clave, label) tuples preserving file order."""
    with path.open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        return [(row["clave"], row["label"]) for row in reader]


def load_seed(path: Path) -> list[dict[str, str]]:
    """Return list of rule dicts in file order (first-match-wins)."""
    with path.open(newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        rules = []
        for row in reader:
            for k in ("clave_start", "clave_end", "capitulo_norm",
                      "subgroup", "subgroup_label"):
                if not row.get(k):
                    sys.exit(
                        f"seed row missing field {k!r}: {row}"
                    )
            rules.append({
                "start": row["clave_start"].strip(),
                "end": row["clave_end"].strip(),
                "capitulo": row["capitulo_norm"].strip(),
                "subgroup": row["subgroup"].strip(),
                "label": row["subgroup_label"].strip(),
            })
        return rules


def classify(clave: str, rules: list[dict[str, str]]) -> dict | None:
    """First rule whose [start, end] (lexicographic) contains `clave`."""
    for rule in rules:
        if rule["start"] <= clave <= rule["end"]:
            return rule
    return None


def main() -> int:
    labels = load_labels(LABELS_PATH)
    rules = load_seed(SEED_PATH)

    matched: list[dict[str, str]] = []
    unmatched: list[tuple[str, str]] = []
    for clave, label in labels:
        rule = classify(clave, rules)
        if rule is None:
            unmatched.append((clave, label))
            continue
        matched.append({
            "clave": clave,
            "label": label,
            "capitulo_norm": rule["capitulo"],
            "subgroup": rule["subgroup"],
            "subgroup_label": rule["label"],
        })

    # Coverage report.
    subgroup_counts = Counter(row["subgroup"] for row in matched)
    capitulo_counts = Counter(row["capitulo_norm"] for row in matched)

    print(f"Labels read:       {len(labels):>6}")
    print(f"Matched:           {len(matched):>6}")
    print(f"Unmatched:         {len(unmatched):>6}")
    print()
    print(f"Subgroups used:    {len(subgroup_counts):>6}")
    print(f"Capitulos used:    {len(capitulo_counts):>6}")
    print()
    print("Subgroup distribution (count, subgroup):")
    for sub, n in sorted(subgroup_counts.items(),
                         key=lambda kv: (-kv[1], kv[0])):
        marker = "  " if n >= 5 else " *"
        print(f"  {marker} {n:>5}  {sub}")
    print("  (* = subgroup has < 5 claves; consider merging)")
    print()
    print("Capitulo distribution:")
    for cap, n in sorted(capitulo_counts.items()):
        print(f"    {cap}: {n}")

    if unmatched:
        print()
        print(f"UNMATCHED claves ({len(unmatched)}):")
        for clave, label in unmatched[:30]:
            print(f"  {clave:>8}  {label[:80]}")
        if len(unmatched) > 30:
            print(f"  ... and {len(unmatched) - 30} more")
        return 2

    # Write output crosswalk.
    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    with OUT_PATH.open("w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(
            fh, fieldnames=["clave", "label", "capitulo_norm",
                            "subgroup", "subgroup_label"],
        )
        writer.writeheader()
        for row in matched:
            writer.writerow(row)
    print()
    print(f"WROTE {OUT_PATH.relative_to(ROOT)}  ({len(matched)} rows)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
