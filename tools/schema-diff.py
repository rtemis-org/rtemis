#!/usr/bin/env python3
# schema-diff.py
# ::rtemis::
# 2026- EDG rtemis.org

"""Compare complete generated artifact trees and preserve their byte digests."""

import argparse
import hashlib
import json
from pathlib import Path


def inventory(root):
    """Return a deterministic manifest of every file under root."""
    return {
        path.relative_to(root).as_posix(): {
            "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
            "bytes": path.stat().st_size,
        }
        for path in sorted(root.rglob("*"))
        if path.is_file()
    }


def main():
    """Write both inventories and the exact added, removed, and changed paths."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("before", type=Path)
    parser.add_argument("after", type=Path)
    parser.add_argument("report", type=Path)
    args = parser.parse_args()
    if not args.before.is_dir() or not args.after.is_dir():
        parser.error("both artifact directories must exist")
    before = inventory(args.before)
    after = inventory(args.after)
    report = {
        "before": before,
        "after": after,
        "added": sorted(after.keys() - before.keys()),
        "removed": sorted(before.keys() - after.keys()),
        "changed": sorted(
            name for name in before.keys() & after.keys() if before[name] != after[name]
        ),
    }
    args.report.parent.mkdir(parents=True, exist_ok=True)
    args.report.write_text(json.dumps(report, indent=2) + "\n")
    print(f"before={len(before)} after={len(after)}")
    for key in ("added", "removed", "changed"):
        print(f"{key}={len(report[key])}")
        for path in report[key]:
            print(f"  {path}")


if __name__ == "__main__":
    main()
