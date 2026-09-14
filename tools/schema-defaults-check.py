#!/usr/bin/env python3
# schema-defaults-check.py
# ::rtemis::
# 2026- EDG rtemis.org

"""Check defaults determinism in mixed output directories and missing records."""

import argparse
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import tempfile


def generate(root, *, succeeds=True):
    result = subprocess.run(
        ["just", "schema-defaults", str(root)], capture_output=True, text=True
    )
    print(result.stdout, end="")
    print(result.stderr, end="")
    if (result.returncode == 0) != succeeds:
        raise RuntimeError(f"Unexpected generator exit status: {result.returncode}")
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("artifacts", type=Path)
    parser.add_argument("report", type=Path)
    args = parser.parse_args()
    report = {}
    with tempfile.TemporaryDirectory(prefix="rtemis-defaults-check-") as tmp:
        root = Path(tmp)
        owned = []
        for source in sorted(args.artifacts.rglob("*.json")):
            if source.name not in ("schema.json", "record.json"):
                continue
            document = json.loads(source.read_text())
            publication = document.get("x-rtemis", {}).get("publication", {})
            if publication.get("producer") != "rtemis":
                continue
            relative = source.relative_to(args.artifacts)
            target = root / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(source, target)
            owned.append(relative.as_posix())
        if not owned:
            raise RuntimeError("No rtemis schemas in the supplied corpus")
        generate(root)
        defaults = root / "defaults/v1/defaults.json"
        clean = defaults.read_bytes()
        manifest = json.loads(clean)["schemas"]
        if sorted(manifest) != sorted(owned):
            raise RuntimeError("Defaults manifest differs from the producer-owned corpus")
        for relative, digest in manifest.items():
            if hashlib.sha256((root / relative).read_bytes()).hexdigest() != digest:
                raise RuntimeError(f"Wrong manifest digest: {relative}")
        report["manifest_entries"] = len(manifest)
        for relative in (
            "chart/review/v1/schema.json",
            "artifact/review/v1/record.json",
            "obsolete/v0/schema.json",
        ):
            target = root / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text("Unrelated files must not be parsed or hashed.\n")
        generate(root)
        if defaults.read_bytes() != clean:
            raise RuntimeError("Unrelated files changed defaults bytes")
        report["mixed_directory_byte_identical"] = True
        record = next(path for path in owned if path.endswith("/record.json"))
        (root / record).unlink()
        failure = generate(root, succeeds=False)
        if record not in failure.stderr or "generate the missing files" not in failure.stderr:
            raise RuntimeError("Missing record did not produce a corrective diagnostic")
        if defaults.read_bytes() != clean:
            raise RuntimeError("Failed generation overwrote the defaults artifact")
        report["missing_record_rejected"] = record
        report["failed_generation_preserved_artifact"] = True
        report["defaults_sha256"] = hashlib.sha256(clean).hexdigest()
    args.report.parent.mkdir(parents=True, exist_ok=True)
    args.report.write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
