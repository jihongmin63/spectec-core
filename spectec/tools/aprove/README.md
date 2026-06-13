# AProVE bridge (termination of unconditional systems)

`verify` routes termination of an **unconditional** (plain TRS) slice to
[AProVE](https://aprove.informatik.rwth-aachen.de/) instead of MuTerm; a slice
that still carries conditions stays on MuTerm. The bridge ([../../lib/rewrite/aprove.ml](../../lib/rewrite/aprove.ml))
runs a local AProVE jar — there is no Python client here, because the jar prints
its verdict directly:

```
java -ea -jar aprove.jar -m wst -t <timeout> <file.trs>
```

The first token of the output is the verdict (`YES` / `NO` / `MAYBE`).

## Setup

1. Install a Java runtime (`java` must be on `PATH`).
2. Download `aprove.jar` from <https://aprove.informatik.rwth-aachen.de/> (the
   jar is tens of MB and is **not** checked into this repo).
3. Point the bridge at it, by either:
   - placing it at `spectec/tools/aprove/aprove.jar` (the default lookup path), or
   - setting `SPECTEC_APROVE_JAR=/path/to/aprove.jar`, or
   - passing `--aprove-jar /path/to/aprove.jar` to `verify`.

If no jar is found the AProVE rows report
`termination(aprove): ERROR: aprove jar not found …` rather than crashing.

## Usage

```bash
spectec/_build/default/bin/main.exe verify --only termination spec.spectec
```

Each row is labelled with the tool that decided it, e.g.
`termination(aprove): YES` or `termination(muterm): YES`.
