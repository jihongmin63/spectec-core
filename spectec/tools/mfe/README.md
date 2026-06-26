# Maude Formal Environment bridge (confluence + coherence)

The confluence/coherence gate routes a rewriting-system slice to the **Maude
Formal Environment (MFE)** — Full Maude plus the **Church-Rosser Checker (CRC)**
and **Coherence Checker (ChC)** — instead of CoCoWeb. The bridge
([../../lib/rewrite/mfe.ml](../../lib/rewrite/mfe.ml)) renders the system as a
single-sort Full Maude *system* module
([`Rewrite_system.string_of_system_maude`](../../lib/rewrite/rewrite_system.ml))
and runs both checks in one local `maude` invocation — no Python, no network:

```
load <mfe>/full-maude.maude
(mod SPEC is ... endm)
(check Church-Rosser SPEC .)
(check coherence SPEC .)
```

- **CRC** decides whether the *equational* fragment (functions, prelude,
  input-moded relations -- the `eq`/`ceq`) is Church-Rosser, i.e. whether
  `reduce` is well-defined.
- **ChC** decides whether the *rules* (`rl`/`crl` -- the non-input-moded,
  genuinely non-deterministic relations) are coherent with the equations, i.e.
  whether search is complete modulo them.

See [../../lib/rewrite/CORE_LOGIC.md](../../lib/rewrite/CORE_LOGIC.md) §6.5.

## Setup

The MFE is not checked into this repo (it carries its own license and is sizable).

1. Install Maude (the bridge reuses the local binary at `tools/maude/maude`, or
   `SPECTEC_MAUDE_BIN`, or `maude` on `PATH`).
2. Download the Maude Formal Environment from
   <https://maude.cs.illinois.edu/> (the MFE / Full Maude distribution).
3. Place its files in a directory whose **entry file is `full-maude.maude`** and
   which loads the CRC and ChC tools, then point the bridge at it by either:
   - placing it at `spectec/tools/mfe/` (the default lookup path), or
   - setting `SPECTEC_MFE_DIR=/path/to/mfe`, or
   - passing `--mfe-dir /path/to/mfe` (once the `verify` CLI is reintroduced).

If no MFE is found the bridge reports a clean `ERROR: MFE not found …` rather than
crashing.

## Calibration (first run)

Because the MFE is not bundled, three things in
[mfe.ml](../../lib/rewrite/mfe.ml) are best-effort and must be confirmed against a
real MFE run; they are isolated as named constants/parsers so calibration is local:

- **`mfe_entry`** — the load file name (`full-maude.maude` assumed). Adjust if your
  MFE bundles a different entry that loads CRC + ChC.
- **`crc_command` / `chc_command`** — the `(check Church-Rosser …)` /
  `(check coherence …)` command syntax.
- **`crc_verdict` / `chc_verdict`** — the success/failure phrasings (`is
  Church-Rosser`, `is coherent`, …) that map output to `YES`/`MAYBE`.

To calibrate, dump a small system to a `.maude` file, run the load + checks by
hand, and adjust the constants/parsers to the observed tokens.
