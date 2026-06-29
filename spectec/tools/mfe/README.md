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

## Installed in this environment (observed)

- **Maude**: stock Maude 3.5.1 (`maude-lang/Maude` release `Maude-3.5.1-linux-x86_64.zip`)
  at `spectec/tools/maude/maude`.
- **MFE**: `maude-team/MFE` (`master`) under `spectec/tools/mfe/` — entry
  **`src/mfe.maude`** (NOT `full-maude.maude`), which relatively `load`s
  `FM/full-maude351.maude`, `CRChC/crchc3u.maude`, `MTT/mtt.maude`, `SCC/scc.maude`.
  It loads cleanly under **stock** Maude 3.5.1 (banner: "The Maude Formal
  Environment 3.0 / Church-Rosser Checker 3t / Coherence Checker 3t / Sufficient
  Completeness Checker 2b"); Maude++ is NOT required just to load + run CRC/ChC.
- Both are gitignored (own licenses, sizable) — see the repo `.gitignore`.

## MTT (Maude Termination Tool) — bundled, but not runnable here

MTT is **already bundled** in the MFE (`src/MTT/`: `mtt.maude`,
`MTT-transformations.1.5l.maude`, `termcheck.maude`); there is no separate
download (the old `lcc.uma.es/~duran/MTT` page no longer serves a distribution).
It is **not runnable in this setup**, because it needs both:
1. **Maude++** — the `writeToFile`/`termCheck` reflective hooks MTT calls
   (`ceta-hook`/`mtt-hook` in `src/mfe.maude`); stock Maude 3.5.1 lacks them, and
2. an **external WST-format termination prover** named in an `mfe.config`
   (absent), e.g. AProVE / mu-term.

Consequence for the gate: the **CRC reports local confluence + sort-decreasingness**
(`All critical pairs have been joined.` / `The specification is locally-confluent.`),
not full Church-Rosser — full Church-Rosser additionally needs the termination
proof MTT would discharge. For our confluence gate, local confluence is the
practical positive verdict (the CTRS is assumed terminating).

## Calibration (observed against the real MFE)

The earlier best-effort constants in [mfe.ml](../../lib/rewrite/mfe.ml) were
confirmed/corrected against a real run. Observed truth:

- **Entry**: `src/mfe.maude` (was `full-maude.maude`).
- **The MFE is a Full Maude LOOP that reads commands from STDIN.** You cannot
  `load mfe.maude` and then append the module + check commands to the same file —
  the loop blocks on stdin and hangs. Feed `set include BOOL off .`, the
  `(mod SPEC … endm)` (Full Maude, parenthesized — what `string_of_system_maude`
  emits), the tool commands, and `quit` via **stdin** to
  `maude -no-banner tools/mfe/src/mfe.maude`.
- **`set include BOOL off .`** is required first — the tools cannot handle Maude's
  built-in `BOOL`; our structural `SPEC` defines its own `true`/`false`, so this is
  safe.
- **Commands**: select the tool, THEN check (a bare `(check … .)` with no tool
  selected is a parse error):
  - CRC: `(select tool CRC .)` then `(ccr SPEC .)` (alias `(check Church-Rosser SPEC .)`)
  - ChC: `(select tool ChC .)` then `(cch SPEC .)` (alias `(check coherence SPEC .)`)
- **Verdict tokens** (substring match):
  - CRC confluent -> `The specification is locally-confluent.` / `All critical pairs have been joined.`
  - CRC pending   -> `The following critical pairs must be proved joinable:`
  - ChC coherent  -> `All critical pairs have been rewritten and no rewrite with rules can happen at non-overlapping positions`
  - ChC pending   -> pending critical pairs are listed (as for CRC).
