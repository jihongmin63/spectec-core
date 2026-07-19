# Maude Formal Environment bridge (confluence + coherence)

The confluence/coherence gate routes a rewriting-system slice to the **Maude
Formal Environment (MFE)** — Full Maude plus the **Church-Rosser Checker (CRC)**
and **Coherence Checker (ChC)** — instead of CoCoWeb. The bridge
([../../lib/rewrite/mfe.ml](../../lib/rewrite/mfe.ml)) renders the system as a
single-sort Full Maude *system* module
([`Rewrite_system.string_of_system_maude`](../../lib/rewrite/rewrite_system.ml))
and runs both checks in one local `maude` invocation — no Python, no network.
The MFE is an interactive Full Maude **loop reading stdin**, a tool must be
**selected before each check**, and the loop has no clean `quit` (it floods an
incomplete-input prompt at EOF), so the bridge pipes everything to stdin and
kills the process once both checks have printed:

```
load <mfe>/src/mfe.maude          # piped on stdin, NOT `maude FILE`
set include BOOL off .            # (already the first line of the module text)
(mod SPEC is ... endm)
(select tool CRC .)
(check Church-Rosser SPEC .)
(select tool ChC .)
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
3. Place its files in a directory whose **entry file is `src/mfe.maude`** and
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

## MTT (Maude Termination Tool) — runnable via the Maude 2.7.1 hook stack

The MTT bundled in *this* (3.5.1) MFE does not run: its `termCheck`/`writeToFile`
hooks need **Maude++**, and its `MTT-transformations.1.5l.maude` is Maude-2.x
syntax that stock Maude 3.5.1 rejects (unpatchable `TPDB-SIGN` errors). BUT the
whole termination path **does run** on the matching Maude-2.7.1-era stack, which
is obtainable and wired here (all gitignored, under `spectec/tools/`):

1. **`tools/maude271-hooks/maude`** — Maude 2.7.1 *with external hooks* (the
   `v2.7.1-ext-hooks` release of `maude-team/maude`, asset
   `maude-2.7.1-hooks-linux.tar.gz`). Binds the `TerminationCheckerSymbol`
   special ops (`termCheck`/`writeToFile`) that stock Maude leaves inert. Its
   dir is exported as `MAUDE_LIB` (and holds `mfe.config`).
2. **`tools/mfe271/MFE-mfe-2.7.1/`** — the *old* MFE (`maude-team/MFE` tag
   `mfe-2.7.1`). Bundles old Full Maude (`src/FM/full-maude27*.maude`, giving
   `FULL-MAUDE-SIGN`, which the MTT transformations parse against) + **MTT 1.5j**.
   Boots under the hook binary; banner lists `Maude Termination Tool 1.5j`.
3. **`tools/aprove/aprove.jar`** — WST backend (`aprove-developers/aprove-releases`);
   `tools/aprove/runme` runs `java -jar aprove.jar -u cli -t $2 -p plain -m wst $1`
   and `tools/maude271-hooks/mfe.config` = `aprove <abs>/tools/aprove/runme .trs`.
4. **`tools/z3/z3`** — the SMT backend AProVE actually uses (its default
   `SmtSolver = "z3"`; official download page lists Z3 ≥ 4.4.0, no license gate:
   `Z3Prover/z3` release asset `z3-*-x64-glibc-*.zip`). `runme` puts it on PATH.
   The WST strategy *also* has legacy `Engine = YICES` processors, but with no
   `yices` on PATH those **abort gracefully** (`Aborted Exec, QTRSRRR with some
   error`) and AProVE proceeds with Z3-based dependency-pair proofs — so
   **yices 1.x is NOT required** (AProVE's own docs call it "an outdated
   dependency [we] will update in the future"). yices/minisat2 would only be a
   faster path for arithmetic-heavy slices (SAT falls back to internal SAT4J).

Run one slice with `tools/mfe/run-termination.sh <symbol>`: it dumps the
analysis slice (`rewrite --ctrs --symbol`), rewrites the header to an old
Full-Maude functional module (`set include BOOL[-OPS] off .` as commands,
`mod`→`fmod`), then feeds `(select tool MTT .) (select external tool aprove .)`
`(select path C;A .)` for conditional slices `(ct SPEC .)`. MTT transforms the
(order-sorted, conditional) module to a TPDB CTRS — adding `isTerm`/`isThruth`
sort guards — and `termCheck` shells out to AProVE per the `mfe.config` (its
hardcoded per-call timeout was bumped 30→120 s in `mtt.maude`). Verified
end-to-end via Z3: `FOO`, `$empty_map`, `$is_lpm_key_prime` → termination `YES`;
arithmetic-heavy slices (`$un_op` — its transform drags in the whole int prelude)
are slower and may return MAYBE/TIMEOUT without the yices KBO fast-path.

So the gate can pair **CRC local confluence** with an **MTT/AProVE(Z3) termination
proof** for full Church-Rosser. Standalone, the CRC still stands alone as local
confluence + sort-decreasingness (the CTRS assumed terminating).

## Calibration (observed against the real MFE)

The earlier best-effort constants in [mfe.ml](../../lib/rewrite/mfe.ml) were
**wrong** (they assumed a `maude FILE` invocation, an `is Church-Rosser` token,
no tool selection); the bridge now encodes the real protocol below, verified
against MFE-master + Maude 3.5.1.

- **Entry**: `src/mfe.maude` (was `full-maude.maude`).
- **`MAUDE_LIB`**: `mfe.maude` does `sload file`/`process`/`time`, which resolve
  from the Maude library directory. The bridge exports `MAUDE_LIB` = the Maude
  binary's own directory when it holds `prelude.maude` (the bundled
  `tools/maude`), so the load succeeds regardless of the working directory.
- **The MFE is a Full Maude LOOP that reads commands from STDIN.** You cannot put
  the module + checks in a file and run `maude FILE` — after `load mfe.maude` the
  loop reads the terminal/stdin, so the file's trailing lines are never seen.
  Pipe to stdin instead: `load <abs>/src/mfe.maude`, then the module text (which
  already begins with `set include BOOL off .`), then the tool commands. The
  bridge feeds stdin from a temp file (so a module larger than the OS pipe buffer
  cannot deadlock against the unread stdout) and reads the merged stdout+stderr.
- **`set include BOOL off .`** must come first — the tools cannot handle Maude's
  built-in `BOOL`; our structural `SPEC` defines its own `true`/`false`, so this
  is safe. (`string_of_system_maude` emits it as the module's first line.)
- **Commands**: select the tool, THEN check (a bare `(check … .)` with no tool
  selected is a parse error):
  - CRC: `(select tool CRC .)` then `(check Church-Rosser SPEC .)`
  - ChC: `(select tool ChC .)` then `(check coherence SPEC .)`
- **No clean quit / EOF flood.** At end of input the loop floods the
  incomplete-input prompt `> ` forever (no `quit` is honored). The bridge reads
  under the `--timeout` deadline and **kills the process** once it sees the
  coherence-check header followed by a long run of that prompt (the run is done),
  parsing whatever verdicts were printed. So a normal run exits via SIGKILL, not
  status 0 — a verdict already in the output is still authoritative. Only a
  deadline reached with no verdict yields `Timeout`.
- **Verdict tokens** (substring match; the MFE wraps result lines at the terminal
  width, so the bridge collapses whitespace before matching):
  - CRC confluent -> `The specification is locally-confluent.` (with
    `All critical pairs have been joined.` / `The module is sort-decreasing.`)
  - CRC pending   -> `The following critical pairs must be proved joinable:`
  - ChC coherent  -> `… no rewrite with rules can happen at non-overlapping
    positions of equations left-hand sides.`
  - ChC pending   -> the coherence header printed but not the coherent token
    (proof obligations remain) -> `Maybe`.

## Performance — use per-symbol slices

One CRC call is dominated by critical-pair generation, which **explodes on the
whole system** (every `match_*`/`subty_*` equation pairs against the others). On
`impty/base` the whole-system CRC does not finish in tens of seconds, but a
single definition's dependency slice is fast:

```
# from repo root; per-symbol slice is the practical path
spectec/_build/default/bin/main.exe verify --list-symbols \
  spectec/specs/impty/base/spec.spectec
spectec/_build/default/bin/main.exe verify --symbol '$lookup' \
  spectec/specs/impty/base/spec.spectec
#   church-rosser: YES  coherence: YES        (~1.4s)
```

`verify --symbol NAME` slices to `NAME`'s downward dependency closure
(`Rewrite_system.slice`); the explosive roots (e.g. `Run_prog`, whose slice is
the whole reachable system) report `TIMEOUT` rather than a false `Error`.
