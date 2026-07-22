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
   - passing `--mfe-dir /path/to/mfe` to `confluence`.

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

## Termination — `main.exe termination` (structure-preserving unravel + AProVE)

`main.exe termination --symbol <sym> FILES…` proves per-slice termination
in-binary: `lib/rewrite/unravel.ml` unravels the analysis slice
structure-preservingly into a plain TPDB TRS (`--emit-trs` shows it) and
`lib/rewrite/aprove.ml` hands it straight to AProVE. Only two gitignored
pieces are needed (under `spectec/tools/`):

1. **`tools/aprove/aprove.jar`** — WST backend (`aprove-developers/aprove-releases`);
   `tools/aprove/runme` runs `java -jar aprove.jar -u cli -t $2 -p plain -m wst $1`.
   Resolution: `--aprove-bin`, then `SPECTEC_APROVE_BIN`, then
   `spectec/tools/aprove/runme`.
2. **`tools/z3/z3`** — the SMT backend AProVE actually uses (its default
   `SmtSolver = "z3"`; official download page lists Z3 ≥ 4.4.0, no license gate:
   `Z3Prover/z3` release asset `z3-*-x64-glibc-*.zip`). `runme` puts it on PATH.
   The WST strategy *also* has legacy `Engine = YICES` processors, but with no
   `yices` on PATH those **abort gracefully** (`Aborted Exec, QTRSRRR with some
   error`) and AProVE proceeds with Z3-based dependency-pair proofs — so
   **yices 1.x is NOT required** (AProVE's own docs call it "an outdated
   dependency [we] will update in the future"). yices/minisat2 would only be a
   faster path for arithmetic-heavy slices (SAT falls back to internal SAT4J).

So the gate pairs **CRC local confluence** with an **AProVE(Z3) termination
proof** for full Church-Rosser. Standalone, the CRC still stands alone as local
confluence + sort-decreasingness (the CTRS assumed terminating).

**The MTT path is retired.** Termination used to run through the old external
stack — Maude 2.7.1 with external hooks (`tools/maude271-hooks/`, release
`v2.7.1-ext-hooks`, binding MTT's `termCheck`/`writeToFile`), the old MFE's
MTT 1.5j, and a `run-termination.sh` driver. MTT's `equal(s,t) -> tt` condition
encoding plus its hardcoded 120 s inner AProVE budget is exactly what produced
the historical MAYBEs; the direct path closes them (153/153 YES). The full
causal analysis lives in CLAUDE.md ("Do not route termination through MTT").
`tools/maude271-hooks/maude` is still worth keeping on disk: it is the
CETA-less binary the SCC plumbing smoke-test points at (below).

## SCC (Sufficient Completeness Checker) — the CETA-linked Maude 2.7 stack

`main.exe scc --symbol <sym> FILES…` checks one analysis-CTRS slice for
**sufficient completeness**: does every ground term reduce to a constructor
term, or does some defined symbol get stuck for want of a matching rule? That
is the static form of the completeness gap `check_diff_p4.sh` hunts
empirically. (`--all` sweeps every defined head, smallest slice first; `--out`
makes the sweep resumable.)

Two components, both gitignored, both obtained on demand:

1. **`tools/maude27-ceta/`** — Maude 2.7 built against the **CETA** library (tree
   automata modulo equational theories), which is what binds the SCC's
   `test-emptiness` hook. Release `v2.7-ext-hooks` of `maude-team/maude`, asset
   `maude-2.7-hooks-linux.zip`, unpacked flat. **This is the only Linux binary that
   has it** — despite its title, `v2.7.1-ext-hooks` ships the MTT hooks only
   (`strings maude | grep -ci ceta` → 0 for it and for stock 3.5.1, 111 here).
   Without CETA the SCC loads and selects fine, then refuses the check.
   Resolution: `--ceta-maude-bin`, then `SPECTEC_CETA_MAUDE_BIN`, then
   `spectec/tools/maude27-ceta/maude`.
2. **`tools/mfe271/MFE-mfe-2.7.1/`** — the old MFE (`maude-team/MFE` tag
   `mfe-2.7.1`); its `src/SCC/scc.maude` is the SCC 2a bundle. Resolution:
   `--mfe271-dir`, then `SPECTEC_MFE271_DIR`, then
   `spectec/tools/mfe271/MFE-mfe-2.7.1`.

Pointing `--ceta-maude-bin` at the CETA-less `tools/maude271-hooks/maude`
exercises the whole pipeline and gets `ERROR-NO-CETA`, which is how to
smoke-test the plumbing.

### Reading a verdict

`scc` prints `<symbol> <verdict> <fidelity> [witness]` (the retired
`run-scc.sh`'s exact row format), and **the fidelity column decides what the
verdict is worth**:

- The SCC's `drop-bad-eqs` silently discards conditional and non-left-linear
  equations, which on our surface would throw the slice away. So the bridge
  first drops conditions and linearizes patterns
  (`Rewrite_system.drop_conds`/`linearize_lhs`, the same transform as
  `rewrite --ctrs --unconditional`). That **over-approximates matching**: it
  can hide a missing case, never invent one.
- ⇒ **COUNTEREXAMPLE is sound either way** (the witness names a constructor case no
  rule's lhs covers), while **COMPLETE only proves something for an `exact` slice**
  — one the transform did not have to touch.
- The `analysis:` half of the column is the SCC's own report on its abstraction
  (`complete+sound`), which it prints per run.

A sound witness is still not automatically a bug: it may be **unreachable**. Triage
it exactly like a CRC MAYBE — confirm a real call site can build the term. Two
recurring unreachable classes are documented in
[lib/rewrite/todo.md](../../lib/rewrite/todo.md): the binary-encoding canonicity
invariant (`bd0`/`bd1` never wrap `bzero` — hand-verified, not enforced by the
sorts), and predicates declared over the top sort `Val` (so a `NatV` argument is
well-sorted but never actually passed).

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

## Signature pruning

`To_mfe` emits the whole ~460-sort / ~750-op P4 signature for **every** slice
(only the rules are sliced), and the SCC's tree-automaton construction is
superlinear in the signature. `rewrite --ctrs --prune-signature` (in-binary,
the semantic equivalent of the retired `prune_slice_signature.py full`) keeps
only the sorts/ops the rules actually use — a few dozen, bakery scale — plus
every sort on a subsort path between two kept sorts; `scc` applies it
automatically. It operates only on the emitted analysis module, never a rule,
the spec source, or the executable system. The termination path does not prune
at all: the unraveling drops sorts and never reads the signature.

(History: a *modular* A/B termination decomposition — abstract the arithmetic
library as free constructors, prove the spec layer and the library separately —
was prototyped in `prune_modular.py`/`prune_root.py` while MTT was the
bottleneck. The direct unravel+AProVE path made it unnecessary — 153/153 YES
without decomposition — and both scripts are retired with it.)

### ⚠️ Caveat — analysis surface, not executable surface

`termination` checks the `rewrite --ctrs` **analysis** surface, which leaves
**`isStuckHead` ruleless**. That is a
strictly weaker system than the executable surface (`to_maude.ml`, which fully
defines `isStuckHead`), so a termination `YES` here **does not certify executable
termination**. Concrete witness: `$bitstr_to_int` non-terminates at `w=0` on the
executable surface (empty two's-complement range `[-2^(w-1), 2^(w-1))`), but that
loop is *masked* on the analysis surface (its `isStuckHead(…)=false` premise is
unsatisfiable). numerics.ml's reference `bitstr_to_int'` shares the same w=0 loop
(so the CTRS translation is faithful); it terminates in practice only because
callers always pass `w≥1`. The detailed notes (**F-w0**, **F-표면**, **F-정리**)
lived in the repo-root recalibration report until it was reduced to tables in
`e58c6153` — recover them with
`git show e58c6153^:spectec-crc-termination-recalibration.md`; the current
tables/notes are `verification.md` / `verification-notes.md`.

## Performance — use per-symbol slices

One CRC call is dominated by critical-pair generation, which **explodes on the
whole system** (every `match_*`/`subty_*` equation pairs against the others). On
`impty/base` the whole-system CRC does not finish in tens of seconds, but a
single definition's dependency slice is fast:

```
# from repo root; per-symbol slice is the practical path
spectec/_build/default/bin/main.exe rewrite --list-symbols \
  spectec/specs/impty/base/spec.spectec
spectec/_build/default/bin/main.exe confluence --symbol '$lookup' \
  spectec/specs/impty/base/spec.spectec
#   $lookup	YES	YES        (~1.4s)
```

`confluence --symbol NAME` slices to `NAME`'s downward dependency closure
(`Rewrite_system.slice`); the explosive roots (e.g. `Run_prog`, whose slice is
the whole reachable system) report `TIMEOUT` rather than a false `Error`.
