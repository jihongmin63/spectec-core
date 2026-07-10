# Structural (CTRS) backend — Phase D result-value re-validation

**Date:** 2026-07-10
**Branch:** `new-rewrite`
**Scope:** result-value oracle (Phase D) of the structural differential harness
[`check_diff_structural_p4.sh`](check_diff_structural_p4.sh), re-run over the full
interp-PASS corpus on the new **binary-nat** structural theory.

Prior structural validation had only ever been verdict-level (OK / STUCK / OOM).
Phase D compares the **typing RESULT value itself** — the interpreter's value vs
the value Maude computes and `Of_maude` decodes back — so it catches the class of
bug the verdict oracle cannot: *"accepts the program but computes the wrong
type."* This run is the first such check on the structural backend, and it found
(and this session fixed) exactly that class of bug.

## Headline result

Over the **1227 interp-PASS** P4 programs (of 1568 total; 341 interp-FAIL), after
the fix below:

| verdict | count | meaning |
|---------|------:|---------|
| **MATCH** | **1227** | structural value == interpreter value (100%) |
| **MISMATCH** | **0** | wrong value computed — none |

Before the fix, 17 of those 1227 computed a **wrong value** (MISMATCH). They had
initially been mis-recorded as "TIMEOUT" — see the next section for why that
masking happened and how it was undone.

Live output: [`check_diff_p4_structural_resultmatch.tsv`](check_diff_p4_structural_resultmatch.tsv).

## The 17 "TIMEOUT" files were MISMATCHes in disguise

The first pass classified 17 slow programs as `TIMEOUT`. Investigation showed
this label was wrong on two counts:

1. **Not OOM.** Run uncapped (`--timeout 0`, no memory cap), three
   representatives completed with peak RSS **169–339 MB** (free RAM never dropped
   below 5 GB), so the harness's 2.5/5 GB `ulimit -v` was never the cause:

   | file | CPU time | peak RSS |
   |------|---------:|---------:|
   | issue1806.p4 (43 lines) | ~120 s | 169 MB |
   | psa-dpdk-table-key-consolidation-mixed-keys.p4 | 144 s | 339 MB |
   | ternary2-bmv2.p4 | 135 s | 299 MB |

2. **The "timeout" masked a MISMATCH.** These reductions are genuinely slow
   (~120–145 s of CPU) because they exercise heavy table-key / bit-slice /
   wide-width typing. `run-structural` runs `timeout <T> maude …`, and the
   harness's per-reduce `--timeout` (120 s main pass) killed them *before the
   comparison ran*. Re-run with a generous `--timeout 600`, all 17 completed and
   reported `result: MISMATCH` — a real wrong value the timeout had been hiding.

## Root cause and fix — `$int_to_text` sign — commit `5e1c3ea1`

Every one of the 17 mismatches was the same single-character difference in a
numeric IR **name** annotation: the interpreter prefixes non-negative integers
with `+`, the structural backend did not.

| file | interp | structural (before fix) |
|------|--------|-------------------------|
| issue1806 | `h.eth.tst[+13:+4]` | `h.eth.tst[13:4]` |
| match-on-exprs2 | `srcAddr[+22:+18]` | `srcAddr[22:18]` |
| psa-header-stack | `vlan_tag[+0]` | `vlan_tag[0]` |
| psa-dpdk-mixed-keys | `+8w+72` | `8w72` |

Root cause: [`translate/builtin.ml`](spectec/lib/rewrite/translate/builtin.ml)'s
structural `$int_to_text` rendered `int_pos n` as bare digits, dropping the sign.
The interpreter's `Xl.Num.string_of_num` and
[`To_maude`](spectec/lib/rewrite/maude/to_maude.ml)'s **Native** `$int_to_text`
delegation both emit an explicit sign for a signed `int` (`+0`, `+72`, `-5`);
only the structural rule omitted it. The `int_neg` arm already prepended `-`
(`chr_45`); the fix prepends `+` (`chr_43`) on the `int_pos` arm to match.

`$int_to_text` reaches this path from `$name_expression` (`D i` / `n W i` /
`n S i`) when a table key or annotation contains a numeric literal, slice index,
or width — exactly the constructs the 17 slow programs share, which is why the
slowness and the mismatch co-occurred.

**Native was never affected** (it delegates the signed rendering to Maude's
`string`), so the Native oracle stayed 1227/1227 MATCH throughout.

### Verification

- **Unit** (direct Maude reduce on the emitted structural module):
  `$int-to-text(int-pos(bone))` → `"+1"`, `int-pos(bzero)` → `"+0"`,
  `int-neg(bzero)` → `"-1"` (negative rendering unchanged).
- **End-to-end:** all **17** previously-MISMATCH files now `result: MATCH`
  (`run-structural --check-p4 --timeout 600`).
- **Regression:** a 20-file sample of previously-MATCHing programs (tuple types,
  numeric table keys, annotations — the constructs most likely to exercise the
  same path) stays **20/20 MATCH**; no non-negative `nat` name regressed to a
  spurious `+`.
- **Golden:** impty `spec.ctrs` unchanged (impty does not use `$int_to_text`).

## Earlier fixes on this track

Two prior fixes on the same branch, both prerequisites for reaching a clean
result-value comparison at all:

- **`199c72eb`** — `Of_maude` scalar-decode symbol demangling. Three decode arms
  (`chr_<code>`, `int_pos`, `int_neg`) compared the parsed (hyphen-mangled)
  symbol against its pre-mangling underscore spelling and could never match, so
  every cleanly-reducing P4 result failed at decode
  (`decode error: expected a char-list element (chr_<code>)`). Fix: demangle
  locally at the two consumption sites.
- **`5e3b37a1`** — structural `nat`: Peano → binary. A `nat_of_int` down-cast on
  a wide constant unfolded the binary magnitude into a `2^48`-tall `succ` tower
  and OOM'd (`v1model-const-entries-bmv2.p4`). Migrated `nat` to the binary
  encoding the structural `int` already used, reusing the `badd`/`bsub`/`bmul`/
  `bdiv`/`bmod` engine.

## Soundness

**0 MISMATCH** after the fix — no interp-PASS program has the structural backend
compute a wrong value. The complementary soundness *leg* (interp-FAIL programs
the structural backend nonetheless accepts) is a separate run and was out of
scope for this Phase-D-only re-validation; the Native oracle's standing figure
there is 1 known soundness gap (issue1944).

## Native backend regression spot-check

Because the fixes touch shared translation code, 12 diverse programs (including
six of the slow files above) were re-checked through the **Native** execution
module (`run --p4 --check-p4 --timeout 0`, built-in `nat`/`int`): **12/12 MATCH**,
confirming none of this track's changes perturbed the Native path.

## Reproduction

```bash
# build
cd spectec && opam exec --switch=spectecx -- dune build bin/main.exe && cd ..
make exe                                   # hardlink ./spectecx

# one structural result-value check. --timeout 0 (or a generous value): these
# table-heavy programs need ~120-145s of CPU. Pass 2+ files (or one + a small
# sentinel) so run-structural stays in its multi-file "=== path ===" banner mode
# -- a lone --p4 skips the banner and the harness mis-parses the result:
spectec/_build/default/bin/main.exe run-structural \
  --p4 <FILE.p4> --p4 <SENTINEL.p4> \
  -i spectec/testdata/interp/p4/p4c/includes --check-p4 --timeout 600 \
  $(find spectec/specs/p4 -name '*.spectec' | sort)
# -> "result: MATCH" | "result: MISMATCH ..." per program

# NOTE: Native run --check-p4 needs --timeout 0; the default wall-clock cap can
# interrupt the reduce and report a spurious "not reduced".

# full Phase D over the corpus (Phase A/B verdicts reused; resumable):
./check_diff_structural_p4.sh
```

### Method notes (why the counts are trustworthy)

- The corpus was sharded 2-way (`CORPUS_LIST` shards, per-shard `MPROG`/`RESMATCH`)
  under a memory cap to fit 6 GB RAM.
- Phase D has no per-file fallback: one slow file that exceeds `BATCH_TIMEOUT`
  (or a batch-mate hitting the memory cap) marks the whole batch `TIMEOUT`. Every
  such collateral `TIMEOUT` was re-run at progressively smaller batch sizes, then
  the residue was **isolated one slow file at a time** (paired with a 1-line
  known-MATCH sentinel to preserve the banner the parser needs). Collateral
  peeled off across the passes: 300 → 58 → 30 → 17 truly-slow files — which then
  turned out to be the 17 MISMATCHes fixed above.
- **Lesson for the harness:** a finite per-reduce `--timeout` silently converts a
  *wrong-value* result into a *timeout*. For a trustworthy result-value oracle,
  slow files must be re-run uncapped before a `TIMEOUT` is believed.

### Artifacts

- [`check_diff_structural_p4.sh`](check_diff_structural_p4.sh) — the A/B/C/D
  differential driver (`CORPUS_LIST`/`MPROG`/`RESMATCH`/`JOINED` overridable for
  disjoint parallel shards).
- [`check_diff_p4_structural_resultmatch.tsv`](check_diff_p4_structural_resultmatch.tsv)
  — the merged Phase D verdicts.
