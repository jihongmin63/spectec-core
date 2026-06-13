# Maude bridge (execution of the translated spec)

The `run` subcommand emits the translated spec as an order-sorted Maude **system
module** and executes it with a local [Maude](https://maude.cs.illinois.edu/)
binary. Unlike CoCoWeb / AProVE / MuTerm (which only *decide* confluence or
termination), Maude actually *runs* the rewriting system: given a start term it
`reduce`s (equations only) or `search`es / `rewrite`s (rules + equations) and
reports the resulting normal form(s).

The bridge ([../../lib/rewrite/maude_run.ml](../../lib/rewrite/maude_run.ml))
invokes Maude directly via `Unix.open_process_in` — no Python client — because
Maude reads a module-plus-commands file and prints the result to stdout:

```
maude -no-banner module-and-commands.maude
```

`reduce f(a) .` prints `result <Sort>: <term>`; `search t =>* X:S .` prints one
`Solution N (state M)` block per solution (each with an `X:S --> <term>` line)
ending in `No more solutions.` (or `No solution.`).

## Setup

The Maude binary and its `.maude` library are **not** checked into this repo
(the binary is ~6.6 MB). Download a release and place it here:

1. Download the platform build from
   <https://github.com/maude-lang/Maude/releases> (Linux x86_64:
   `Maude-<ver>-linux-x86_64.zip`).
2. Unzip it into this directory (`spectec/tools/maude/`) so that `maude` and
   `prelude.maude` sit side by side, then `chmod +x maude`. Maude locates
   `prelude.maude` relative to its own binary, so it works from any working
   directory.

This repo was tested against **Maude 3.5.1**:

```bash
cd spectec/tools/maude
curl -sL -o maude.zip \
  https://github.com/maude-lang/Maude/releases/download/Maude3.5.1/Maude-3.5.1-linux-x86_64.zip
unzip -o maude.zip && rm maude.zip && chmod +x maude
./maude --version          # -> 3.5.1
```

## Binary resolution

The bridge looks for the binary in this order (a missing binary is a clean
`Error` with this setup hint, not a crash):

1. `--maude-bin /path/to/maude`
2. `SPECTEC_MAUDE_BIN=/path/to/maude`
3. repo-relative `spectec/tools/maude/maude` (the default this README sets up)
4. `maude` on `PATH`

## Usage

```bash
spectec/_build/default/bin/main.exe run --lang impty --search spec.spectec
```
