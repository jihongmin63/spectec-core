(** Submit a {!Rewrite_system.t} to the Maude Formal Environment (MFE) and
    report its Church-Rosser (CRC) and coherence (ChC) verdicts.

    [check] renders the system as a single-sort Full Maude *system* module
    ({!Rewrite_system.string_of_system_maude}) -- the equational fragment as
    [eq]/[ceq], the [rule_heads] relations as [rl]/[crl] -- loads the MFE into a
    local Maude binary, and runs the Church-Rosser and coherence checks in one
    invocation. CRC decides whether the equations are confluent (so [reduce] is
    well-defined); ChC whether the rules are coherent with them (so search is
    complete modulo the equations). See [tools/mfe/README.md].

    [Timeout] (the tool ran out of time) is kept distinct from [Maybe] (it
    finished but could not confirm the property). *)

type verdict = Yes | No | Maybe | Timeout | Error of string
type result = { church_rosser : verdict; coherence : verdict }

val string_of_verdict : verdict -> string

(** [check ?timeout ?maude_bin ?mfe_dir ~rule_heads system].

    [rule_heads] are the CTRS symbols emitted as Maude rules ([rl]) rather than
    equations -- the non-input-moded relations (the complement of
    {!To_ctrs.input_moded_rel_syms} among the relation symbols); every other
    symbol is equational. [maude_bin] defaults to [SPECTEC_MAUDE_BIN], then a
    repo-relative [maude]. [mfe_dir] (holding the Full Maude + CRC/ChC loader)
    defaults to [SPECTEC_MFE_DIR], then [spectec/tools/mfe]; a missing MFE is a
    clean [Error], not a crash. [timeout] is the whole-run budget in seconds
    (default 60). *)
val check :
  ?timeout:int ->
  ?maude_bin:string ->
  ?mfe_dir:string ->
  rule_heads:string list ->
  Rewrite_system.t ->
  result
