(** Submit a {!Rewrite_system.t} to the Maude Formal Environment (MFE) and
    report its Church-Rosser (CRC) and coherence (ChC) verdicts.

    [check] renders the system as an order-sorted Full Maude *system* module
    ({!To_mfe.module_of_system}) -- purely equational ([eq]/[ceq]), since every
    SpecTecx relation is input-moded -- loads the MFE into a local Maude binary,
    and runs the Church-Rosser and coherence checks in one invocation. CRC
    decides whether the equations are confluent (so [reduce] is well-defined);
    ChC whether the (absent) rules are coherent with them. See
    [tools/mfe/README.md].

    [Timeout] (the tool ran out of time) is kept distinct from [Maybe] (it
    finished but could not confirm the property). *)

type verdict = Yes | No | Maybe | Timeout | Error of string
type result = { church_rosser : verdict; coherence : verdict }

val string_of_verdict : verdict -> string

(** [check ?timeout ?maude_bin ?mfe_dir ?sig_rules orig system]. [orig] is the
    elaborated IL spec, from which {!To_mfe} recovers each operator's sort.
    [sig_rules] are the rules the signature is recovered from (default:
    [system]'s own): pass the WHOLE system when [system] is a slice, so the
    slice declares the same predicate domains the full module does
    ({!Maude_sorts.predicate_domains}).

    [maude_bin] defaults to [SPECTEC_MAUDE_BIN], then a repo-relative [maude].
    [mfe_dir] (holding the Full Maude + CRC/ChC loader) defaults to
    [SPECTEC_MFE_DIR], then [spectec/tools/mfe]; a missing MFE is a clean
    [Error], not a crash. [timeout] is the whole-run budget in seconds (default
    60). *)
val check :
  ?timeout:int ->
  ?maude_bin:string ->
  ?mfe_dir:string ->
  ?sig_rules:Rewrite_system.rule list ->
  Lang.Il.spec ->
  Rewrite_system.t ->
  result
