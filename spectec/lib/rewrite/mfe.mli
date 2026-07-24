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

(** A component verdict of {!check_normalize_upgrade}: [via_normalize] marks a
    verdict that only the normalized re-run established. *)
type checked = { verdict : verdict; via_normalize : bool }

type upgrade_result = { crc : checked; chc : checked }

val string_of_verdict : verdict -> string

(** [batch_checks_done raw]: in a batched session where [raw] accumulates only
    the current symbols output, its CRC+ChC block is complete once the
    coherence check output is followed by the next [MFE>] prompt. Serves as the
    per-symbol [Subproc.run]/session [done_when] in a batched sweep. *)
val batch_checks_done : string -> bool

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
    60). [prune_signature] (default [false]) restricts the emitted signature to
    what the rules use ({!To_mfe.module_of_system}) -- verdict-preserving, for
    when the full signature would drown the checker. *)
val check :
  ?timeout:int ->
  ?maude_bin:string ->
  ?mfe_dir:string ->
  ?prune_signature:bool ->
  ?sig_rules:Rewrite_system.rule list ->
  Lang.Il.spec ->
  Rewrite_system.t ->
  result

(** The upgrade-only verdict transfer: a [normalized] YES proves the [original]
    system's property (the {!Rewrite_system.crc_normalize} inline is an
    equivalence and its unravel REFLECTS confluence -- Marchiori 1996;
    Nishida-Sakai-Sakabe LMCS 2012), so an inconclusive original ([Maybe]/
    [Timeout]) is upgraded to [Yes] and NOTHING ELSE ever changes: any other
    normalized outcome keeps the original verdict (never a downgrade -- a
    normalized MAYBE says nothing about the original, since the unravel does not
    preserve confluence). *)
val upgrade : original:verdict -> normalized:verdict -> verdict

(** [check] followed by an upgrade-only retry: when a component verdict is
    inconclusive ([Maybe]/[Timeout]), the system is re-checked with
    {!Rewrite_system.crc_normalize} applied AND its signature pruned
    ([~prune_signature:true], verdict-preserving), and each component is
    {!upgrade}d independently. A system the normalization leaves unchanged is
    not re-run. The base check stays unpruned, matching the plain {!check}
    baseline; the [sig_rules] default recovers the signature from the ORIGINAL
    system's rules, matching the [rewrite --ctrs --crc-normalize] dump. *)
val check_normalize_upgrade :
  ?timeout:int ->
  ?maude_bin:string ->
  ?mfe_dir:string ->
  ?sig_rules:Rewrite_system.rule list ->
  Lang.Il.spec ->
  Rewrite_system.t ->
  upgrade_result

(** [check_batch ... slices] checks many [(label, slice)] pairs in ONE MFE
    session, paying the ~100s Full Maude load once instead of once per symbol.
    Each slice is emitted as a uniquely-named module, so the session's output
    stream carries each symbol's verdict under its own name. A symbol exceeding
    [timeout] is recorded [Timeout]/[Timeout] and the now-blocked session is
    respawned for the remaining symbols. Verdicts match {!check} run per symbol.
    [prune_signature]/[sig_rules] are as in {!check}. [on_result label r secs] is
    called as each symbol lands, [secs] being that symbol's wall-clock (the first
    symbol of a (re)spawned session also carries the one-time Full Maude load). *)
val check_batch :
  ?timeout:int ->
  ?maude_bin:string ->
  ?mfe_dir:string ->
  ?prune_signature:bool ->
  ?sig_rules:Rewrite_system.rule list ->
  ?on_result:(string -> result -> float -> unit) ->
  Lang.Il.spec ->
  (string * Rewrite_system.t) list ->
  (string * result) list
