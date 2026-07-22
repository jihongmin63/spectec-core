(** Sufficient completeness of one analysis-CTRS slice, via the CETA-enabled
    Maude 2.7 + old MFE 2.7.1 (SCC 2a) backend.

    The verdict is only half the answer -- read the {!fidelity} with it: [Exact]
    means the slice needed no transformation, so the SCC saw every rule and
    [Complete] really holds (modulo the SCC's standing assumptions: ground weak
    normalization, confluence, sort-decreasingness -- established separately by
    the CRC and termination runs). [Approx] means {!unconditional} dropped
    conditions and/or linearized patterns to get past the SCC's [drop-bad-eqs]
    filter, which over-approximates what the rules match -- [Complete] then
    proves NOTHING, while a counterexample is sound either way (matching more
    can only hide a missing case, never invent one).

    A counterexample's {!domain} automates the first triage question -- is the
    witness even reachable?: [Val_wide] (the symbol is declared over the top
    sort, so the SCC enumerated every constructor -- suspect first, chase last),
    [Elem_erased] (a List/Opt argument whose element type the container sorts
    cannot carry -- true about the sorts, unreachable in the system), [Narrow]
    (declared over its real IL sort -- THIS is the finding to chase). *)

type domain = Val_wide | Elem_erased | Narrow | Unknown

type verdict =
  | Complete
  | Counterexample of { witness : string; sort : string; domain : domain }
  | Degenerate  (** the slice has no rules; nothing to check *)
  | Timeout
  | No_ceta
      (** the Maude binary has no CETA library bound (the SCC's emptiness test),
          so everything up to it ran -- the plumbing smoke-test verdict *)
  | Error of string

type fidelity = Exact | Approx

type report = {
  verdict : verdict;
  fidelity : fidelity;
  analysis : string option;
      (** the SCC's own "Analysis: it is X and it is Y" line, as [X+Y]: a
          [sound] analysis means a reported counterexample is real, a [complete]
          one means the absence of one is a proof *)
}

val string_of_domain : domain -> string

(** The SCC-facing over-approximation ({!Rewrite_system.drop_conds} +
    {!Rewrite_system.linearize_lhs}) and whether it changed the slice. *)
val unconditional : Rewrite_system.t -> Rewrite_system.t * fidelity

(** The exact module text fed to the checker: the {!To_mfe} order-sorted surface
    as an old-Full-Maude FUNCTIONAL module ([(fmod ... endfm)] with the
    [BOOL]/[BOOL-OPS] includes off), its signature pruned to what the rules use
    ([~prune_signature] -- the SCC needs the [ctor] split, and the full
    ~460-sort P4 signature drowns its tree automaton). Pass the {!unconditional}
    slice; [sig_rules] as in {!Mfe.check}. *)
val module_text :
  ?sig_rules:Rewrite_system.rule list ->
  Lang.Il.spec ->
  Rewrite_system.t ->
  string

(** Classify a raw checker output. Ordered exactly as the retired run-scc.sh:
    the no-CETA refusal first (it contains the words a naive success grep would
    match), then the counterexample presence, then parse errors, else [Timeout].
    [module_text] is scanned for the witness head's [op] declaration to classify
    its {!domain}. *)
val classify : module_text:string -> string -> verdict

(** The "Analysis: it is X and it is Y" passthrough, as ["X+Y"]. *)
val analysis_of_output : string -> string option

(** Where the CETA-enabled Maude 2.7 is resolved from: the argument, then
    [SPECTEC_CETA_MAUDE_BIN], then [spectec/tools/maude27-ceta/maude]. *)
val resolve_ceta_bin : string option -> string

(** Where the old MFE 2.7.1 is resolved from: the argument, then
    [SPECTEC_MFE271_DIR], then [spectec/tools/mfe271/MFE-mfe-2.7.1]; probed for
    [src/mfe.maude], missing is a clean [Error]. *)
val resolve_mfe271_dir : string option -> (string, string) Stdlib.result

(** [check ?timeout ?ceta_bin ?mfe271_dir ?sig_rules orig slice] runs the whole
    chain: {!unconditional} -> {!module_text} -> CETA Maude + old MFE ->
    {!classify}. [timeout] defaults to 600s. *)
val check :
  ?timeout:int ->
  ?ceta_bin:string ->
  ?mfe271_dir:string ->
  ?sig_rules:Rewrite_system.rule list ->
  Lang.Il.spec ->
  Rewrite_system.t ->
  report
