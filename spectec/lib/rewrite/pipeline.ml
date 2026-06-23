(* The one composition of the IL -> CTRS translation: the {!Simplify} pre-pass,
   {!Builtin}'s collection-builtin rules, and the {!To_ctrs} translation. The
   COPS surface ({!Rewrite.rewrite_spec}) and the Maude backend ({!To_maude}'s
   module text and stuck-head set) must all reduce the same system, so every
   consumer goes through this builder instead of re-assembling the stages.

   Debug fallback: to bypass simplification (e.g. to tell whether an odd rule
   comes from [Simplify] or from [To_ctrs]), pass [spec] as the last argument
   instead of [Simplify.simplify_spec spec]. *)
let ctrs_of_spec (spec : Lang.Il.spec) : Rewrite_system.t =
  (* First: specialize away [def]-valued arguments, so every call is
     first-order before simplification/translation see it. *)
  let spec = Defunctionalize.defunctionalize spec in
  To_ctrs.of_spec
    ~extra_defs:(Builtin.rules_of_builtins spec)
    ~orig:spec
    (Simplify.simplify_spec spec)
  (* Last: thread the gensym state through every $fresh_typeId-reaching
     symbol, so both the analysis surface and the Maude backend (which
     restates this system) see the same pure gensym. *)
  |> Gensym.thread

(* The execution pipeline: the same translation restated over Maude's built-in
   Bool/Nat/Int/String ({!Maude_theory.native_system}), consumed only by
   {!To_maude}. The analysis surface (COPS/TPDB) keeps the structural system
   above -- the two pipelines intentionally diverge at this point.

   One-slot memo keyed by physical equality: a single `run` invocation builds
   this system for the module text, the stuck-head set, and the start term,
   all from the same elaborated spec value. *)
let maude_memo : (Lang.Il.spec * Rewrite_system.t) option ref = ref None

let maude_system_of_spec (spec : Lang.Il.spec) : Rewrite_system.t =
  match !maude_memo with
  | Some (s, sys) when s == spec -> sys
  | _ ->
      let sys = Maude_theory.native_system (ctrs_of_spec spec) in
      maude_memo := Some (spec, sys);
      sys
