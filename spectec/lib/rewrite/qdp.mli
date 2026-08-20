(** One child of AProVE's dependency-graph decomposition, re-posed as a
    standalone problem it can be run on alone.

    AProVE's [(4) AND] node is its own sound modularization: the initial
    dependency-pair problem is split into one QDP child per strongly connected
    component of the dependency graph, and proving every child finite proves the
    system terminating. A whole-system run shares ONE deadline across all of
    them -- 577 of them on [Program_ok] -- so a child the proof tree leaves open
    may be hard, or may simply never have been reached, and the tree cannot tell
    those apart. Handing a child the budget to itself can: of the 75 children a
    3600s run left open there, 48 close standalone in a median 37s, and the
    processors that close them ([QDPOrderProof], the reduction-pair search) are
    ones the shared run never completed even once.

    What the split does NOT change is the answer's meaning. The children are
    conjuncts, so closing all of them is the same proof the whole run was after;
    the verdict transfers by the soundness of the [AND], not by an argument of
    ours.

    The re-posed problem is deliberately HARDER than the one AProVE printed: it
    carries the rules of the system we submitted, which is a superset of
    whatever the run had reduced them to, and restricting a problem's rules only
    removes chains. So a [YES] here is a [YES] there, whatever processors ran
    above the child. *)

(** A child obligation as the echo printed it: its dependency pairs, its [Q],
    and whether the chains it must consider are the MINIMAL ones (which is what
    licenses the usable-rules pruning below, and what [(MINIMAL)] declares). *)
type problem = {
  node : int;
  pairs : string list;
  q : string list;
  minimal : bool;
}

(** The line filter a capture run needs ({!Aprove.capture}): everything except
    the body of each obligation's [R] section, which is the submitted rule set
    echoed back once per child -- gigabytes of text we already have. Stateful
    (it tracks which section it is inside), so one filter serves one run. *)
val capture_filter : unit -> string -> bool

(** The nodes a proof tree leaves OPEN: those with no sub-proof under them and
    no verdict of their own. Empty when the tree contains an [OR] node, whose
    branches are alternatives rather than conjuncts -- closing all the leaves of
    such a tree proves more than is needed, but reading it as an AND would be a
    claim about a structure we have never seen AProVE print, so it is refused
    instead of guessed. *)
val open_leaves : string -> int list

(** The obligations of a captured run, parsed out of the echo behind the tree. A
    node whose echo carries no dependency pairs (the root [QTRS], an [ITRS]
    problem, an echo the deadline cut short) is not a QDP child and is skipped
    -- {!Termination} checks that every open leaf came back before it trusts a
    split. *)
val problems : string -> problem list

(** [standalone ~export problem] is the TPDB [.qdp] text of one child, with the
    number of rewrite rules it carries.

    The rules come from [export], not from the echo, and are pruned to the
    problem's USABLE rules: those defining a symbol the dependency pairs name,
    closed under the symbols those rules use. That is what makes the split
    affordable -- about 30s of each standalone run is parsing the 4MB rule set
    the child would otherwise carry -- and it is sound on a minimal problem
    provided the [c(x,y) -> x | y] pair goes in with it, which is why the prune
    is taken only when [problem.minimal] holds and why those two rules are
    emitted whenever anything was dropped.

    [Error] rather than a silently different system when an identifier in the
    pairs is neither a variable ([x<n>], the names AProVE renames to) nor a
    symbol of [export]: an undeclared identifier parses as a CONSTANT, which
    would specialize the problem and prove something narrower than was asked. *)
val standalone :
  export:Unravel.export -> problem -> (string * int, string) result
