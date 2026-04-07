(** Parse a textual value into an IL value.

    Supported syntax includes:
    - booleans: [true], [false], [BoolV true]
    - numbers: [0], [-1], [+2], [NumV(3)]
    - strings: ["text"], [TextV("text")]
    - options: [None], [Some(value)]
    - lists: [[value1, value2]]
    - tuples: [(value1, value2)]
    - structs: [{ field = value; other = value }]
    - funcs: [$name], [Func(name)], [FuncV name]
    - cases: [Case(Tag, [value1, value2])] *)

val parse_il_value : string -> (Lang.Il.value, string) result
val parse : string -> (Lang.Il.value, string) result
