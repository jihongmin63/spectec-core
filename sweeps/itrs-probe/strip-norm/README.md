# strip-norm probes (2026-09-05)

Six hand-written ITRS files holding `$strip_prefix_rec`'s recursion shape and
nothing else, used to decide **what the list-length lift has to inline** before
it was written (todo.md 2026-09-05). Each is run as

    spectec/tools/aprove/runme <file>.itrs 300

- `a_flat_inline.itrs` — the recursion with both the strip and the guard written
  as constraints. YES.
- `b_chain_inline.itrs` — the same through the `u_i` chain the unraveling makes.
  YES.
- `c_chain_wrapper.itrs` — the strip as a `strip` RULE (one wrapper). YES.
- `d_faithful.itrs` — both the strip (`dropL`) and the guard (`sw`) as rules,
  which is what the lift emits if it inlines neither. **KILLED at 300.**
- `d1_no_sw.itrs` — `d` with the guard inlined into a constraint. YES.
- `d2_no_dropL.itrs` — `d` with the strip inlined. YES.

The point is `d` against `d1`/`d2`: one wrapper in a value position is
survivable and two are not, which is why `Itrs`'s lift inlines a closed-form
call rather than leaving it a rule (`Itrs.discover_macros`), and why
`$starts_with` -- the one wrapper that stays -- has to collapse to a single
constrained rule (`Itrs.drop_vacuous_conds`).
