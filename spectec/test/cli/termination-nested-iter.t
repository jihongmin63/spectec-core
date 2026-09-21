An iterated premise may bind a structured collection at each outer iteration
step.  Its generated collecting helper must use that bound collection directly,
rather than an unbound synthetic element name.

  $ cat > nested-iter.spectec <<'SPEC'
  > syntax val =
  >   | V
  > var x : val
  > var z : val
  > var y : val
  > dec $finds(val*, val) : val*?
  > dec $copy(val*, val*) : val*?*
  > def $copy(x*, z*) = y*?*
  >   -- if (y*? = $finds(x*, z))*
  > SPEC

The termination surface preserves the per-step structured result and exports a
well-formed TRS.

  $ spectec termination --emit --symbol '$copy' nested-iter.spectec > copy.trs 2> copy.err
  $ grep -Fc 'unbound rhs variable' copy.err || true
  0
  $ grep -Fc 'd_iterproj_y_star_lbrace_y_lt_minus_y_star_rbrace_opt_y(d_finds(x, z__hd))' copy.trs
  2

The same rule applies when an iterated premise collects several outputs: the
collector stores their tuple under the original binder names, while each
projection helper binds its own per-element names from that tuple.

  $ cat > multi-iter.spectec <<'SPEC'
  > syntax val =
  >   | V
  > var x : val
  > var y : val
  > var z : val
  > dec $pair(val) : (val, val)
  > dec $copy(val*) : (val*, val*)
  > def $copy(x*) = (y*, z*)
  >   -- if ((y, z) = $pair(x))*
  > SPEC
  $ spectec termination --emit --symbol '$copy' multi-iter.spectec > pair.trs 2> pair.err
  $ grep -Fc 'unbound rhs variable' pair.err || true
  0
