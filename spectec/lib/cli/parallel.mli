(** Bounded-concurrency parallel map for I/O-bound work -- chiefly external tool
    subprocesses (CoCoWeb, MuTerm) that spend nearly all their time waiting on
    the network, so POSIX threads overlap the waits even without multicore.

    [map ~jobs f xs] applies [f] to each element of [xs], running up to [jobs]
    applications at once, and returns the results in the original order. With
    [jobs <= 1], or an empty/singleton list, it is a plain sequential [List.map]
    and spawns no threads. If [f] raises, the in-flight work is left to drain
    and the first exception observed is re-raised from [map]. *)
val map : jobs:int -> ('a -> 'b) -> 'a list -> 'b list
