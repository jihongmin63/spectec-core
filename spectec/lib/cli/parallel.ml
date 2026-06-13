(* A fresh pool per call: [workers] threads pull indices off a shared counter
   (work-stealing) until the input is exhausted, writing into a preallocated
   results array so order is preserved. No global pool, so nesting one [map]
   inside another cannot deadlock -- each call's threads are independent and
   only ever block on their own I/O. *)
let map ~jobs f xs =
  match xs with
  | [] -> []
  | [ x ] -> [ f x ]
  | _ when jobs <= 1 -> List.map f xs
  | _ ->
      let arr = Array.of_list xs in
      let n = Array.length arr in
      let results = Array.make n None in
      let next = ref 0 in
      let mutex = Mutex.create () in
      let first_exn = ref None in
      let take () =
        Mutex.lock mutex;
        let i = !next in
        if i < n then incr next;
        Mutex.unlock mutex;
        if i < n then Some i else None
      in
      let rec worker () =
        match take () with
        | None -> ()
        | Some i ->
            (try results.(i) <- Some (f arr.(i))
             with e ->
               Mutex.lock mutex;
               if !first_exn = None then first_exn := Some e;
               Mutex.unlock mutex);
            worker ()
      in
      let workers = min jobs n in
      List.init workers (fun _ -> Thread.create worker ())
      |> List.iter Thread.join;
      (match !first_exn with Some e -> raise e | None -> ());
      Array.to_list
        (Array.map (function Some v -> v | None -> assert false) results)
