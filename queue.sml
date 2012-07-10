(* Fact Queue/worklist *)

structure Queue :> sig

   type queue
   type t = queue
   val new : unit -> queue
   val insert : queue -> Term.term * bool ref -> unit
   val pop : int option -> queue -> (Term.term * bool ref) option
   val print : queue -> unit
   val size : queue -> int

end = struct
  
   type queue = ((int * Term.term * bool ref) list * int) ref
   type t = queue

   fun size (ref (facts, _)) = length facts

   fun new () = ref ([], 1)

   fun insert (facts as ref (queue, n)) (tm, r) =
      let 
      in facts := ((n, tm, r) :: queue, n+1)
      end

   fun do_pop _ [] = (NONE, [])
     | do_pop _ [(_, tm, r)] = (SOME (tm, r), [])
     | do_pop NONE ((y, tm, r) :: queue) = (SOME (tm, r), queue)
     | do_pop (SOME x) ((y, tm, r) :: queue) = 
          if EQUAL = Int.compare (x, y) 
          then (SOME (tm, r), queue)
          else let val (tm', queue') = do_pop (SOME x) queue
               in (tm', (y, tm, r) :: queue') 
               end

   fun pop x (facts as ref (queue, n)) = 
      let val (tm, queue') = do_pop x queue
      in facts := (queue', n); tm 
      end

   val print =
      fn (ref (queue, _)) =>
        List.app (fn (n, tm, _) =>
                    print ("  " ^ Term.to_string tm ^
                           " (" ^ Int.toString n ^ ")\n")) 
           queue

end
