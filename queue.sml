(* Fact Queue/worklist *)

structure Queue :> sig

   type queue
   type t = queue
   val new : unit -> queue
   val insert : queue -> Term.term -> unit
   val pop : int option -> queue -> Term.term option
   val print : queue -> unit

end = struct
  
   type queue = ((int * Term.term) list * int) ref
   type t = queue

   fun new () = ref ([], 1)

   fun insert (facts as ref (queue, n)) tm =
      let 
      in facts := ((n, tm) :: queue, n+1)
      end

   fun do_pop _ [] = (NONE, [])
     | do_pop _ [(_, tm)] = (SOME tm, [])
     | do_pop NONE ((y, tm) :: queue) = (SOME tm, queue)
     | do_pop (SOME x) ((y, tm) :: queue) = 
          if EQUAL = Int.compare (x, y) 
          then (SOME tm, queue)
          else let val (tm', queue') = do_pop (SOME x) queue
               in (tm', (y, tm) :: queue') 
               end

   fun pop x (facts as ref (queue, n)) = 
      let val (tm, queue') = do_pop x queue
      in facts := (queue', n); tm 
      end

   val print =
      fn (ref (queue, _)) =>
        List.app (fn (n, tm) =>
                    print ("  " ^ Term.to_string tm ^
                           " (" ^ Int.toString n ^ ")\n")) 
           queue

end
