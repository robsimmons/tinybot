(* Fact Queue *)

structure Queue :> sig

   type facts
   val new : Term.term list -> facts
   val insert : facts -> Term.term -> unit
   val pop : int option -> facts -> Term.term option

end = struct
  
   type facts = ((int * Term.term) list * int) ref

   fun new tms = ref ([], 621413)

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

end
