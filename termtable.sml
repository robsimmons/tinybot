structure TermTable:> sig

   type key = Term.term
   type 'a table
   type 'a t = 'a table
   exception Absent
   val table : int -> 'a table
   val reset : 'a table -> int -> unit
   val find : 'a table -> key -> 'a option
   val member : 'a table -> key -> bool
   val lookup : 'a table -> key -> 'a
   val app : (key * 'a -> unit) -> 'a table -> unit
   val operate : 'a table -> key -> (unit -> 'a) -> ('a -> 'a) -> 'a option * 'a

end = struct

   type key = Term.term 
   type 'a table = (key * 'a) list ref
   type 'a t = 'a table
   exception Absent
  
   fun table n = ref []

   fun reset tabl n = tabl := []
   
   fun do_find key [] = NONE
     | do_find key ((tm, v) :: pairs) =
          if Term.eq (key, tm) 
          then SOME v else do_find key pairs

   fun find (ref pairs) key = do_find key pairs

   fun member tabl key = 
      case find tabl key of NONE => false | SOME _ => true

   fun lookup tabl key = 
      case find tabl key of NONE => raise Absent | SOME v => v

   fun app f (ref pairs) = List.app f pairs

   fun do_operate key ifnew ifthere [] =
          let val v' = ifnew () 
          in (NONE, v', [(key, v')]) end
     | do_operate key ifnew ifthere ((tm, v) :: pairs) = 
          if Term.eq (key, tm)
          then let val v' = ifthere v 
               in (SOME v, v', ((tm, v') :: pairs)) end
          else let val (oldv, newv, pairs) = do_operate key ifnew ifthere pairs
               in (oldv, newv, (tm, v) :: pairs) end

   fun operate tabl tm ifnew ifthere = 
      let val (oldv, newv, pairs) = do_operate tm ifnew ifthere (!tabl)
      in tabl := pairs; (oldv, newv) end

end
