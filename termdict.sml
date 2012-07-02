structure TermTable:> sig

   type key = Term.term
   type 'a table
   exception Absent
   val table : int -> 'a table
   val reset : 'a table -> int -> unit
   val find : 'a table -> key -> 'a option
   val lookup : 'a table -> key -> 'a
   val app : (key * 'a -> unit) -> 'a table -> unit

end = struct

   type key = Term.term 
   type 'a table = (key * 'a) list ref
   exception Absent
  
   fun table n = ref []

   fun reset tabl n = tabl := []
   
   fun do_find key [] = NONE
     | do_find key ((tm, v) :: pairs) =
          if Term.eq (key, tm) 
          then SOME v else do_find key pairs

   fun find (ref pairs) key = do_find key pairs

   fun lookup tabl key = 
      case find tabl key of NONE => raise Absent | SOME v => v

   fun app f (ref pairs) = List.app f pairs

end
