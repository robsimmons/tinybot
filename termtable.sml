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
   val remove : 'a table -> key -> unit
   val operate : 'a table -> key -> (unit -> 'a) -> ('a -> 'a) -> 'a option * 'a

end = struct

   structure Dict = SplayDict(structure Key = Term)

   type key = Term.term 
   type 'a table = 'a Dict.t ref
   type 'a t = 'a table
   exception Absent
  
   fun table n = ref Dict.empty

   fun reset tabl n = tabl := Dict.empty
   
   fun find (ref dict) key = Dict.find dict key

   fun member (ref dict) key = Dict.member dict key

   fun lookup (ref dict) key = Dict.lookup dict key

   fun app f (ref dict) = Dict.app f dict

   fun remove tabl tm = tabl := Dict.remove (!tabl) tm

   fun operate tabl tm ifnew ifthere = 
   let val (old, new, dict) = Dict.operate (!tabl) tm ifnew ifthere
   in tabl := dict; (old, new) 
   end

end
