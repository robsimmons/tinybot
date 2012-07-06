signature TERM = sig
  
  type term 
  type t = term
  datatype term_rep =  Atom of string * term list
  val inj : term_rep -> term
  val prj : term -> term_rep
  val Atom' : string * term list -> term
  val eq : term * term -> bool
  val compare : term * term -> order
  val to_string : term -> string

end
