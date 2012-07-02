(* Tinybot syntax
 * Robert J. Simmons *)

structure Syntax = struct
 
  datatype term = 
    Var of string
  | Atom of string * term list

  datatype rule = 
    R of {prem : term list, conc : term list}

  type prog = rule list

  fun freevars term = 
    case term of 
      Var x => SetS.singleton x
    | Atom(x, tms) =>
      foldr SetS.union SetS.empty (map freevars tms)

  fun to_string term = 
    case term of 
      Var x => x
    | Atom(x,[]) => x
    | Atom(x,tms) => 
      x ^ "(" ^ String.concatWith ", " (map to_string tms) ^ ")"
      
  fun rule_to_string (R{prem, conc}) = 
    String.concatWith ", " (map to_string prem) ^ " => " ^
    String.concatWith ", " (map to_string conc)    

end

