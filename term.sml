(* Tinybot internal term representation
 * Robert J. Simmons
 * 
 * NOTE: This does not satisfy the McAllester complexity result! 
 * If we have recursive terms (like natural numbers) that can be arbitrarily
 * large, then equality is not a constant-time operation as the McAllester
 * result requires. The signature is written with inj/prj so that the 
 * implementation could be swapped out for a hash-consing implementation
 * that does satisfy the complexity result. *)

structure Term :> TERM = struct
 
  datatype term_rep = 
    Atom of string * term list
  and term = R of term_rep
  type t = term

  val prj = fn (R x) => x
  val inj = fn x => (R x)
  val Atom' = inj o Atom

  fun eq (tm1, tm2) = 
    case (prj tm1, prj tm2) of 
      (Atom(x,tms1), Atom(y,tms2)) => if x = y then eqs(tms1, tms2) else false
  and eqs (tms1, tms2) = 
    case (tms1, tms2) of
      ([],[]) => true
    | (tm1::tms1, tm2::tms2) => if eq(tm1,tm2) then eqs(tms1, tms2) else false
    | _ => false (* type error? *)

  fun compare (tm1, tm2) = 
    case (prj tm1, prj tm2) of
      (Atom(x, tms1), Atom(y, tms2)) =>
      (case String.compare(x,y) of EQUAL => compares (tms1, tms2) | ord => ord)
  and compares (tms1, tms2) = 
    case (tms1, tms2) of
      ([], []) => EQUAL
    | (tm1::tms1, tm2::tms2) =>
      (case compare (tm1, tm2) of EQUAL => compares(tms1,tms2) | ord => ord)
    | ([],_) => LESS    (* type error? *)
    | (_,[]) => GREATER (* type error? *)

  fun to_string term = 
    case prj term of 
      Atom(x,[]) => x
    | Atom(x,tms) => 
      x ^ "(" ^ String.concatWith ", " (map to_string tms) ^ ")"
 
end
