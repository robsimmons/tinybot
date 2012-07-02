(* Tinybot grounding substitutions 
 * Robert J. Simmons 
 * Essentially just maps from strings to terms  *)

signature SUBST = sig

  type subst

  (* Empty substitution *)
  val empty : subst

  (* Returns true iff the substitution has a given member *)
  val member : subst -> string -> bool

  (* Extends a substitution with a given binding *)
  val extend : subst -> string * Term.term -> subst

  (* Find a term in a substitution *)
  val find : subst -> string -> Term.term option

  (* Apply a grounding substitution to a term with free variables. *)
  val apply : subst -> Syntax.term -> Term.term

  (* Merges two substitutions (must be identical on the intersection!) *)
  val merge : subst * subst -> subst

  (* Takes only elements of the substitution that appear in a given set.
   * filter ({"X" ↦ a, "Y" ↦ b, "Z" ↦ c},{"X","W","Q"}) = {"X" ↦ a}   *)
  val filter : SetS.set -> subst -> subst 
 
  val compare : subst * subst -> order
  val to_string : subst -> string

end

structure Subst :> SUBST = struct

  open Syntax

  type subst = Term.term MapS.map

  fun to_string subst = 
    let 
      fun elem_to_str (x, tm) = (x ^ "=" ^ Term.to_string tm)
      val elems = map elem_to_str (MapS.listItemsi subst)
    in
      "{" ^ String.concatWith ", " elems ^ "}"
    end

  val empty = MapS.empty
  val member = fn subst => fn x => MapS.inDomain(subst, x)
  val extend = fn subst => fn (x, tm) => MapS.insert(subst, x, tm)
  val find = fn subst => fn x => MapS.find(subst, x)
  val merge =
    MapS.unionWith 
        (fn (tm1, tm2) => 
            if Term.eq(tm1, tm2) then tm1 
            else raise Fail "Invariant")

  fun apply subst term = 
    case term of 
      Var x =>
      (MapS.lookup(subst, x)
       handle exn => 
         (print ("Could not find " ^ x ^ " in " ^ to_string subst ^ "\n");
          raise exn))
    | Atom(x, tms) => Term.Atom'(x, map (apply subst) tms)

  fun filter set = MapS.filteri (fn (x,tm) => SetS.member(set,x)) 

  val compare = MapS.collate Term.compare


end

