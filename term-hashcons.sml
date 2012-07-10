(* Tinybot internal term representation with hashconsing
 * Robert J. Simmons *)

structure Term :> TERM = struct
 
  type term = int
  type t = term
  datatype term_rep = 
    Atom of string * term list

  val next = ref 0
  val lookup_hash: (term_rep * int) list MapW.map ref = ref MapW.empty
  val lookup_rep: term_rep GrowArray.growarray = GrowArray.empty ()
 
  val prj = GrowArray.sub lookup_rep 

  fun inj (tm as (Atom (x, tms))) = 
  let 
     (* Compute a hash value *)
     fun hashloop (w, []) = w
       | hashloop (w, (i :: is)) = 
            hashloop (JenkinsHash.hashInc w (Word.fromInt i), is) 
     val hash = hashloop (StringHashable.hash x, tms)

     (* Lookup in the hashtable *)
     val existing = 
        case MapW.find (!lookup_hash, hash) of NONE => [] | SOME cand => cand

  in
     case List.find (fn (tm', i) => 
                       if tm = tm' then true
                       else (print ("COLLISION\n"); false)) existing of
        NONE => 
        let val this = !next
        in
         ( next := this + 1
         ; lookup_hash :=
              MapW.insert (!lookup_hash, hash, ((tm, this)::existing))
         ; GrowArray.append lookup_rep tm
         ; this)
        end
      | SOME (_, i) => i
  end

  val Atom' = inj o Atom

  fun eq (i: term, j) = i = j 

  val compare = Int.compare

  fun to_string term = 
    case prj term of 
      Atom(x,[]) => x
    | Atom(x,tms) => 
      x ^ "(" ^ String.concatWith ", " (map to_string tms) ^ ")"
 
end
