(* Tinybot indexing 
 * Robert J. Simmons *)

signature INDEX = sig

  type 'a table 

  datatype 'a rightmatch = 
      RM of {rule : string, 
             premise : int,
             subst : Subst.subst, 
             data : 'a}

  datatype 'a leftmatch = 
      LM of {rule : string, 
             premise : int,
             subst : Subst.subst, 
             data : 'a list}  

  val new : Syntax.rule list -> 'a table

  (* Lookup all the matching {left, right} matches for a {right, left} match *)
  val lookup_right : 'a table * 'a rightmatch -> 'a leftmatch list
  val lookup_left  : 'a table * 'a leftmatch  -> 'a rightmatch list

  (* Insert a new match into the table *)
  val insert_right : 'a table * 'a rightmatch -> unit
  val insert_left  : 'a table * 'a leftmatch  -> unit

  (* Advance - takes a leftmatch and a rightmatch for the nth premise 
   * of a rule and returns a leftmatch for the (n+1)th premise of that rule. *)
  val advance : 'a leftmatch * 'a rightmatch -> 'a leftmatch
 
  val lm_to_string : 'a leftmatch -> string
  val rm_to_string : 'a rightmatch -> string
  val print_table : 'a table -> unit

end

structure Index :> INDEX = struct

  open Syntax
  open Subst

  datatype 'a rightmatch = 
      RM of {rule : string, 
             premise : int,
             subst : Subst.subst, 
             data : 'a}

  datatype 'a leftmatch = 
      LM of {rule : string, 
             premise : int,
             subst : Subst.subst, 
             data : 'a list}  

  datatype key = K of {rule : string, premise : int, subst : subst}
  fun compare (K{rule, premise, subst}, K{rule=r2, premise=p2, subst=s2}) =
    case (String.compare (rule, r2), Int.compare (premise, p2)) of
      (EQUAL, EQUAL) => Subst.compare (subst, s2)
    | (EQUAL, ord) => ord
    | (ord, _) => ord
    
  fun key_to_string (K{rule, premise, subst}) =
    rule ^ "-" ^ Int.toString premise ^ " " ^ 
    Subst.to_string subst

  fun rm_to_string (RM{rule, premise, subst, ...}) =
    rule ^ "-" ^ Int.toString premise ^ " " ^ 
    Subst.to_string subst

  fun lm_to_string (LM{rule, premise, subst, ...}) =
    rule ^ "-" ^ Int.toString premise ^ " " ^ 
    Subst.to_string subst
            

  structure MapK = 
  RedBlackMapFn(struct type ord_key = key val compare = compare end)

  fun mapfind (map, key) =
    case MapK.find (map, key) of SOME x => x | NONE => []

  datatype 'a table = 
      T of {substs : SetS.set vector MapS.map, 
            left : ('a rightmatch list) MapK.map ref,
            right : ('a leftmatch list) MapK.map ref}

  (* Generation of index substitutions *)
 
  fun generate_indices (_,[]) = []
    | generate_indices (fvs, prem :: prems) = 
      let val fv = freevars prem in
        SetS.intersection (fv, fvs) :: 
        generate_indices (SetS.union (fv, fvs), prems)
      end

  fun generate_substs [] = MapS.empty
    | generate_substs (R{name, prem, conc} :: rules) =
      MapS.insert (generate_substs rules, name,  
                   generate_indices (SetS.empty, prem))

  (* Turn a MapS of lists into a MapS of vectors *)
  fun vectorify map = MapS.map Vector.fromList map

  fun print_substs substs = 
    let 
      fun to_list vec = 
        List.tabulate (Vector.length vec, (fn i => Vector.sub (vec, i)))

      fun print2 (i, j, []) = ()
        | print2 (i, j, set::sets) = 
          let in
            print (Int.toString i ^ "-" ^ Int.toString j ^ ": ");
            print (String.concatWith ", " (SetS.listItems set) ^ "\n");
            print2 (i, j+1, sets)
          end
                              
      fun print1 (i, []) = ()
        | print1 (i, rule::rules) = 
          let in 
            print2 (i, 0, to_list rule); 
            print1 (i+1, rules)
          end
    in print1 (0, to_list substs) end

  fun print_table (T{left = ref left, right = ref right, ...}) = 
    let in
      print "Left -> right index:\n";
      MapK.appi
         (fn (k,rms) => 
             (print (key_to_string k ^ "\n");
              List.app (fn rm => print ("  " ^ rm_to_string rm ^ "\n")) rms))
         left;
      print "Right -> left index:\n";
      MapK.appi
         (fn (k,lms) => 
             (print (key_to_string k ^ "\n");
              List.app (fn lm => print ("  " ^ lm_to_string lm ^ "\n")) lms))
         right
    end

  fun new rules = 
    T{substs = vectorify (generate_substs rules), 
      left = ref MapK.empty, 
      right = ref MapK.empty}

  (* Strips out the non-index-relevant parts of a substitution *)
  fun filter substs (rule, premise, subst) = 
    let in
      Subst.filter (Vector.sub (MapS.lookup (substs, rule), premise)) subst
    end

  fun key_left (substs, LM{rule, premise, subst, data}) = 
    let in
      (* print ("KL "^Int.toString rule^"-"^Int.toString premise^"\n"); *)
      K{rule = rule, premise = premise, 
        subst = filter substs (rule, premise, subst)}
    end

  fun key_right (substs, RM{rule, premise, subst, data}) = 
    let in
      (* print ("KR "^Int.toString rule^"-"^Int.toString premise^"\n"); *)
      K{rule = rule, premise = premise, 
        subst = filter substs (rule, premise, subst)}
    end

  fun lookup_right (T{substs, right, ...}, rm) = 
    let in
      (* print "Look "; *)
      mapfind (!right, key_right (substs, rm))
    end

  fun lookup_left  (T{substs, left, ...}, lm) = 
    let in
      (* print "Look "; *)
      mapfind (!left, key_left (substs, lm))
    end

  fun insert_right (T{substs, left, ...}, rm) = 
    let 
      (* val _ = print "Ins  " *)
      val key = key_right (substs, rm) 
    in
      left := MapK.insert (!left, key, rm :: mapfind (!left, key)) 
    end

  fun insert_left (T{substs, right, ...}, lm) = 
    let 
      (* val _ = print "Ins  " *)
      val key = key_left (substs, lm) 
    in
      right := MapK.insert (!right, key, lm :: mapfind (!right, key)) 
    end

  fun advance (LM{rule = rL, premise = pL, subst = sL, data = dL},
               RM{rule = rR, premise = pR, subst = sR, data = dR}) = 
      let 
        val _ = Global.assert (fn () => rL = rR andalso pL = pR)
        val subst = Subst.merge (sL, sR)
        val data = dR :: dL (* Warning! This is backwards, I guess *)
      in LM{rule = rL, premise = pL + 1, subst = subst, data = data} end


end
