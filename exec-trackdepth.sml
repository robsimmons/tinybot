(* Tinybot execution 
 * Robert J. Simmons *)

(* Unified queue/database. Inefficient! *)
structure Facts :> sig
  
  type facts
  val new : Term.term list -> facts
  val insert : facts -> int -> Term.term -> unit 
  val to_string : facts -> string
  val pop : int option -> facts -> (Term.term * int) option
  
end = struct
  
  type facts = ((int option ref * Term.term * int list ref) list * int) ref

  fun listmin [] = valOf (Int.maxInt)
    | listmin (x :: xs) = Int.min (x, listmin xs)

  fun listmax [] = 0
    | listmax (x :: xs) = Int.max (x, listmax xs)

  fun do_new ([], i) = []
    | do_new (tm :: tms, i) = (ref (SOME i), tm, ref [0]) :: do_new (tms, i+1)

  fun new tms = ref (do_new(tms,0), length tms)

  fun do_pop [] = NONE
    | do_pop ((ref NONE, _, _) :: facts) = do_pop facts
    | do_pop ((r as (ref (SOME i)), tm, ref xs) :: facts) =
      (r := NONE; SOME (tm, listmin xs))

  fun pop NONE (ref (facts, max)) = do_pop facts
    | pop (SOME i) (ref (facts, max)) = 
      if i >= max then do_pop facts
      else let val (r as ref x, tm, ref xs) = List.nth (facts, i) in
        case x of 
          NONE => do_pop facts
        | SOME j => (r := NONE; SOME (tm, listmin xs))
      end

  fun insert_test term (_,tm,_) = Term.eq (tm, term)

  fun insert (r as (ref (facts, max : int))) x term =
    case List.find (insert_test term) facts of
      NONE => r := (facts @ [(ref (SOME max), term, ref [x])], max+1)
    | SOME (_,_,xs) => (xs := !xs @ [x])

  fun xs_to_string xs = 
      " [" ^ String.concatWith "," (map Int.toString xs) ^ "]\n"

  fun elem_to_string (ref NONE, tm, ref xs) = 
      "** " ^ Term.to_string tm ^ xs_to_string xs
    | elem_to_string (ref (SOME i), tm, ref xs) =
      (if i < 10 then Int.toString i ^ "  " else Int.toString i ^ " ") ^
      Term.to_string tm ^ xs_to_string xs

  fun to_string (ref (facts, max)) = 
    String.concat (map elem_to_string (rev facts))

end

structure Exec = struct

  open Syntax
  open Match
  open Index

  fun execute rules facts = 
    let
      val match = Match.match rules
      val advance_rl = fn rm => fn lm => advance (lm, rm)
      val advance_lr = fn lm => fn rm => advance (lm, rm)
      val index = new rules
      val prems = map (fn R{prem,...} => length prem) rules
      val facts = Facts.new facts

      val print_state = fn () => 
        let in 
          print_table index;
          print "--\n";
          print (Facts.to_string facts);
          print "\n"
        end

      fun init 0 = ()
        | init n = 
          let val n = n-1 in
            insert_left 
                (index, LM{rule=n, premise=0, subst=Subst.empty, data=[]}); 
            init n
          end

      fun listmax [] = 0
        | listmax (x :: xs) = Int.max (x, listmax xs)

      fun finish conc (LM{subst, data, ...}) =
          app (Facts.insert facts (listmax data + 1) o Subst.apply subst) conc
        
      (* Comes up with all immediate consequences of a specific match. *)
      fun apply depth (M{rule, premise, subst}) =
        let 
          val R{prem, conc} = List.nth (rules, rule)
          val num_premises = length prem
          fun rapply (lm as LM{premise, subst, ...}) = 
            if premise = num_premises then finish conc lm
            else (insert_left (index, lm);
                  app rapply (map (advance_lr lm) (lookup_left (index, lm))))

          val rm = RM{rule=rule, premise=premise, subst=subst, data=depth}
        in
          insert_right (index, rm);
          app rapply (map (advance_rl rm) (lookup_right (index, rm)))
        end

      fun loop () = 
        case Facts.pop 
                 (Option.composePartial (Int.fromString,TextIO.inputLine)
                      TextIO.stdIn)
                 facts of
          NONE => print "Done!\n"
        | SOME (fact, depth) => 
          let 
            val matches = match fact
          in
            app (apply depth) matches;
            print_state ();
            loop ()
          end

    in
      init (length rules);
      print_state ();
      loop ()
    end

end 
