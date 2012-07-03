
structure RandomizeList = struct
  fun randomize list = list (* Totally bogus! *)
end

structure Exec = struct

  open Syntax
  datatype match = 
     datatype Match.match (* M{rule,premise,subst} *)
  datatype rightmatch = 
     datatype Index.rightmatch (* RM{rule,premise,subst,data} *)
  datatype leftmatch = 
     datatype Index.leftmatch (* LM{rule,premise,subst,data} *)
  
  fun lin_execute linear_predicates rules facts = 
    let
      datatype perm = LINEAR of int | PERSISTENT

      val match = Match.match rules
      val index = Index.new rules
      val table: perm TermTable.t = TermTable.table 0
      val queue = Queue.new ()

      (* Lookup a rule by name in the list of rules *)
      fun lookup rule = 
         valOf (List.find (fn (R{name,...}) => name = rule) rules)

      (* Reports the state *)
      val print_state = fn () =>
        let in
          print "\nSUBSTITUTION INDEXING STRUCTURE";
          print "\n===============================\n";
          Index.print_table index;
          print "\nWORK QUEUE";
          print "\n==========\n";
          Queue.print queue;
          print "\nDATABASE";
          print "\n========\n";
          TermTable.app 
             (fn (term, PERSISTENT) => print (Term.to_string term ^ " pers\n")
               | (term, LINEAR 0) => () 
               | (term, LINEAR 1) => 
                   print (Term.to_string term ^ " lin (1 copy)\n")
               | (term, LINEAR n) =>
                   print (Term.to_string term ^ " lin (" ^ Int.toString n ^
                          " copies)\n"))
             table;
          print "\n"
        end 

    
      (* Asserts a fact, adding it to the queue if appropriate *)
      fun assert fact = 
         ignore
            (TermTable.operate table fact
                (* If the key's not in the database, put it in, lin or pers,
                 * and definitely add it to the queue. *)
                (fn () =>
                 let val Term.Atom (a, _) = Term.prj fact
                 in 
                    Queue.insert queue fact; 
                    if SetS.member (linear_predicates, a)
                    then LINEAR 1
                    else PERSISTENT
                 end)

                (fn LINEAR n => LINEAR (n+1)
                  | PERSISTENT => PERSISTENT))

      
      (* Checks whether a fact in the database has available copies *)
      fun available fact = 
         case TermTable.lookup table fact of
            LINEAR 0 => false
          | LINEAR _ => true
          | PERSISTENT => true
         
      (* Decrements the number of available copies of a resource *)
      fun decr fact = 
         ignore
            (TermTable.operate table fact
                (fn () => raise Fail "Decrementing untracked fact?!")
                (fn LINEAR 0 => raise Fail "Decrementing unavailable fact"
                  | LINEAR n => LINEAR (n-1)
                  | PERSISTENT => PERSISTENT))

      (* Increments the number of available copies of a resource *) 
      fun incr fact = 
         ignore
            (TermTable.operate table fact
                (fn () => raise Fail "Incrementing untracked fact?!")
                (fn LINEAR n => LINEAR (n+1)
                  | PERSISTENT => PERSISTENT))

      


      (* Exploring any subtree of a rule's substitution tree leads to
       * applying a rule or failing. *)
      datatype exploration_result =
         Success of Term.term list
       | Failure     


      (* finalize_rule
       * 
       * Called from explore_substitution_tree when we've completely
       * applied the premise of a rule. We've gotten through the
       * substitution tree! But according to the saturating semantics,
       * we can only apply this rule if it derives new facts; we could
       * still fail. *)
      fun finalize_rule (LM {rule, subst, data, ...}) = 
        let 
          val R{prem, conc, ...} = lookup rule
    
          val new_facts = map (Subst.apply subst) conc

          val there_is_some_linear_stuff_in_this_rule = 
             List.exists 
                (fn (Var _) => raise Fail "Rule invariant"
                  | (Atom (a, _)) => SetS.member (linear_predicates, a)) 
                (prem @ conc)

          val all_of_the_premises_are_already_in_the_database = 
             List.all (TermTable.member table) new_facts
        in
          if there_is_some_linear_stuff_in_this_rule
          then 
           (print (Subst.to_string subst ^ " success! (linearity)\n");
            Success new_facts)
          else if all_of_the_premises_are_already_in_the_database
          then 
           (print (Subst.to_string subst ^ " complete, but redundant.\n");
            Failure)
          else 
           (print (Subst.to_string subst ^ " success! (new facts)\n");
            Success new_facts)
        end

      (* explore_substitution_tree
       *
       * Mutually recursive with extend_substitution_tree, initially
       * called from attempt_rule_application. The leftmatch given as
       * an argument represents our current point in the traversal of
       * the substitution tree for this rule, *)
      fun explore_substitution_tree num_prems (leftmatch as LM {premise,...}) = 
         if premise = num_prems 
         then (* Almost done! (unless we've only rederived known facts) *)
           finalize_rule leftmatch 
         else 
           let 
             (* Use the indexing structure to find all the matching facts *)
             val possible_extensions = 
               map (fn rightmatch => Index.advance (leftmatch, rightmatch))
                 (Index.lookup_left (index, leftmatch))

             (* This is more hackish than it should be: the point here
              * is that some of the possible extensions may involve
              * facts that we've actually exhausted (they are linear
              * and there are no copies left) and we want to make sure
              * not to call extend_substitution_tree with those
              * extensions. *)
             val filtered_extensions = 
                List.mapPartial
                   (fn (LM {data = [], ...}) => raise Fail "filter invariant"
                     | (leftmatch as LM {data = fact :: _, ...}) => 
                          if not (available fact) then NONE
                          else SOME (fact, leftmatch))
                   possible_extensions 

             (* Make tree traversal fair *)
             val extensions = RandomizeList.randomize filtered_extensions
           in
             if null extensions
             then (let val LM {subst, premise, ...} = leftmatch 
                   in print (Subst.to_string subst ^ " fails (can't extend \
                             \to premise #" ^ Int.toString (premise+1) ^ ")\n");
                      Failure
                   end)
             else extend_substitution_tree num_prems extensions 
           end

      (* extend_substitution_tree
       *
       * Mutually recursive with explore_substitution_tree. The Index
       * lookup gives us a bunch of possible ways of extending the
       * substitution tree given that we consume a certain fact. This
       * function tries, to explore the individual possibilities
       * (consuming a fact and then calling explore_substitution_tree
       * again). If the sub-exploration fails, we undo the
       * consumption. *)
      and extend_substitution_tree num_prems [] = Failure
        | extend_substitution_tree num_prems ((fact, leftmatch) :: extensions) =
            (decr fact; (* Consume this fact for now. *)
             case explore_substitution_tree num_prems leftmatch of 
                Success new_facts =>
                   Success new_facts (* Fact stays consumed. *)
              | Failure => 
                  (incr fact; (* Undo fact consumption. *)
                   extend_substitution_tree num_prems extensions))
                 
             

      (* Attempt to apply a rule, any rule. *)
      fun attempt_rule_application [] = NONE
        | attempt_rule_application (R{name, prem, ...} :: rules) =
          let 
            val () = 
               print ("\nExploring substitution tree for rule "^name^"\n")
            
            val initial_leftmatch = 
               LM {rule = name, (* Rule's name *)
                   premise = 0, (* We start out having matched 0 premises *)
                   subst = Subst.empty, (* And with an empty substitution *)
                   data = []} (* No facts consumed yet in rule *)
          in
            case explore_substitution_tree (length prem) initial_leftmatch of
               Failure => attempt_rule_application rules
             | Success facts => SOME facts
          end


      (* The innermost loop that consumes things off the workqueue. *)
      fun inner_loop () = 
       (print_state ();
        print "\n(press Enter to take a step)\n";
        ignore (TextIO.inputLine TextIO.stdIn);
        case Queue.pop NONE queue of
           NONE => (* Try to perform a rule application *)
           let
              (* Decide which order to attempt rule application in. *)
              val reordered_rules = RandomizeList.randomize rules
           in
              print ("@@@ Queue empty, attempting to apply some rule!\n");
              case attempt_rule_application reordered_rules of 
                 NONE => print ("\nQUIESCENCE. Done!\n\n")
               | SOME newfacts => (List.app assert newfacts; inner_loop ())
           end

         | SOME fact => (* Add the new fact to the indexing structure *)
           let
              (* We have a fact! Find every place in the program that
               * this fact matches. *)
              val matches = match fact 
              val num_matches = length matches
           in
              print ("@@@ Removed " ^ Term.to_string fact ^ " from queue (" ^
                     (if num_matches = 1 
                      then "1 match"
                      else Int.toString num_matches ^ " matches") ^ ")\n");

              (* Record each match in the indexing structure (as a
               * "right match"). *)
              app (fn (M{rule, premise, subst}) => 
                     Index.insert_right 
                       (index, RM{rule = rule,
                                  premise = premise,
                                  subst = subst,
                                  data = fact}))
                 matches;
              
              (* Then keep going *)
              inner_loop ()
           end)
    in
       List.app assert facts;
       inner_loop ()
    end

    val execute = lin_execute SetS.empty

end