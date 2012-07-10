(* Author: Carsten Schuermann *)
(* Systematic and regular test signature generation for tinybot *)

structure GenTest = struct

  local 
    open Syntax 
  in

  fun genidx 0 = [0]
    | genidx n = (genidx (n-1)) @ [n]

  fun zip (nil, L2) = nil
    | zip (L1, nil) = nil
    | zip (H1 :: L1, H2 :: L2) = (H1, H2) :: zip (L1, L2)

  fun mkConst (c, n) = c ^ Int.toString n
  fun mkAtom c = Atom (c, [])
  fun mkAtom' c = Term.Atom' (c, [])
  fun linpreds list = List.foldl SetS.add' SetS.empty list

  fun test b n =
    let 
       val idx = genidx n
       val cs = map (fn i => mkConst ("a", i)) idx
       val pairs = zip (cs, tl cs)
       val prog = map (fn (c1, c2) => 
           R {name = c1 ^ "->" ^ c2,
	      prem = [mkAtom c1], 
	      conc = [mkAtom c2]}) pairs
       val ctx = [mkAtom' (List.hd cs)]
    in	 
      (Timers.time Timers.solving(fn () => Exec.lin_execute b (linpreds cs) prog ctx) (); Timers.show ())
    end

  fun test1 b n =
    let 
       val idx = genidx n
       val aas = map (fn i => mkConst ("a", i)) idx
       val bs = map (fn i => mkConst ("b", i)) idx
       val cs = zip (aas, bs)
       val pairs = zip (cs, tl cs)
       val prog = map (fn ((a1, b1), (a2, b2)) => 
           R {name = a1 ^ "*" ^ b1 ^ "->" ^ a2 ^ "*" ^ b2,
	      prem = [mkAtom a1, mkAtom b1], 
	      conc = [mkAtom a2, mkAtom b2]}) pairs
       val (x, y) = List.hd cs
       val ctx = [mkAtom' x, mkAtom' y]
    in	 
      (Timers.time Timers.solving(fn () => Exec.lin_execute b (linpreds (aas @ bs)) prog ctx) (); Timers.show ())
    end


  fun test2 b n =
    let
       val prog =  [R{name = "rule",
                prem = [Atom("a",[Atom ("s", [Var "X"])])],
       		conc = [Atom("a",[Var "X"])]}]
 
       fun intToTerm 0 = mkAtom' "z"
         | intToTerm n = Term.Atom' ("s", [intToTerm (n-1)])
    
       
       val ctx = [ Term.Atom' ("a", [intToTerm n])]
    in	 
      (Timers.time Timers.solving(fn () => Exec.lin_execute b (linpreds ["a"]) prog ctx) (); Timers.show ())
    end


  fun test3 b n =
    let
       val prog =  [R{name = "rule",
                prem = [Atom("a",[Var "X"]), Atom ("b", [Var "Y"])],
       		conc = [Atom("c",[Var "X", Var "Y"])]}]
 
       fun intToTerm 0 = mkAtom' "z"
         | intToTerm n = Term.Atom' ("s", [intToTerm (n-1)])
    
       val idx = genidx n
       
       val ts = map (fn i => mkConst ("t", i)) idx
       val ctx = foldr (fn (t, l) => (Term.Atom' ("a", [Term.Atom' (t, nil)]))
       	       	      :: (Term.Atom' ("b", [Term.Atom' (t, nil)])) :: l) nil ts
    in	 
      (Timers.time Timers.solving(fn () => Exec.lin_execute b (linpreds ["a", "b", "c"]) prog ctx) (); Timers.show ())
    end




    fun	run f 0 n = ()
      | run f i n =
    	(print ("n = " ^ (Int.toString n)); f false n; run f (i-1) (n*2))
    	
      
  end 
end 
