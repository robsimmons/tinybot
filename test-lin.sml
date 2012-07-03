structure LinTest = struct

  local 
    open Syntax 
  in

  val prog_conflict = 
    [R{name = "r01",
       prem = [Atom("a0",[]),
               Atom("a1",[])],
       conc = [Atom("c1",[])]},
     R{name = "r0",
       prem = [Atom("a0",[])],
       conc = [Atom("a1",[]), Atom("a2",[])]},
     R{name = "r1",
       prem = [Atom("a1",[])],
       conc = [Atom("b1",[])]},
     R{name = "r2",
       prem = [Atom("a2",[])],
       conc = [Atom("b2",[])]}]

  end

  local
    open Term
    val c = fn x => Atom'(x,[])
  in

  val f0 = 
    [Atom'("a0", []),
     Atom'("a0", []),
     Atom'("a0", []),
     Atom'("a0", [])]

  end

  val linpreds = 
     List.foldl SetS.add' SetS.empty ["a0", "a1", "a2", "b1", "b2", "c1"] 

  fun test0 () = Exec.lin_execute linpreds prog_conflict f0

end
