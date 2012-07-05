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

  val prog_spantree = 
    [R{name = "r/symm",
       prem = [Atom("edge",[Var "X", Var "Y"])],
       conc = [Atom("edge",[Var "Y", Var "X"])]},
     R{name = "r/start",
       prem = [Atom("vertex",[Atom("a",[])])],
       conc = [Atom("intree",[Atom("a",[])])]},
     R{name = "r/extend",
       prem = [Atom("edge",[Var "X", Var "Y"]),
               Atom("intree",[Var "X"]),
               Atom("vertex",[Var "Y"])],
       conc = [Atom("intree",[Var "Y"]),
               Atom("tree",[Var "X", Var "Y"])]}]

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

  val edge1 =
    [Atom'("edge", [Atom'("a",[]), Atom'("b",[])]),
     Atom'("edge", [Atom'("a",[]), Atom'("c",[])]),
     Atom'("edge", [Atom'("a",[]), Atom'("d",[])]),
     Atom'("edge", [Atom'("b",[]), Atom'("c",[])]),
     Atom'("edge", [Atom'("b",[]), Atom'("e",[])]),
     Atom'("edge", [Atom'("d",[]), Atom'("e",[])]),
     Atom'("vertex", [Atom'("a",[])]),
     Atom'("vertex", [Atom'("b",[])]),
     Atom'("vertex", [Atom'("c",[])]),
     Atom'("vertex", [Atom'("d",[])]),
     Atom'("vertex", [Atom'("e",[])])]
  end

  fun linpreds list = List.foldl SetS.add' SetS.empty list

  fun test0 b = 
     Exec.lin_execute b (linpreds ["a0", "a1", "a2", "b1", "b2", "c1"]) 
       prog_conflict f0

  fun test1 b = 
     Exec.lin_execute b (linpreds ["vertex"])
       prog_spantree edge1
end
