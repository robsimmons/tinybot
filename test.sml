structure Test = struct

  local 
    open Syntax 
  in

  val prog_edge = 
    [R{prem = [Atom("edge",[Var "X", Var "Y"])],
       conc = [Atom("path",[Var "X", Var "Y"])]},
     R{prem = [Atom("edge",[Var "X", Var "Y"]),
               Atom("path",[Var "Y", Var "Z"])],
       conc = [Atom("path",[Var "X", Var "Z"])]}]

  val prog_edge_symm = 
    [R{prem = [Atom("edge",[Var "X", Var "Y"])],
       conc = [Atom("edge",[Var "Y", Var "X"])]},
     R{prem = [Atom("edge",[Var "X", Var "Y"])],
       conc = [Atom("path",[Var "X", Var "Y"])]},
     R{prem = [Atom("edge",[Var "X", Var "Y"]),
               Atom("path",[Var "Y", Var "Z"])],
       conc = [Atom("path",[Var "X", Var "Z"])]}]
   
  val prog_path = 
    [R{prem = [Atom("path",[Var "X", Var "Y"])],
       conc = [Atom("path",[Var "Y", Var "X"])]},
     R{prem = [Atom("path",[Var "X", Var "Y"]),
               Atom("path",[Var "Y", Var "Z"])],
       conc = [Atom("path",[Var "X", Var "Z"])]}]
   
  val prog_cky = 
    [R{prem = [Atom("unary",[Var "X", Var "C"]),
               Atom("char",[Var "C", Var "I"])],
       conc = [Atom("string",[Var "X", Var "I", Atom("s",[Var "I"])])]},
     R{prem = [Atom("binary",[Var "X", Var "Y", Var "Z"]),
               Atom("string",[Var "Y", Var "I", Var "J"]),
               Atom("string",[Var "Z", Var "J", Var "K"])],
       conc = [Atom("string",[Var "X", Var "I", Var "K"])]}]

  end

  local
    open Term
    val c = fn x => Atom'(x,[])
  in

  val f0 = 
    [Atom'("edge", [c "6", c "7"]),
     Atom'("edge", [c "5", c "6"]),
     Atom'("edge", [c "4", c "5"]),
     Atom'("edge", [c "3", c "4"]),
     Atom'("edge", [c "2", c "3"]),
     Atom'("edge", [c "1", c "2"])]

  val f1 = 
    [Atom'("edge", [c "a", c "b"]),
     Atom'("edge", [c "b", c "c"]),
     Atom'("edge", [c "c", c "d"]),
     Atom'("edge", [c "d", c "e"])]
     
  val f2 = 
    [Atom'("path", [c "a", c "b"]),
     Atom'("path", [c "b", c "c"]),
     Atom'("path", [c "c", c "d"]),
     Atom'("path", [c "d", c "e"])]

  val f3 = 
    [Atom'("path", [c "a", c "b"]),
     Atom'("path", [c "b", c "c"])]

  val f4 = 
    [Atom'("path", [c "a", c "b"]),
     Atom'("path", [c "b", c "c"]),
     Atom'("path", [c "a", c "c"])]

  end

  fun test0 () = Exec.execute prog_edge f0
  fun test1 () = Exec.execute prog_edge f1
  fun test2 () = Exec.execute prog_edge_symm f1
  fun test3 () = Exec.execute prog_path f2
  fun test4 () = Exec.execute prog_path f3
  fun test5 () = Exec.execute prog_path f4

end
