(* Attempt to provide a hacky set data structure that
 *
 * 1) Allows fair-enough traversal of entries
 * 2) Allows fast-enough invalidation of entries *)

signature FAIR_TREE = 
sig
   type 'a fairtree
   type 'a t = 'a fairtree
   val empty: 'a fairtree
   val insert: 'a fairtree -> ('a * bool ref) -> 'a fairtree
   val traverse: 'a fairtree -> 'a Stream.stream
   val app: ('a * bool * int -> unit) -> 'a fairtree -> unit
end

structure FairTree:> FAIR_TREE =
struct
   local val randgen: (unit -> word) option ref = ref NONE
   in

   fun flip (a, b) = 
   let 
      val r = case !randgen of 
         NONE => 
         let val r = Rand.mkRandom
                        (Word.fromLargeInt (Time.toMilliseconds (Time.now ())))
         in randgen := SOME r; r end
       | SOME r => r
   in
      if 0 = Rand.range (0,1) (valOf(!randgen) ())
      then (a, b)
      else (b, a)
   end

   end

   datatype 'a ft = 
      Empty
    | Singleton of 'a * bool ref 
    | Node of int * 'a ft ref * int * 'a ft ref 

   (* The real_mindepth is the minimum depth at which data is stored *)
   fun real_mindepth ft =
      case !ft of
         Empty => 0
       | Singleton _ => 1
       | Node (_, ft1, _, ft2) => 
            Int.min (real_mindepth ft1, real_mindepth ft2) + 1
   
   (* A tree is well-formed if the annotations overapproximate the minimum 
    * depth *)
   fun well_formed ft = 
      case !ft of
         Empty => true
       | Singleton _ => true
       | Node (d1, ft1, d2, ft2) => 
            well_formed ft1 
            andalso well_formed ft2
            andalso d1 >= real_mindepth ft1
            andalso d2 >= real_mindepth ft2

   fun app depth f ft = 
      case !ft of 
         Empty => ()
       | Singleton (x, r) => f (x, !r, depth)
       | Node (_, ft1, _, ft2) => (app (depth+1) f ft1; app (depth+1) f ft2)

   val app = 
      fn f => (fn NONE => ()
                | SOME ft => app 1 f ft)

   type 'a fairtree = 'a ft ref option
   type 'a t = 'a fairtree

   fun traverse ft (cont: (int * 'a ft) option -> 'a Stream.front) =
      case !ft of
         Empty => cont NONE
       | Singleton (x, r as ref false) => cont NONE
       | Singleton (x, r as ref true) =>
            Stream.Cons (x, 
               Stream.lazy (fn () => cont (SOME (1, Singleton (x, r)))))
       | Node (depth1, ft1, depth2, ft2) => 
         let val ((da, fta), (db, ftb)) = flip ((depth1, ft1), (depth2, ft2))
         in traverse fta 
               (fn NONE => (fta := Empty; traverse ftb cont)
                 | SOME (da', fta') => 
                    ( fta := fta'
                    ; traverse ftb
                         (fn NONE => cont (SOME (da', fta'))
                           | SOME (db', ftb') => 
                              ( ftb := ftb'
                              ; cont (SOME (Int.min (da', db') + 1, 
                                            Node (da, fta, db, ftb)))))))
         end

   (* Requires: depth = real_mindepth tree > 0 *)
   (* Ensures: #1 \result >= real_mindepth (#1 \result) *)
   fun insert ft (depth, tree) =
      case !ft of 
         Empty => (depth, tree)
       | Singleton (_, ref false) => (depth, tree)
       | Singleton _ => (2, ref (Node (1, ft, depth, tree)))
       | Node (depth1, ft1, depth2, ft2) =>
         if depth1 <= depth2 
         then let val (depth1', ft1') = insert ft1 (depth, tree)
              in (Int.min (depth1', depth2) + 1,
                  ref (Node (depth1', ft1', depth2, ft2)))
              end
         else let val (depth2', ft2') = insert ft2 (depth, tree)
              in (Int.min (depth1, depth2') + 1,
                  ref (Node (depth1, ft1, depth2', ft2')))
              end

   val empty = NONE

   val insert =
      fn NONE => (fn data => (SOME (ref (Singleton data))))
       | SOME ft => (fn data => SOME (#2 (insert ft (1, ref (Singleton data)))))

   val traverse = 
      fn NONE => Stream.eager Stream.Nil
       | SOME ft => 
            Stream.lazy
               (fn () => traverse ft 
                            (fn NONE => (ft := Empty; Stream.Nil)
                              | SOME (_, ft') => (ft := ft'; Stream.Nil)))

(*

   (* When we  *)
   fun remove_one (ft: 'a fairtree) = 
      case #1 (whnf ft) of 
         Empty => NONE
       | Singleton (x, r) => SOME (x, r, 0, ref Empty)
       | Node (depth1, ft1, depth2, ft2) =>
         let
            val (small, ftsmall, big, ftbig) =
               if depth1 < depth2 
               then (depth1, ft1, depth2, ft2) 
               else (depth2, ft2, depth1, ft1)
         in case remove_one ftbig of
               NONE => remove_one ftsmall
             | SOME (x, r, depth, ft) => 
                 SOME (x, r, Int.max (small, depth) + 1, 
                       ref (Node (depth, ft, small, ftsmall)))
         end

   fun whnf (ft: 'a fairtree) =  
      case ft of 
         (ref Leaf) => (Leaf, 0)
       | (ref (Node (x, ref true, ft1, n1, ft2, n2))) => (!ft, n1+n2+1)
       | (ref (Node (x, ref false, ft1, n1, ft2, n2))) =>
         let 
            val newft = 
               if n1 < n2 
               then (case whnf ft2 of 
                        Leaf => whnf f1
                      | Node (x2, r2, ft21, n21, f22, n22) => 
                           
               else (case whnf ft1 of
                        Leaf => whnf f2
                      | Node (x1, r1, ft11, n11, f12, n12) =>
         in
          ( ft := newft
          ; 
         end


   fun size ft = 
      case ft of 
         Leaf => 0
       | Node (x, ref false, ft1, n1, ft2, n2) => n1+n2+1

   fun insert ft (x, ref false) = (ft, size ft)
     | insert ft (x, r) = 
         (case ft of 
             Leaf => (Node (x, r, Leaf, 0, Leaf, 0), 1)
           | Node (_, r' as ref false, ft1, n1, ft2, n2) =>
                Node (x, r, ft1, n1, ft2, n2)
           | Node (x', r' as ref true, ft1, n1, ft2, n2) =>
               (if n1 < n2
                then let (ft1', n1') = insert ft1 (x, r) 
                     in Node (x', r', ft1', n1', ft2, n2)
                     end
                else let (ft2', n2') = insert ft2 (x, r) 
                     in Node (x', r', ft1, n1, ft2', n2') 
                     end))
*)
end
