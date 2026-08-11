(*

Exercise 02
Exercise 02: Projections
Turn-in directory : ex02/
Files to turn in : ex02.ml
Allowed functions : Stdlib.fst and Stdlb.snd
Enough with OCaml’s STD lib, you got it now. It’s time to create your first own
functor. Your first two functors actually. . . The goal of this exercice is to write the two
functors MakeFst and MakeSnd and their signature MAKEPROJECTION to allow the following
code to compile:
module type PAIR = sig val pair : (int * int) end
module type VAL = sig val x : int end
(* FIX ME !!! *)
module Pair : PAIR = struct let pair = ( 21, 42 ) end
module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)
let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x
And to output:
$> ocamlopt ex02.ml && ./a.out
Fst.x = 21, Snd.x = 42
  $>

  *)


module type PAIR = sig val pair : (int * int) end
module type VAL = sig val x : int end

(* exercise *) 
module MakeFst = functor (M : PAIR) -> struct
  let x = Stdlib.fst M.pair
end

module MakeSnd = functor (M : PAIR) -> struct
  let x = Stdlib.snd M.pair
end
(* exercise *)

module Pair : PAIR = struct let pair = ( 21, 42 ) end
module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)
let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x
