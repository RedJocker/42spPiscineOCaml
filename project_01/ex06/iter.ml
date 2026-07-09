
(*

Turn-in directory : ex06/
Files to turn in : iter.ml
Allowed functions : None
You will write a function that takes three arguments: a function of type int -> int,
a start argument, and a number of iterations. This function is really simple:
iter(f, x, n) =
| x if n = 0
| f (x) if n = 1
| f (f (x)) if n = 2
...and so on.
This time I’m not giving you the exact function definition; I could, but then the ex-
ercise would be too easy, and you haven’t been thinking hard enough today.
If n is negative, your function will return -1. Its type will be (int -> int) -> int
-> int -> int.
Example:
# iter (fun x -> x * x) 2 4;;
- : int = 65536
# iter (fun x -> x * 2) 2 4;;
- : int = 32
18
OCaml Recursion and higher-order functions - 0
  This exercise is not mandatory

  *)


let iter f x n =
  let rec loop i acc =
    match i with
    | i when i < 0 -> ~-1
    | 0 -> acc
    | i -> loop (i - 1) (f acc)
  in
  loop n x


(* TODO test *)
