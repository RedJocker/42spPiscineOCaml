
(*
  Turn-in directory : ex07/
Files to turn in : converges.ml
Allowed functions : None
You will write a function named converges which takes the same arguments as iter
but returns true if the function reaches a fixed point in the number of iterations given
to converges and false otherwise. Its type will be (’a -> ’a) -> ’a -> int -> bool.
A fixed point is an x for which x = f (x). For example, let’s say f (x) = x2. f (1) =
12 = 1, which means f has a fixed point at 1. If you want more information, well, look
it up.
Example:
# converges (( * ) 2) 2 5;;
- : bool = false
# converges (fun x -> x / 2) 2 3;;
- : bool = true
# converges (fun x -> x / 2) 2 2;;
- : bool = true
What is ’a? Wait for Victor to explain that to you tomorrow.
Remember to pronounce “alpha”, not “quote a”. Nobody would
understand, and you would sound stupid.
20
OCaml Recursion and higher-order functions - 0
  This exercise is not mandatory
  *)

let converges f x n =
  let rec loop i acc = match (i, f acc) with
  | (i, _) when i <= 0 -> false
  | (i, res) -> acc == res || (loop (i - 1) res)
  in
  loop n x


(* TODO TEST

   utop[63]> Seq.ints 1 |> Seq.take 10 |> Seq.map (converges (fun x -> (x / 3) * (x / 4)) 10)|> List.of_seq;;

- : bool list =
   [false; false; false; true; true; true; true; true; true; true]

   *)
