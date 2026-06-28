(*


A tribute to Fibonacci. Not the pasta, sadly.
Turn-in directory : ex04/
Files to turn in : fibonacci.ml
Allowed functions : None
You will write a function named fibonacci, which will be an implementation of
the Fibonacci sequence. However, your implementation will be tail-recursive. No tail
recursion, no points. The Fibonacci sequence is defined as follows:
F (n) =
| 0 if n = 0
| 1 if n = 1
| F (n − 2) + F (n − 1) if n > 1
  If given a negative argument, your function will return -1.

Obviously, your function’s
type will be int -> int. It’s okay not to know about Fibonacci, but you should at least
look up why this sequence is important in history, mathematics, and art, and what kind
of number it generates... and what rabbits have to do with all that. Also, don’t forget
to watch some Raving Rabbids for a few minutes. That might help you heal your poor
brain, badly wounded by imperative languages.

Example:
# fibonacci (-42);;
- : int = -1
# fibonacci 1;;
- : int = 1
# fibonacci 3;;
- : int = 2
# fibonacci 6;;
- : int = 8

  Remember that if your function uses more than one top-level let
definition, you will get no points for this exercise. Also, the
function must be tail-recursive, and it must run in linear time. If
  your exercise doesn’t comply with these restrictions, it’s a failure.

  *)

let fibonacci n = 
  let rec loop i prev_prev prev =
    match i with
    | i when i > n -> prev
    | i -> loop (i + 1) prev (prev + prev_prev)
  in
  match n with
  | n when n < 0 -> ~-1
  | n when n < 2 -> n
  | n -> loop 2 0 1


let () =
  let assertEquals tested expected actual =
    let n = tested in
    let case = Printf.sprintf "(%d)" n in
    Printf.printf "TestCase %s: " case;
  if expected <> actual then
    Printf.printf "[FAIL]\nexpected:%d\nactual:%d\n" expected actual
  else
    Printf.printf "[OK]\n"
  in

  let expected = [~-1; 0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55] in
  let tested = Seq.ints ~-1 |> Seq.take (List.length expected) in
  Seq.iteri (fun i tested_i -> assertEquals tested_i (List.nth expected i) (fibonacci tested_i)) tested 

