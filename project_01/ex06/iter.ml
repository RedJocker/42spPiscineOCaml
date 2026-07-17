
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


let () =
  let assertEquals case expected actual =
    Printf.printf "TestCase %s: " case;
  if expected <> actual then
    Printf.printf "[FAIL]\nexpected:%d\nactual:%d\n" expected actual
  else
    Printf.printf "[OK]\n"
  in

  print_endline "===========";
  print_endline "Test iter:";


  let tested = ((fun x -> x * x), 2, 4) in
  let (f, x, n) = tested in
  let expected = 65536 in
  let actual = iter f x n in
  assertEquals "((fun x -> x * x), 2, 4) = 65536" expected actual;

  let tested = ((fun x -> x * x), 42, 0) in
  let (f, x, n) = tested in
  let expected = 42 in
  let actual = iter f x n in
  assertEquals "((fun x -> x * x), 42, 0) = 42" expected actual;

  let tested = ((fun x -> x * x), 42, 1) in
  let (f, x, n) = tested in
  let expected = 1764 in
  let actual = iter f x n in
  assertEquals "((fun x -> x * x), 42, 0) = 1764" expected actual;

  let tested = ((fun x -> x * x), 100, ~-1) in
  let (f, x, n) = tested in
  let expected = ~-1 in
  let actual = iter f x n in
  assertEquals "((fun x -> x * x), 100, ~-1) = -1" expected actual;

  let tested = ((fun x -> x + 1), 0, 100) in
  let (f, x, n) = tested in
  let expected = 100 in
  let actual = iter f x n in
  assertEquals "((fun x -> x + 1), 0, 100) = 100" expected actual;

  let tested = ((fun x -> x + 2), 0, 100) in
  let (f, x, n) = tested in
  let expected = 200 in
  let actual = iter f x n in
  assertEquals "((fun x -> x + 2), 0, 100) = 200" expected actual;
  
  let tested = ((fun x -> x * 2), 2, 4) in
  let (f, x, n) = tested in
  let expected = 32 in
  let actual = iter f x n in
  assertEquals "((fun x -> x * 2), 2, 4) = 32" expected actual;

