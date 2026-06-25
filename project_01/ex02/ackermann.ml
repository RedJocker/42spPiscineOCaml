(*
Exercise 02
A tribute to a person named Ackermann. Not the one from Attack on Titan, nor the
greatest hacker of all time, sadly.
Turn-in directory : ex02/
Files to turn in : ackermann.ml
Allowed functions : None
You will write a function named ackermann, which will be an implementation of the
Ackermann function. The Ackermann function is defined as follows:
A(m, n) =
| n + 1                 if m = 0
| A(m − 1, 1)		if m > 0 and n = 0
| A(m − 1, A(m, n − 1)) if m > 0 and n > 0
If any argument given to the function is negative, the function must return -1. Ob-
viously, your function’s type will be int -> int -> int. Don’t forget to look up who
Wilhelm Ackermann is and why this function is important in the history of computer
science.
Example:
# ackermann (-1) 7;;
- : int = -1
# ackermann 0 0;;
- : int = 1
# ackermann 2 3;;
- : int = 9
# ackermann 4 1;; (* This may take a while. Don’t worry. *)
- : int = 65533
10

This function is very heavy to compute and will cause a stack
overflow if given unreasonable input. Remember, this is expected

  *)


let rec ackermann m n = match (m, n) with
| m, n when m < 0 || n < 0 -> ~-1
| 0, n -> n + 1
| m, 0 -> ackermann (m - 1) 1
| m, n -> ackermann (m - 1) (ackermann m (n - 1))

let () =
  let assertEquals tested expected actual =
    let (m, n) = tested in 
    let case = Printf.sprintf "(%d, %d)" m n in
    Printf.printf "TestCase %s: " case;
  if expected <> actual then
    Printf.printf "[FAIL]\nexpected:%d\nactual:%d\n" expected actual
  else
    Printf.printf "[OK]\n"
  in

  let tested = (~-1, 0) in
  let (m, n) = tested in
  let expected = ~-1 in
  let actual =  ackermann m n in
  assertEquals tested expected actual;

  let tested = (~-10, 0) in
  let (m, n) = tested in
  let expected = ~-1 in
  let actual =  ackermann m n in
  assertEquals tested expected actual;

  let tested = (0, ~-1) in
  let (m, n) = tested in
  let expected = ~-1 in
  let actual =  ackermann m n in
  assertEquals tested expected actual;

  let tested = (0, ~-50) in
  let (m, n) = tested in
  let expected = ~-1 in
  let actual =  ackermann m n in
  assertEquals tested expected actual;

  let tested = (2, 3) in
  let (m, n) = tested in
  let expected = 9 in
  let actual =  ackermann m n in
  assertEquals tested expected actual;

  let tested = (4, 1) in
  let (m, n) = tested in
  let expected = 65533 in
  let actual =  ackermann m n in
  assertEquals tested expected actual
