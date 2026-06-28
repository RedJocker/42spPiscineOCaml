(*

A tribute to Mr. Takeuchi. Not the one from Type-MOON, sadly.
Turn-in directory : ex03/
Files to turn in : tak.ml
Allowed functions : None
You will write a function named tak, which will be an implementation of the Tak
function.
The Tak function is defined as follows:
tak(x, y, z) =
{
| tak(tak(x − 1, y, z), tak(y − 1, z, x), tak(z − 1, x, y)) if y < x
| z otherwise
Obviously, your function’s type will be int -> int -> int -> int. Don’t forget
to look up who Takeuchi Ikuo is and why this function is important in the history of
computer science.
There are different definitions of the Tak function because it
has evolved over the years. Note that the definition given in the
subject is the only implementation that will be deemed correct.
12
OCaml Recursion and higher-order functions - 0
Example:
# tak 1 2 3;;
- : int = 3
# tak 5 23 7;;
- : int = 7
# tak 9 1 0;;
- : int = 1
# tak 1 1 1;;
- : int = 1
# tak 0 42 0;;
- : int = 0
# tak 23498 98734 98776;;
- : int = 98776

  *)

let rec tak x y z = match (x, y, z) with
| (x, y, z) when y < x ->
  let x' = tak (x - 1) y z in
  let y' = tak (y - 1) z x in
  let z' = tak (z - 1) x y in
  tak x' y' z'
| (_, _, z) -> z


let () =
  let assertEquals tested expected actual =
    let (x, y, z) = tested in 
    let case = Printf.sprintf "(%d, %d, %d)" x y z in
    Printf.printf "TestCase %s: " case;
  if expected <> actual then
    Printf.printf "[FAIL]\nexpected:%d\nactual:%d\n" expected actual
  else
    Printf.printf "[OK]\n"
  in

  let tested = (1, 2, 3) in
  let (x, y, z) = tested in
  let expected = 3 in
  let actual =  tak x y z in
  assertEquals tested expected actual;

  let tested = (5, 23, 7) in
  let (x, y, z) = tested in
  let expected = 7 in
  let actual =  tak x y z in
  assertEquals tested expected actual;

  let tested = (9, 1, 0) in
  let (x, y, z) = tested in
  let expected = 1 in
  let actual =  tak x y z in
  assertEquals tested expected actual;

  let tested = (1, 1, 1) in
  let (x, y, z) = tested in
  let expected = 1 in
  let actual =  tak x y z in
  assertEquals tested expected actual;
  
  let tested = (0, 42, 0) in
  let (x, y, z) = tested in
  let expected = 0 in
  let actual =  tak x y z in
  assertEquals tested expected actual;

  let tested = (23498, 98734, 98776) in
  let (x, y, z) = tested in
  let expected = 98776 in
  let actual =  tak x y z in
  assertEquals tested expected actual;
