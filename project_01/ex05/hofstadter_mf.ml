(*

A tribute to Mr. Hofstadter. Not Leonard, sadly.
Turn-in directory : ex05/
Files to turn in : hofstadter_mf.ml
Allowed functions : None
You will write two functions named hfs_f and hfs_m, which will implement the
Hofstadter Female and Male sequences. The Hofstadter Female and Male sequences are
defined as follows:
F (n) =
| 1 if n = 0
| n − M (F (n − 1)) if n > 0
M (n) =
| 0 if n = 0
| n − F (M (n − 1)) if n > 0
If given a negative argument, your functions must return -1. In case you could not
guess, the type of your functions will be int -> int. Of course, don’t forget to look up
who Douglas Hofstadter is, and watch an episode of *The Big Bang Theory* if you have
never done so.
Example:
# hfs_m 0;;
- : int = 0
# hfs_f 0;;
- : int = 1
# hfs_m 4;;
- : int = 2
# hfs_f 4;;
- : int = 3

Obviously, each sequence must be implemented only once. Implementing
them more than once means you have failed the exercise.
This exercise is not mandatory.

  
  *)



let rec hfs_f n = match n with
| n when n < 0 -> ~-1
| 0 -> 1
| _ -> n - ((n - 1) |> hfs_f |> hfs_m) 
and hfs_m n = match n with
| n when n < 0 -> ~-1
| 0 -> 0
| _ -> n - ((n - 1) |> hfs_m |> hfs_f) 


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

  print_endline "===========";
  print_endline "Test hfs_f:";

  let tested = 0 in
  let expected = 1 in
  let actual = hfs_f tested in
  assertEquals tested expected actual;

  let tested = 4 in
  let expected = 3 in
  let actual = hfs_f tested in
  assertEquals tested expected actual;

  print_endline "===========";
  print_endline "Test hfs_m:";

  let tested = 0 in
  let expected = 0 in
  let actual = hfs_m tested in
  assertEquals tested expected actual;

  let tested = 4 in
  let expected = 2 in
  let actual = hfs_m tested in
  assertEquals tested expected actual;
  print_endline ""
  


