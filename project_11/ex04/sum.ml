(*

Seriously, just a sum. There’s no catch.
Turn-in directory : ex04/
Files to turn in : sum.ml
Allowed functions : None
You will write a function named sum that takes two floating-point numbers and adds
them together. Yes, that’s it.
Your function’s type will be float -> float -> float.
Don’t forget to submit a full program with examples to show your work
is functional. This is harder than it seems.


 *)



let sum f0 f1 = f0 +. f1


let _ =
  let assertEquals case expected actual =
    let (f0, f1) = case in
    Printf.printf "TestCase (%.2f, %.2f): " f0 f1;
    let has_failed = if expected == nan then
                       expected != actual
                     else abs_float (expected -. actual) > 0.01
    in
    if has_failed then
      Printf.printf "[FAIL]\nexpected:%.2f\nactual:%.2f\n" expected actual
    else
      Printf.printf "%.2f [OK]\n" actual
  in

  let case = (2.0, 3.0) in
  let (f0, f1) = case in
  let expected = 5.0 in
  let actual = sum f0 f1 in
  assertEquals case expected actual;

  let case = (1.0, 1.1) in
  let (f0, f1) = case in
  let expected = 2.1 in
  let actual = sum f0 f1 in
  assertEquals case expected actual;

  let case = (15.3, 11.35) in
  let (f0, f1) = case in
  let expected = 26.65 in
  let actual = sum f0 f1 in
  assertEquals case expected actual;

  let case = (~-.15.3, ~-.11.35) in
  let (f0, f1) = case in
  let expected = ~-.26.65 in
  let actual = sum f0 f1 in
  assertEquals case expected actual;
