(*

turn-in directory : ex09/
Files to turn in : leibniz_pi.ml
Allowed functions : atan, float_of_int
Now that you can do a summation,
  we’re going to use your skills to compute π.
  To do that, we’ll be using Leibniz’s formula,
  which is fairly easy to understand:
         i=∞    i
  π = 4 × ∑ (−1) / (2i + 1)
         i=0 
  Okay, I know it’s scary, but it’s actually not difficult.
  Now just do what I do: hold on tight and pretend it’s a plan.
  
  Of course, you can’t really go to infinity;
  only Chuck Norris can — and he did it twice.
  
  That means we’ll stop when we reach a minimal delta.
  A delta is a gap between your computed value and
  π’s real value.
  To compute your delta, the reference value to use is: π = 4 × arctan 1.
  Your function will return the number of iterations
  needed to reach a minimum delta, which will be given
  to your function as an argument.
  If the given delta is negative, your function will return -1.
  Its type will be float -> int, and it will be named leibniz_pi.
Your function must be tail-recursive. No tail recursion, no points.
Phew! That’s a wrap. You can grab a drink and chill.
  This exercise is not mandatory.

  *)

let leibniz_pi delta =
  let abs = function
    | x when x < 0.0 -> ~-.x
    | x -> x
  in  
  let pi = 4.0 *. (atan 1.0) in
  let minus_one_raised_by i = ~-.1.0 ** (float_of_int i) in
  let summation_term i =
    let upper_term = minus_one_raised_by i in
    let lower_term = (2 * i) + 1 in
    
    upper_term /. (float_of_int lower_term)
  in
  let rec loop i acc =
    match i with
    | i when (abs (pi -. (4.0 *. acc))) <= delta -> i
    | i -> (
      let sum_term = summation_term i in
      loop (i + 1) (acc +. sum_term))
  in
  if delta < 0.0 || delta == nan then ~-1 else loop 0 0.0



let () =
  let assertEquals case expected actual =
    Printf.printf "TestCase %s: " case;
    let has_failed = expected <> actual
    in
  if has_failed then
    Printf.printf "[FAIL]\nexpected:%d\nactual:%d\n" expected actual
  else
    Printf.printf "[OK]\n"
  in

  print_endline "===========";
  print_endline "Test leibniz_pi:";

  let tested = 1.0 in
  let case = Printf.sprintf "leibniz_pi %f" tested in
  let expected = 1 in
  let actual = leibniz_pi tested in
  assertEquals case expected actual;

  let tested = 0.5 in
  let case = Printf.sprintf "leibniz_pi %f" tested in
  let expected = 2 in
  let actual = leibniz_pi tested in
  assertEquals case expected actual;

  let tested = 0.2 in
  let case = Printf.sprintf "leibniz_pi %f" tested in
  let expected = 5 in
  let actual = leibniz_pi tested in
  assertEquals case expected actual;

  let tested = 0.1 in
  let case = Printf.sprintf "leibniz_pi %f" tested in
  let expected = 10 in
  let actual = leibniz_pi tested in
  assertEquals case expected actual;

  let tested = 0.05 in
  let case = Printf.sprintf "leibniz_pi %f" tested in
  let expected = 20 in
  let actual = leibniz_pi tested in
  assertEquals case expected actual;

  let tested = 0.02 in
  let case = Printf.sprintf "leibniz_pi %f" tested in
  let expected = 50 in
  let actual = leibniz_pi tested in
  assertEquals case expected actual;

  let tested = 0.01 in
  let case = Printf.sprintf "leibniz_pi %f" tested in
  let expected = 100 in
  let actual = leibniz_pi tested in
  assertEquals case expected actual;
