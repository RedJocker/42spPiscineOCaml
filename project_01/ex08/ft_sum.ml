(*

ft_sum.ml
Allowed functions : None
Starting from now, we’re going to do some maths, for a change. You’ve seen some
math before in your life, right? We’re going to do a summation function, named ft_sum,
which works like Σ. If you don’t know what the big scary E-like symbol is, it’s called a
sigma, and you can look it up. Your function will accept the following arguments:
1. An expression to add: since its value usually depends on the index, that means it
will be a function taking the index as a parameter
2. The index’s lower bound of summation
3. The index’s upper bound of summation
Your function’s type will be: (int -> float) -> int -> int -> float, and it will
be tail-recursive. No tail recursion, no points. For example, the following expression:
 i=10
 ∑  (i ** 2)
 i=1

Will be computed using your function as:
# ft_sum (fun i -> float_of_int (i * i)) 1 10;;
- : float = 385.
If the upper bound is less than the lower bound, ft_sum must return nan.
22
OCaml Recursion and higher-order functions - 0
  This exercise is not mandatory.

  *)

let ft_sum f lower upper =
  let rec loop i acc = match i with
  | i when i > upper -> acc
  | i -> loop (i + 1) (acc +. (f i))
  in
  if upper < lower then
    nan
  else
    loop lower 0.0


let () =
  let assertEquals case expected actual =
    Printf.printf "TestCase %s: " case;
    let has_failed = if expected == nan then
                       expected != actual
                     else expected <> actual
    in
  if has_failed then
    Printf.printf "[FAIL]\nexpected:%f\nactual:%f\n" expected actual
  else
    Printf.printf "[OK]\n"
  in

  print_endline "===========";
  print_endline "Test ft_sum:";

  let expecteds =
    [1.0; 3.0; 6.0; 10.0; 15.0; 21.0; 28.0; 36.0; 45.0; 55.0]
  in
  Seq.ints 1
  |> Seq.take 10
  |> Seq.iteri (fun i n ->
         let case =
           Printf.sprintf "ft_sum (fun x -> float_of_int x) 0 %d" n
         in
         let expected = List.nth expecteds i in
         let actual = ft_sum (fun x -> float_of_int x) 0 n in
         assertEquals case expected actual);

  let expecteds =
    [1.0; 5.0; 14.0; 30.0; 55.0; 91.0; 140.0; 204.0; 285.0; 385.0]
  in
  Seq.ints 1
  |> Seq.take 10
  |> Seq.iteri (fun i n ->
         let case =
           Printf.sprintf
             "ft_sum (fun x -> (float_of_int x) *. (float_of_int x)) 0 %d" n
         in
         let expected = List.nth expecteds i in
         let actual = ft_sum (fun x -> (float_of_int x) *. (float_of_int x)) 0 n in
         assertEquals case expected actual);

  let case = "ft_sum (fun x -> float_of_int x) 10 9"
  in
  let expected = nan in
  let actual = ft_sum (fun x -> float_of_int x) 10 9 in
  assertEquals case expected actual;
