(*

Exercise 02: ft_power
Turn-in directory : ex02/
Files to turn in : ft_power.ml
Allowed functions : Nothing
Write a function ft_power of type int -> int -> int that returns the first parame-
ter raised to the power of the second parameter. Both parameters will always be positive
or equal to 0, but they will never both be 0 at the same time.
Examples in the interpreter:
# ft_power 2 4;;
- : int = 16
# ft_power 3 0;;
- : int = 1
# ft_power 0 5;;
- : int = 0
#
Be sure to provide a test suite to demonstrate that your function works as intended
  during peer evaluation


*)

let rec ft_power base exponent = match base, exponent with
| 0, _ -> 0
| _, 0 -> 1
| b, xp -> b * (ft_power base (exponent - 1))


let () =
  print_endline "\n==..first test..==";
  Seq.ints 0
  |> Seq.take 11
  |> Seq.iter (fun x ->
    Printf.printf "TestCase: 2 ** %d: %d\n" x (ft_power 2 x));

  print_endline "\n==..second test..==";
  Seq.ints 0
  |> Seq.take 11
  |> Seq.iter (fun x ->
    Printf.printf "TestCase: %d ** 2: %d\n" x (ft_power x 2));

  print_endline "\n==..third test..==";
  Seq.ints 0
  |> Seq.take 11
  |> Seq.iter (fun x ->
    Printf.printf "TestCase: 10 ** %d: %d\n" x (ft_power 10 x));

  print_endline "\n==..fourth test..==";
  Seq.ints 0
  |> Seq.take 11
  |> Seq.iter (fun x ->
    Printf.printf "TestCase: %d ** 10: %d\n" x (ft_power x 10));
