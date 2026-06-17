(*
Exercise 00: ft_test_sign
Exercise 00
Exercise 00: ft_test_sign
Turn-in directory : ex00/
Files to turn in : ft_test_sign.ml
Allowed functions : print_endline
Write a function ft_test_sign of type int -> unit that displays "positive" or
"negative", each followed by a new line, depending on the sign of the parameter. The
value 0 is always considered positive.
Examples in the interpreter:
# ft_test_sign 42;;
positive
- : unit = ()
# ft_test_sign 0;;
positive
- : unit = ()
# ft_test_sign (-42);;
negative
- : unit = ()
#
Be sure to provide a test suite to demonstrate that your function works as intended
  during peer evaluation.
  *)

let ft_test_sign number =
  print_endline (match number >= 0 with
  | true -> "positive"
  | _ -> "negative")


let () =
  print_endline "\n==..first test..==";
  Seq.ints ~-10
  |> Seq.take 21
  |> Seq.iter (fun x -> Printf.printf "case: %+d: " x; ft_test_sign x);


  print_endline "\n==..second test..==";
  Seq.ints ~-10
  |> Seq.take 21
  |> Seq.map (( * ) 10)
  |> Seq.iter (fun x -> Printf.printf "case: %+d: " x; ft_test_sign x);


  print_endline "\n==..third test..==";
  Seq.repeat 0
  |> Seq.take 21
  |> Seq.map (fun _ -> (Random.int 101) - 50)
  |> Seq.iter (fun x -> Printf.printf "case: %+d: " x; ft_test_sign x);


(*
  subject is being a bit careless in not listing expected operators as well
  on permited functions list as operators in ocaml are just infix functions,
  but clearly it would not be possible
  to complete this requirement without an ordering function like >= or <
*)
