(*

  Exercise 01: ft_countdown
  Turn-in directory : ex01/
  Files to turn in : ft_countdown.ml
  Allowed functions : print_int and print_char
  
  Write a function ft_countdown of type int -> unit
  that displays a countdown from
  the parameter’s value down to 0, with a new line after each value.
  If the value is negative, display only 0 followed by a new line.
  Examples in the interpreter:
  # ft_countdown 3;;
  3
  2
  1
  0
  - : unit = ()
  # ft_countdown 0;;
  0
  - : unit = ()
  # ft_countdown (-1);;
  0
  - : unit = ()
  #
  Be sure to provide a test suite to demonstrate that
  your function works as intended during peer evaluation.
  *)


let print_int_line x = print_int x; print_char '\n'

let rec ft_countdown from =
  if from <= 0 then print_int_line 0 else (print_int_line from; ft_countdown  (from - 1))

let () =
  print_endline "\n==..first test..==";
  ft_countdown 10;
  
  print_endline "\n==..second test..==";
  ft_countdown 2;
  
  print_endline "\n==..third test..==";
  ft_countdown 0;
  
  print_endline "\n==..fourth test..==";
  ft_countdown ~-1;
