(*


What’s red and goes up and down?
Turn-in directory : ex02/
Files to turn in : jokes.ml
  Forbidden functions : None

You will write a program to print a joke on the standard output, followed by an end-
  line character.

Your jokes can be whatever you want, but you will get bonus points if they are bad.

  The only restriction is that you must store them in an array and there must be at least
five (5) of them. Your program will randomly pick a joke from this array and print it
to the standard output.
A joke is considered bad if your grader wants to slap you after
  reading it.
  *)



let print_joke () =
  let jokes = [|
    "joke0";
    "joke1";
    "joke2";
    "joke3";
    "joke4";
  |] in
  let index = Random.int_in_range ~min:0 ~max:((Array.length jokes) - 1) in
  let joke = Array.get jokes index in

  print_endline joke

let _ =
  Random.self_init ();
  print_joke ()
