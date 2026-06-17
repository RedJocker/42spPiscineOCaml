(*
Exercise 04: ft_print_comb
Turn-in directory : ex04/
Files to turn in : ft_print_comb.ml
Allowed functions : print_int and print_string
Write a function ft_print_comb of type unit -> unit
that displays in ascending order all the different
combinations of 3 digits, each digit different from the other two,
and the 3 digits also in ascending order.
Each combination is separated from the next
one by a comma and a space. Finish your display with a new line.
You must have something that starts and finishes like this:
# ft_print_comb ();;
012, 013, 014, 015, 016, 017, 018, 019, 023, <more numbers>, 789
- : unit = ()
#
As additional information, 987 is not part of the sequence
because 789 is already part of it.
Also note that, for instance, 999 is not part of the sequence
because the 3 digits are not different from each other.
Displaying the right answer in a big string without actually computing it
will be considered cheating during the peer evaluation.
Be sure to provide a test suite to
demonstrate that your function works as intended during peer evaluation
(“test suite” might be a little bit of an overstatement here as well).
  *)


let print_digits x y z =
  print_int x;
  print_int y;
  print_int z

let ft_print_comb () =
  let rec internal x y z =
    print_digits x y z;
    match x, y, z with
    | 7, 8, 9 -> print_string "\n"
    | x, 8, 9 -> (print_string ", "; internal (x + 1) (x + 2) (x + 3))
    | x, y, 9 -> (print_string ", "; internal x (y + 1) (y + 2))
    | x, y, z -> (print_string ", "; internal x y (z + 1))
  in
    internal 0 1 2

let () =
  ft_print_comb ()

