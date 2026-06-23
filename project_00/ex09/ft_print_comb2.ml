(*

Exercise 09: ft_print_comb2
Turn-in directory : ex09/
Files to turn in : ft_print_comb2.ml
Allowed functions : print_char, print_int
Write a function ft_print_comb2 of type unit -> unit that displays each unique
combination of two numbers, each one between 00 and 99, in ascending order. Each
combination is separated from the next one by a comma and a space. Finish your display
with a new line.
You must have something that starts and finishes that way:
# ft_print_comb2 ();;
00 01, 00 02, 00 03, 00 04, 00 05, <more numbers>, 00 99, 01 02, <more numbers>, 97 99, 98 99
- : unit = ()
#
Displaying the right answer in a big string without actually computing it will be
treated as cheating during the peer evaluation.
Be sure to provide a test suite to prove that your function works as intended during
peer evaluation (“test suite” might be a little bit of an overstatement as well).
This exercise is not mandatory.

  *)

let ft_print_comb2 () =
  let print_separator () =
    print_char ',';
    print_char ' ';
  in
  let print_number num =
    if num < 10 then
      (print_int 0;
      print_int num)
    else
      print_int num
  in
  let print_numbers a b =
    
    print_number a;
    print_char ' ';
    print_number b;
    
  in
  let rec loop = function
  | (98, 99) -> print_numbers 98 99; print_char '\n'
  | (a, 99) -> print_numbers a 99; print_separator(); loop (a + 1, a + 2)
  | (a, b) -> print_numbers a b; print_separator(); loop (a, b + 1)
  in
  loop (0, 1)

let () =
  ft_print_comb2()
