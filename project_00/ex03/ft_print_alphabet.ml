(*

  
Exercise 03: ft_print_alphabet
Turn-in directory : ex03/
Files to turn in : ft_print_alphabet.ml
Allowed functions : char_of_int, int_of_char and print_char
Write a function ft_print_alphabet of type unit -> unit that displays the alpha-
bet on a single line followed by a new line.
Example in the interpreter:
# ft_print_alphabet ();;
abcdefghijklmnopqrstuvwxyz
- : unit = ()
#
Be sure to provide a test suite to demonstrate that
your function works as intended
during peer evaluation
(“test suite” might be a little bit of an overstatement here).
Obviously, printing 26 characters one after the other 
will be considered cheating. 
You are allowed only one use of print_char in the exercise, 
excluding the print_char for the newline at the end of output.

*)

let ft_print_alphabet () =
  let rec private_helper char_code =
    if (char_code > 122) then
      print_char '\n'
    else
      (print_char (char_of_int char_code);
      private_helper (char_code + 1))
  in
  private_helper (int_of_char 'a')


let () =
  ft_print_alphabet ()
