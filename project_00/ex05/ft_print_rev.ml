(*
Exercise 05: ft_print_rev
Turn-in directory : ex05/
Files to turn in : ft_print_rev.ml
Allowed functions : print_char, String.get and String.length
Write a function ft_print_rev of type string -> unit that prints its string pa-
rameter in reverse order, one character at a time, ending with a new line.
Example in the interpreter:
# ft_print_rev "Hello world !";;
! dlrow olleH
- : unit = ()
# ft_print_rev "";;
- : unit = ()
#
Be sure to provide a test suite to demonstrate that your function works as intended
during peer evaluation.
  This exercise is not mandatory
  *)

let ft_print_rev str =
  let rec loop i =
    if i >= 0 then
      (print_char (String.get str i);
      loop (i - 1))
    else
      print_char '\n'
  in
  loop (String.length str - 1)


let () =
  
  let test_case str =
    Printf.printf "TestCase %s: " str;
    ft_print_rev str

  in
  test_case "ab";
  test_case "abc";
  test_case "abcd";
  test_case "abcde";
  test_case "abcdef";
  test_case "abcdefg";
  test_case "_..hellooooooooo";
  test_case ""
  
