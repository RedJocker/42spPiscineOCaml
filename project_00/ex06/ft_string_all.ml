(*


Exercise 06: ft_string_all
Turn-in directory : ex06/
Files to turn in : ft_string_all.ml
Allowed functions : String.get and String.length
Write a function ft_string_all of type (char -> bool) -> string -> bool. To
help you get on track, the first parameter, of type char -> bool, is a function. As this
function returns a bool, it can therefore be referred to as a “predicate” function.
So, the function ft_string_all takes a predicate function and a string as parame-
ters, and applies each character of the string to the predicate function. If the predicate
is true for every character of the string, ft_string_all returns true. Otherwise, if the
predicate function is false for at least one character, ft_string_all returns false.
Examples in the interpreter:
# let is_digit c = c >= ’0’ && c <= ’9’;;
val is_digit : char -> bool = <fun>
# ft_string_all is_digit "0123456789";;
- : bool = true
# ft_string_all is_digit "O12EAS67B9";;
- : bool = false
#
Be sure to provide a test suite to demonstrate that your function works as intended
during peer evaluation.
This exercise is not mandatory

  *)


let ft_string_all predicate str =
  let rec loop i =
    if i == 0 then
      predicate (String.get str i)
    else 
      (predicate (String.get str i)) && loop (i - 1)
  in
  let len = String.length str in
  if len == 0 then
    true
  else
    loop (len - 1)


let () =
  
  let assertEquals case expected actual =
    Printf.printf "Test %s: " case;
    if expected <> actual then
      print_endline "[FAIL]"
    else
      print_endline "[OK]"
  in
  
  let is_blank = function
  | '\n' | ' ' | '\t'  -> true
  | _ -> false
  in
  let tested = "   " in
  assertEquals
    ("is_blank <"^tested^">") true (ft_string_all is_blank tested);
  let tested = "abc" in
  assertEquals
    ("is_blank <"^tested^">") false (ft_string_all is_blank tested);
  let tested = " . " in
  assertEquals
    ("is_blank <"^tested^">") false (ft_string_all is_blank tested);


  let is_digit = function
  | ch when ch >= '0' && ch <= '9'  -> true
  | _ -> false
  in
  let tested = "123" in
  assertEquals
    ("is_digit <"^tested^">") true (ft_string_all is_digit tested);
  let tested = "12a3" in
  assertEquals
    ("is_digit <"^tested^">") false (ft_string_all is_digit tested);
  let tested = "" in
  assertEquals
    ("is_digit <"^tested^">") true (ft_string_all is_digit tested);


  let is_upper = function
  | ch when ch >= 'A' && ch <= 'Z'  -> true
  | _ -> false
  in
  let tested = "A1B" in
  assertEquals
    ("is_upper <"^tested^">") false (ft_string_all is_upper tested);
  let tested = "ABsD" in
  assertEquals
    ("is_upper <"^tested^">") false (ft_string_all is_upper tested);
  let tested = "ABCSFFD" in
  assertEquals
    ("is_upper <"^tested^">") true (ft_string_all is_upper tested);
    
