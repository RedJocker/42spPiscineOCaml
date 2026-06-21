(*

Exercise 07: ft_is_palindrome
Turn-in directory : ex07/
Files to turn in : ft_is_palindrome.ml
Allowed functions : String.get and String.length
Write a function ft_is_palindrome of type string -> bool that returns true if
the string parameter is a palindrome character by character, false otherwise. If you
intend to use your previous function ft_string_all, please embed its code in the file
ft_is_palindrome.ml as well. The empty string is a palindrome.
Examples in the interpreter:
# ft_is_palindrome "radar";;
- : bool = true
# ft_is_palindrome "madam";;
- : bool = true
# ft_is_palindrome "car";;
- : bool = false
# ft_is_palindrome "";;
- : bool = true
Be sure to provide a test suite to demonstrate that your function works as intended
during peer evaluation.
  This exercise is not mandatory.

  *)

let ft_is_palindrome (str: string) =
  let rec loop = function
    | start, end' when start >= end' -> true
    | start, end'
      when String.get str start <> String.get str end' -> false
    | start, end' -> loop (start + 1, end' - 1)
  in
  loop (0, String.length str - 1)

let () =    
  let assertEquals case expected actual =
    Printf.printf "Test %s: " case;
    if expected <> actual then
      print_endline "[FAIL]"
    else
      print_endline "[OK]"
  in
  let tested = "a" in
  let actual = ft_is_palindrome tested in
  let expected = true in
  assertEquals tested actual expected;

  let tested = "b" in
  let actual = ft_is_palindrome tested in
  let expected = true in
  assertEquals tested actual expected;

  let tested = "ab" in
  let actual = ft_is_palindrome tested in
  let expected = false in
  assertEquals tested actual expected;

  let tested = "aa" in
  let actual = ft_is_palindrome tested in
  let expected = true in
  assertEquals tested actual expected;

  let tested = "aba" in
  let actual = ft_is_palindrome tested in
  let expected = true in
  assertEquals tested actual expected;


  let tested = "abab" in
  let actual = ft_is_palindrome tested in
  let expected = false in
  assertEquals tested actual expected;

  let tested = "abba" in
  let actual = ft_is_palindrome tested in
  let expected = true in
  assertEquals tested actual expected;


  let tested = "abcba" in
  let actual = ft_is_palindrome tested in
  let expected = true in
  assertEquals tested actual expected;

  let tested = "abccba" in
  let actual = ft_is_palindrome tested in
  let expected = true in
  assertEquals tested actual expected;

  let tested = "abcdba" in
  let actual = ft_is_palindrome tested in
  let expected = false in
  assertEquals tested actual expected;

  let tested = "abcacba" in
  let actual = ft_is_palindrome tested in
  let expected = true in
  assertEquals tested actual expected;

  
  let tested = "radar" in
  let actual = ft_is_palindrome tested in
  let expected = true in
  assertEquals tested actual expected;

  let tested = "madam" in
  let actual = ft_is_palindrome tested in
  let expected = true in
  assertEquals tested actual expected;

  let tested = "" in
  let actual = ft_is_palindrome tested in
  let expected = true in
  assertEquals tested actual expected;

  let tested = "car" in
  let actual = ft_is_palindrome tested in
  let expected = false in
  assertEquals tested actual expected;
  
