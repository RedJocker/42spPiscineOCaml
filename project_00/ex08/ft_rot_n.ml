(*

Exercise 08: ft_rot_n
Turn-in directory : ex08/
Files to turn in : ft_rot_n.ml
Allowed functions : char_of_int, int_of_char and String.map
Write a function ft_rot_n of type int -> string -> string. Let n be the first pa-
rameter and str be the second parameter. The function ft_rot_n rotates each lowercase
and uppercase alphabetical character of str by n in ascending order. The value n will
always be positive.
Examples in the interpreter:
# ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz";;
- : string = "bcdefghijklmnopqrstuvwxyza"
# ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz";;
- : string = "nopqrstuvwxyzabcdefghijklm"
# ft_rot_n 42 "0123456789";;
- : string = "0123456789"
# ft_rot_n 2 "OI2EAS67B9";;
- : string = "QK2GCU67D9"
# ft_rot_n 0 "Damned !";;
- : string = "Damned !"
# ft_rot_n 42 "";;
- : string = ""
# ft_rot_n 1 "NBzlk qnbjr !";;
- : string = "OCaml rocks !"
Be sure to provide a test suite to demonstrate that your function works as intended
during peer evaluation.
This exercise is not mandatory

  *)

let ft_rot_n n str =
  let rotchar char =
    let a = match char with
    | ch when ch >= 'a' && ch <= 'z' -> 'a'
    | ch when ch >= 'A' && ch <= 'Z' -> 'A'
    | _ -> '0'
    in
    if a == '0'
    then
      char
    else
      let a_code = int_of_char a in
      let base = (int_of_char char) - a_code in
      let rot_base = (base + n) mod 26 in
      let rot_code = rot_base + a_code in
      let rot_ch = char_of_int rot_code in
      rot_ch
  in
  String.map rotchar str

let () =
  let assertEquals case expected actual =
    Printf.printf "Test %s: " case;
    if expected <> actual then
      print_endline "[FAIL]"
    else
      print_endline "[OK]"
  in
  let tested = "0 a" in
  let actual = ft_rot_n 0 "a" in
  let expected = "a" in
  assertEquals tested actual expected;

  let tested = "1 a" in
  let actual = ft_rot_n 1 "a" in
  let expected = "b" in
  assertEquals tested actual expected;

  let tested = "2 a" in
  let actual = ft_rot_n 2 "a" in
  let expected = "c" in
  assertEquals tested actual expected;

  let tested = "0 z" in
  let actual = ft_rot_n 0 "z" in
  let expected = "z" in
  assertEquals tested actual expected;

  let tested = "1 z" in
  let actual = ft_rot_n 1 "z" in
  let expected = "a" in
  assertEquals tested actual expected;

  let tested = "2 z" in
  let actual = ft_rot_n 2 "z" in
  let expected = "b" in
  assertEquals tested actual expected;

  let tested =
    "0 abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let actual = ft_rot_n 0
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let expected =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  assertEquals tested actual expected;

  let tested =
    "1 abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let actual = ft_rot_n 1
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let expected =
    "bcdefghijklmnopqrstuvwxyzaBCDEFGHIJKLMNOPQRSTUVWXYZA0123456789" in
  assertEquals tested actual expected;


  let tested =
    "10 abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let actual = ft_rot_n 10
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let expected =
    "klmnopqrstuvwxyzabcdefghijKLMNOPQRSTUVWXYZABCDEFGHIJ0123456789" in
  assertEquals tested actual expected;
