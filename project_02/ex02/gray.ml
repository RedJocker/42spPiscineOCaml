(*


  Exercise 02: Fifty Strings of Gray
Turn-in directory : ex02/
Files to turn in : gray.ml
Allowed functions : String module and not the @ operator
The sequence of Gray is a sequence of possible combinations of bits ordered so that
when you want to go from one element to the following, you only have to shift one bit.
It’s a way of having a constant time of computing when changing values so that there’s no
intermediate state that can crash a program. If you have a 2-bit standard set sequence,
for example: 00 01 10 11
Assume you are in the state 01. If you want to switch to the next state, you have
to change the last bit to 0 and the first one to 1. There could be an intermediate state
where the set of bits is 00 before becoming 10. And that’s wrong.
The Gray sequence of a set of two bits is as follows: 00 01 11 10. That way, when
you pass from 01 to 11, you only have to shift one bit.
Write a function that takes an integer n as a parameter and writes all the strings of
the Gray sequence of size n, in the correct order on the standard output, finished by a
newline.
# gray 1
0 1
- : unit = ()
# gray 2
00 01 11 10
- : unit = ()
# gray 3
000 001 011 010 110 111 101 100
- : unit = ()


 *)


let gray n =
  let rec reverse_list lst acc = match lst with
    | []  -> acc
    | head::tail -> reverse_list tail (head::acc)
  in
  let rec map_list f lst acc = match lst with
    | [] -> reverse_list acc []
    | head::tail -> map_list f tail ((f head)::acc)
  in
  let rec concat_list lst1 lst2 acc = match (lst1, lst2) with
    | [], [] -> reverse_list acc []
    | [], lst -> concat_list lst [] acc
    | (head::tail), _ -> concat_list tail lst2 (head::acc)
  in
  let insert_0 str = String.cat "0" str in
  let insert_1 str = String.cat "1" str in
  let rec gray_list i = match i with
    | i when i <= 0 -> []
    | 1 -> ["0"; "1"]
    | i ->
       let prev = gray_list (i - 1) in
       let prev_rev = reverse_list prev [] in
       let prev_c = map_list insert_0 prev [] in
       let prev_rec_c = map_list insert_1 prev_rev [] in
       concat_list prev_c prev_rec_c []
  in
  if n <= 0
  then
    ""
  else
    String.concat " " (gray_list n)


let () =
  let assertEquals tested expected actual =
    let n = tested in
    let case = Printf.sprintf "(%d)" n in
    Printf.printf "TestCase %s: " case;
  if expected <> actual then
    Printf.printf "[FAIL]\nexpected:%s\nactual:%s\n" expected actual
  else
    Printf.printf "%s [OK] \n" actual
  in

  let tested = 0 in
  let expected = "" in
  let actual = gray tested in
  assertEquals tested expected actual;

  let tested = 1 in
  let expected = "0 1" in
  let actual = gray tested in
  assertEquals tested expected actual;

  let tested = 2 in
  let expected = "00 01 11 10" in
  let actual = gray tested in
  assertEquals tested expected actual;

  let tested = 3 in
  let expected = "000 001 011 010 110 111 101 100" in
  let actual = gray tested in
  assertEquals tested expected actual;

  let tested = 4 in
  let expected = "0000 0001 0011 0010 0110 0111 0101 0100 1100 1101 1111 1110 1010 1011 1001 1000" in
  let actual = gray tested in
  assertEquals tested expected actual;

  let tested = ~-1 in
  let expected = "" in
  let actual = gray tested in
  assertEquals tested expected actual;
