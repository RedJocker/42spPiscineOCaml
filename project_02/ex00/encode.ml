(*

  Exercise 00: Do you even compress?
Turn-in directory : ex00/
Files to turn in : encode.ml
Allowed functions : None
Run-length encoding is a very simple form of data compression algorithm. Consecu-
tive elements are stored as a single data element and the number of times it repeats. For
instance, the string "aaabbb" can be stored as "3a3b".
Write a function encode that encodes a list of elements to a list of tuples containing
the element and the number of times it repeats. The function must be typed as:
val encode : ’a list -> (int * ’a) list
In case of an empty list as a parameter, the function should return an empty list as
well.

 *)


let encode lst =
  let rec reverse list acc = match list with
    | [] -> acc
    | head::tail -> reverse tail (head::acc)
  in
  let rec loop previous_element count list acc =
    match list with
    | [] -> reverse ((count, previous_element)::acc) []
    | current_element::rest ->
       if current_element = previous_element then
         loop current_element (count + 1) rest acc
       else
         loop current_element 1 rest ((count, previous_element)::acc)
  in
  match lst with
  | [] -> []
  | first::rest -> loop first 1 rest []



let () =
  let list_to_string element_to_string list =
    let rec loop lst acc =
      match lst with
      | [] -> acc
      | [ele] -> acc ^ (element_to_string ele)
      | ele::rest ->  loop rest (acc ^ (element_to_string ele) ^ ", ")
    in
    let inner_list = loop list "" in
    "["^inner_list^"]"
  in
  let pair_to_string format1 format2 (fst, snd) =
    let formatted1 = Printf.sprintf format1 fst in
    let formatted2 = Printf.sprintf format2 snd in
    "(" ^ formatted1 ^ ", " ^ formatted2 ^ ")"
  in
  let pair_intint_to_string  element =
    pair_to_string "%d" "%d" element
  in
  let pair_intstring_to_string  element =
    pair_to_string "%d" "%s" element
  in
  let pair_intchar_to_string  element =
    pair_to_string "%d" "%c" element
  in
  let print_list element_to_string channel list =
    let lst_str = list_to_string element_to_string list in
    print_string lst_str
  in


  let assertEquals case expected actual element_to_string=
    Printf.printf "TestCase %s: " case;
    let has_failed = expected <> actual in
    if has_failed then
      Printf.printf "[FAIL]\nexpected:%a\nactual:%a\n"
        (print_list element_to_string) expected
        (print_list element_to_string) actual
    else
      Printf.printf "%a [OK] \n" (print_list element_to_string) actual
  in

  let tested =  [1;1;2;1;2;2;2] in
  let expected = [(2, 1); (1, 2); (1, 1); (3, 2)] in
  let actual = encode tested in
  let case = Printf.sprintf "%a" (fun () -> (list_to_string string_of_int)) tested in
  assertEquals case expected actual pair_intint_to_string;

  let tested =  [] in
  let expected = [] in
  let actual = encode tested in
  let case = Printf.sprintf "%a" (fun () -> (list_to_string string_of_int)) tested in
  assertEquals case expected actual pair_intint_to_string;

  let tested =  [1] in
  let expected = [(1, 1)] in
  let actual = encode tested in
  let case = Printf.sprintf "%a" (fun () -> (list_to_string string_of_int)) tested in
  assertEquals case expected actual pair_intint_to_string;

  let id any = any in

  let tested =  ["a"] in
  let expected = [(1, "a")] in
  let actual = encode tested in
  let case = Printf.sprintf "%a" (fun () -> (list_to_string id)) tested in
  assertEquals case expected actual pair_intstring_to_string;

  let tested =  ["a"; "a"] in
  let expected = [(2, "a")] in
  let actual = encode tested in
  let case = Printf.sprintf "%a" (fun () -> (list_to_string id)) tested in
  assertEquals case expected actual pair_intstring_to_string;

  let tested =  ["a"; "a"; "abc"; "ab"; "abc"; "abc"] in
  let expected = [(2, "a"); (1, "abc"); (1, "ab"); (2, "abc")] in
  let actual = encode tested in
  let case = Printf.sprintf "%a" (fun () -> (list_to_string id)) tested in
  assertEquals case expected actual pair_intstring_to_string;

  let tested =  ['a';] in
  let expected = [(1, 'a')] in
  let actual = encode tested in
  let case = Printf.sprintf "%a" (fun () -> (list_to_string (String.make 1))) tested in
  assertEquals case expected actual pair_intchar_to_string;

  let tested =  ['a'; 'a';] in
  let expected = [(2, 'a')] in
  let actual = encode tested in
  let case = Printf.sprintf "%a" (fun () -> (list_to_string (String.make 1))) tested in
  assertEquals case expected actual pair_intchar_to_string;

  let tested =  ['a'; 'a'; 'a'] in
  let expected = [(3, 'a')] in
  let actual = encode tested in
  let case = Printf.sprintf "%a" (fun () -> (list_to_string (String.make 1))) tested in
  assertEquals case expected actual pair_intchar_to_string;

  let tested =  ['a'; 'a'; 'a'; 'b'; 'b'; 'b'] in
  let expected = [(3, 'a'); (3, 'b')] in
  let actual = encode tested in
  let case = Printf.sprintf "%a" (fun () -> (list_to_string (String.make 1))) tested in
  assertEquals case expected actual pair_intchar_to_string;

  let tested =  ['a'; 'a'; 'a'; 'b'; 'b'; 'c'; 'b'] in
  let expected = [(3, 'a'); (2, 'b'); (1, 'c'); (1, 'b')] in
  let actual = encode tested in
  let case = Printf.sprintf "%a" (fun () -> (list_to_string (String.make 1))) tested in
  assertEquals case expected actual pair_intchar_to_string;
