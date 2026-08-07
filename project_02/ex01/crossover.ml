(*

Exercise 01: Crossover
Turn-in directory : ex01/
Files to turn in : crossover.ml
Allowed functions : None
Write a function crossover that takes two lists as parameters and returns a list of
all the common elements between the two lists. The function must be typed as:
val crossover : ’a list -> ’a list -> ’a list
In case of an empty list as one of the parameters, the function should return an empty
list too. But it’s obvious, isn’t it? We don’t have to handle duplicates in lists.

 *)


let crossover lst0 lst1 =
  let rec reverse_list lst acc = match lst with
    | []  -> acc
    | head::tail -> reverse_list tail (head::acc)
  in
  let rec remove_all element lst has_removed acc =
    match lst with
    | [] -> (has_removed, reverse_list acc [])
    | el::rest when el = element -> remove_all element rest true acc
    | el::rest -> remove_all element rest has_removed (el::acc)
  in
  let rec loop l0 l1 acc = match (l0, l1) with
    | [], [] -> reverse_list acc []
    | [], lst -> loop lst [] acc
    | (head::tail), _ ->
       let (_, l0_n) = remove_all head tail false [] in
       let (has_removed, l1_n) = remove_all head l1 false [] in
       if has_removed then
         loop l0_n l1_n (head::acc)
       else
         loop l0_n l1_n acc
  in
  loop lst0 lst1 []

let _ =
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

  let list_pair_to_string lst_to_string (fst, snd) =
    let formatted1 = lst_to_string fst in
    let formatted2 = lst_to_string snd in
    "(" ^ formatted1 ^ ", " ^ formatted2 ^ ")"
  in

  let int_list_pair_to_string list_pair =
    list_pair_to_string (list_to_string string_of_int) list_pair
  in

  let assertEquals case expected actual lst_to_string =
    Printf.printf "TestCase %s: " case;
    let has_failed = expected <> actual in
    let res = if has_failed then
      Printf.sprintf "[FAIL]\nexpected:%s\nactual:%s\n"
        (lst_to_string expected)
        (lst_to_string actual)
    else
      Printf.sprintf "%s [OK] \n" (lst_to_string actual)
    in
  print_endline res
  in

  let test_case (tst0, tst1) expected0 expected1 =
    let actual0 = crossover tst0 tst1 in
    let case0 = Printf.sprintf "%a"
                  (fun () -> int_list_pair_to_string) (tst0, tst1) in
    assertEquals case0 expected0 actual0 (list_to_string string_of_int);
    let actual1 = crossover tst1 tst0 in
    let case1 =
      Printf.sprintf "%a" (fun () -> int_list_pair_to_string) (tst1, tst0) in
    assertEquals case1 expected1 actual1 (list_to_string string_of_int);
  in

  test_case ([1; 2; 3], [2; 3; 4]) [2; 3] [2; 3];
  test_case ([1; 2; 3;], [1; 2; 3;]) [1; 2; 3] [1; 2; 3];
  test_case ([1; 2; 3;], [4; 5; 6;]) [] [];
  test_case ([3; 2; 1;], [1; 2; 3;]) [3; 2; 1] [1; 2; 3];
  test_case ([], [1; 2; 3;]) [] [];
  test_case ([], []) [] [];
  test_case ([1; 2; 3], [3; 4; 5]) [3] [3];
  test_case ([3; 2; 1], [3; 4; 5]) [3] [3];
  test_case ([3; 2; 1], [5; 4; 3]) [3] [3];
  test_case ([2; 3; 1], [5; 4; 3]) [3] [3];
