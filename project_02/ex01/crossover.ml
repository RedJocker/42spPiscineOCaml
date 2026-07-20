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
