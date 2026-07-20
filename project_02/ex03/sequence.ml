(*
Exercise 03: One and one and one is three
Turn-in directory : ex03/
Files to turn in : sequence.ml
Allowed functions : None
Assume the following sequence:
1
11
21
1211
111221
312211
13112221
...
Just like in Exercise 00, this sequence generates the element n from the element n-1
and consists of enumerating the count of the numbers found in n-1.
1. The first element is 1, so there is one 1; thus, the second element is 11.
2. The second element is 11, so there are two 1s; thus, the third element is 21.
3. The third element is 21, so there is one 2 and one 1; thus, the fourth element is
1211.
4. And so on. . .
Write a function sequence that takes an integer n as parameter and returns the nth
element of that sequence as a string. The function must be typed as: val sequence :
int -> string. In case of an invalid parameter, the function should return an empty
string.


 *)



let sequence n =
  let rec reverse list acc = match list with
      | [] -> acc
      | head::tail -> reverse tail (head::acc)
    in
  let encode lst =
    let rec loop previous_element count list acc =
      match list with
      | [] -> reverse (count::(previous_element::acc)) []
      | current_element::rest ->
         if current_element = previous_element then
           loop current_element (count + 1) rest acc
         else
           loop current_element 1 rest (count::(previous_element::acc))
    in
    match lst with
    | [] -> []
    | first::rest -> loop first 1 rest []
  in
  let rec int_to_string i = match i with
    | i when i < 0 -> "-" ^ (int_to_string ~-i)
    | 0 -> "0"
    | 1 -> "1"
    | 2 -> "2"
    | 3 -> "3"
    | 4 -> "4"
    | 5 -> "5"
    | 6 -> "6"
    | 7 -> "7"
    | 8 -> "8"
    | 9 -> "9"
    | num ->
       let leading = num / 10 in
       let digit = num - (leading * 10) in
       (int_to_string leading) ^ (int_to_string digit)
  in
  
  let rec concat_intlst l = match l with
    | [] -> ""
    | a::tail -> (int_to_string a) ^ (concat_intlst tail)
  in
  let rec seq i acc =
    if i = n then
      reverse acc []
    else
      let next = encode acc in
      seq (i + 1) next
  in
  if n < 0 then "" else concat_intlst (seq 0 [1])
