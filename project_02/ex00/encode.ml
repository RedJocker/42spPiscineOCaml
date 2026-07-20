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


(*

  TODO TEST

utop[5]> encode [1;1;2;1;2;2;2];;
- : (int * int) list = [(2, 1); (1, 2); (1, 1); (3, 2)]

utop[6]> encode [];;
- : (int * 'a) list = []

utop[7]> encode [1];;
- : (int * int) list = [(1, 1)]

utop[8]> encode ["a"];;
- : (int * string) list = [(1, "a")]

utop[12]> encode ['a'; 'a'];;
- : (int * char) list = [(2, 'a')]

utop[14]> encode ['a'; 'a'; 'a'; 'b'; 'b'; 'b'];;
- : (int * char) list = [(3, 'a'); (3, 'b')]

 *)
