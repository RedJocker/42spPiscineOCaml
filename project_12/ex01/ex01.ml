(*

Exercise 01
Exercise 01: The Hashtbl module and the Hashtbl.Make functor
Turn-in directory : ex01/
Files to turn in : ex01.ml
Allowed functions : The Hashtbl module, String.length and String.get
OCaml’s STD lib also provides a hash table. As you can read in the documentation,
this module exposes a lot things, including a functorial interface. This functorial interface
is composed of several things, but for this exercice, let’s focus on: HashedType, S and
Make. Copy the following lines into the file "ex01.ml":

let () =
let ht = StringHashtbl.create 5 in
let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
let pairs = List.map (fun s -> (s, String.length s)) values in
List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht

Complete the file "ex01.ml" in order to achieve the following output:
$> ocamlopt ex01.ml && ./a.out
k = "Ocaml", v = 5
k = "Hello", v = 5
k = "42", v = 2
k = "H", v = 1
k = "world", v = 5
$>

The order of your output might differ from above according to
your hash function. A dummy hash function such as length won’t
be accepted, write a true one that is known

 *)


module StringHashType = struct
  type t = String.t
  let equal = ( = )
  let hash = Hashtbl.hash
end

module StringHashtbl = Hashtbl.Make(StringHashType)

let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
