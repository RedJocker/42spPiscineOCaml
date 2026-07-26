(*


Exercise 05: DNA -> Helix
Turn-in directory : ex05/
Files to turn in : helix.ml
Allowed functions : String concatenation operator and Random module
As seen previously, two helices can combine to create a DNA structure. As you will
see in this exercise, rules are applied when there is a combination. The link of that
combination occurs where the bases are located: P - D - Base <=> Base - D - P, P - D
- Base <=> Base - D - P, . . .
• Write an helix type that is a list of elements of type nucleotide.
• Write a function generate_helix that takes an int n as a parameter and construct
a random sequence of nucleotides as a list of size n. The function must be typed a
: val generate_helix : int -> helix.
• Write a function helix_to_string that convert a list of nucleotides as helix type
resulting from the previous function to a string of nucleobases. The function must
be typed as: val helix_to_string : helix -> string.
• Write a function complementary_helix that takes an helix as a parameter and
generate the corresponding helix according of the Nucleobase pairing rules that
follows :
◦ A (Adenine) can be associated with T.
◦ T (Thymine) can be associated with A.
◦ C (Cytosine) can be associated with G.
◦ G (Guanine) can be associated with C.
The function must be typed as: val complementary_helix : helix -> helix
13CommentHighlight
OCaml Pattern Matching and Data Types - 0
  This exercise is not mandatory


  *)



(* from ex04 *)

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = {
  phosphate: phosphate;
  deoxyribose: deoxyribose;
  nucleobase: nucleobase;
}

let generate_nucleotide ch =
  let deoxyribose = "deoxyribose" in
  let phosphate = "phosphate" in
  let nucleobase = match ch with
  | 'a' | 'A' -> A
  | 't' | 'T' -> T
  | 'c' | 'C' -> C
  | 'g' | 'G' -> G
  | _ -> None
  in
  {deoxyribose; phosphate; nucleobase}

(* *)

type helix = nucleotide list

let generate_helix n : helix =
  let rec loop i acc =
    if i <= 0 then acc else
      let nucleotide = generate_nucleotide
	(match Random.int_in_range ~min:0 ~max:3 with
	| 0 -> 'a'
	| 1 -> 't'
	| 2 -> 'c'
	| _ -> 'g')
      in
      loop (i - 1) (nucleotide::acc)
  in
  loop n [] 
  
let rec helix_to_string: helix -> string = fun helix -> match helix with
| [] -> ""
| head::tail ->
  let current = match head with
  | {nucleobase=A } -> "A"
  | {nucleobase=T } -> "T"
  | {nucleobase=C } -> "C"
  | {nucleobase=G } -> "G"
  | {nucleobase=None } -> "_"
  in
  current ^ (helix_to_string tail)
  


let complementary_helix: helix -> helix = fun helix ->
  let rec reverse lst acc = match lst with
  | [] -> acc
  | head::tail -> reverse tail (head::acc)
  in
  let rec loop h acc = match h with
  | [] -> reverse acc []
  | head::tail ->
    let complement = (match head with
    | {nucleobase=A } -> { head with nucleobase=T}
    | {nucleobase=T } -> { head with nucleobase=A}
    | {nucleobase=C } -> { head with nucleobase=G}
    | {nucleobase=G } -> { head with nucleobase=C}
    | {nucleobase=None } -> { head with nucleobase=None})
    in
    loop tail (complement::acc)
  in
  loop helix []

(* TODO TEST *)

(* TODO rest of exercises of this list *)
