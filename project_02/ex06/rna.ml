(*


Turn-in directory : ex06/
Files to turn in : rna.ml
Allowed functions : None
A Messenger RNA is a molecule involved in the process of synthesizing proteins. The
main aim of the RNA is to create a complementary working copy of a DNA Helix. It’s
really clever since it prevents the DNA of being altered and allows multiple copies so that
the process can be really fast. It was first introduced by scientists Jacques Monod and
Francois Jacob.
• Write a type rna as a list of elements of type nucleobase.
• Write a function generate_rna that creates an element of type rna from an element
of type helix according to the following rules :
◦ During the creation, the rna is just like a complementary helix except that the T
nucleobase is switched to U nucleobase (Uracil). Modify your type nucleobase
accordingly. (I told you to read the whole subject before starting...)
◦ The list of nucleobases of the rna is the list of nucleobases that are comple-
mentary with the original helix’s nucleobase (except for the first rule).
For instance, the sequence of nucleobase "ATCGA" will produce a [U;A;G;C;U]
rna. The function must be typed as: val generate_rna: helix -> rna.
This exercise is not mandatory.


  *)



(* from ex04 *)

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
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

(* from ex05 *)

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
  | {nucleobase=U } | {nucleobase=None } -> "_"

  in
  current ^ (helix_to_string tail)

let rec reverse lst acc = match lst with
  | [] -> acc
  | head::tail -> reverse tail (head::acc)


let complementary_helix: helix -> helix = fun helix ->
  
  let rec loop h acc = match h with
  | [] -> reverse acc []
  | head::tail ->
    let complement = (match head with
    | {nucleobase=A } -> { head with nucleobase=T}
    | {nucleobase=T } -> { head with nucleobase=A}
    | {nucleobase=C } -> { head with nucleobase=G}
    | {nucleobase=G } -> { head with nucleobase=C}
    | {nucleobase=U } | {nucleobase=None } -> { head with nucleobase=None})
    in
    loop tail (complement::acc)
  in
  loop helix []


(* current exercise *)

type rna = nucleobase list

let generate_rna (helix : helix) : rna =
  let rec loop h acc = match h with
    | [] -> reverse acc []
    | head::tail ->
       let rna_nucleobase = (match head with
                             | {nucleobase=A } -> U
                             | {nucleobase=T } -> A
                             | {nucleobase=C } -> G
                             | {nucleobase=G } -> C
                             | {nucleobase=U } | {nucleobase=None } -> None)
       in
       loop tail (rna_nucleobase::acc)
  in
  loop helix []


(* TODO MAKE TESTS FOR THIS STAGE *)
(* TODO ex07, ex08 *)

let _ =

  let nucleotide_to_string {phosphate; deoxyribose; nucleobase} =
    let nucleobase = match nucleobase with
      | A -> "A"
      | T -> "T"
      | C -> "C"
      | G -> "G"
      | U -> "U"
      | None -> "None"
    in
    Printf.sprintf "{%s; %s; %s}" phosphate deoxyribose nucleobase
  in

  let list_to_string element_to_string list =
    let rec loop lst acc =
      match lst with
      | [] -> acc
      | [ele] -> acc ^ (element_to_string ele)
      | ele::rest ->  loop rest (acc ^ (element_to_string ele) ^ ";\n  ")
    in
    let inner_list = loop list "" in
    "[\n  "^inner_list^"\n]"
  in

  let assertEquals case to_string expected actual =
    Printf.printf "TestCase %s: " case;
    let expected_str = to_string expected in
    let actual_str = to_string actual in
  if expected <> actual then
    Printf.printf "[FAIL]\nexpected:%s\nactual:%s\n" expected_str actual_str
  else
    Printf.printf "%s [OK] \n" actual_str
  in
  let default =
    {phosphate="phosphate"; deoxyribose="deoxyribose"; nucleobase=None}
  in
  let seed = 2 in
  Random.init seed;
  (*
  let id x = x in
  
  
 
  let test_case num_elements exp_gen exp_str exp_cmp exp_cmpstr =
    print_endline "";
    let tested = num_elements in
    let case = Printf.sprintf "generate_helix %d" tested in
    let actual = generate_helix num_elements in
    assertEquals case (list_to_string nucleotide_to_string) exp_gen actual;

    let base = actual in
    let actual = helix_to_string base in
    let case = "helix_to_string" in
    assertEquals case id exp_str actual;

    let actual = complementary_helix base in
    let case = "complementary_helix" in
    assertEquals case (list_to_string nucleotide_to_string) exp_cmp actual;

    let base = actual in
    let actual = helix_to_string base in
    let case = "helix_to_string complementary_helix" in
    assertEquals case id exp_cmpstr actual;
  in

  test_case
    2
    [{default with nucleobase=T}; {default with nucleobase=T}]
    "TT"
    [{default with nucleobase=A}; {default with nucleobase=A}]
    "AA";

  test_case
    0
    []
    ""
    []
    "";

  test_case
    ~-1
    []
    ""
    []
    "";

  test_case
    2
    [{default with nucleobase=A}; {default with nucleobase=C}]
    "AC"
    [{default with nucleobase=T}; {default with nucleobase=G}]
    "TG";

  test_case
    10
    [
      {default with nucleobase=C};
      {default with nucleobase=G};
      {default with nucleobase=C};
      {default with nucleobase=A};
      {default with nucleobase=T};
      {default with nucleobase=C};
      {default with nucleobase=C};
      {default with nucleobase=T};
      {default with nucleobase=C};
      {default with nucleobase=C}
    ]
    "CGCATCCTCC"
    [
      {default with nucleobase=G};
      {default with nucleobase=C};
      {default with nucleobase=G};
      {default with nucleobase=T};
      {default with nucleobase=A};
      {default with nucleobase=G};
      {default with nucleobase=G};
      {default with nucleobase=A};
      {default with nucleobase=G};
      {default with nucleobase=G}
    ]
    "GCGTAGGAGG";

  test_case
    10
    [
      {default with nucleobase=G};
      {default with nucleobase=A};
      {default with nucleobase=T};
      {default with nucleobase=T};
      {default with nucleobase=T};
      {default with nucleobase=A};
      {default with nucleobase=A};
      {default with nucleobase=A};
      {default with nucleobase=G};
      {default with nucleobase=A}
    ]
    "GATTTAAAGA"
    [
      {default with nucleobase=C};
      {default with nucleobase=T};
      {default with nucleobase=A};
      {default with nucleobase=A};
      {default with nucleobase=A};
      {default with nucleobase=T};
      {default with nucleobase=T};
      {default with nucleobase=T};
      {default with nucleobase=C};
      {default with nucleobase=T}
    ]
    "CTAAATTTCT";
  *)


  (* ocamlopt rna.ml && ./a.out *)
