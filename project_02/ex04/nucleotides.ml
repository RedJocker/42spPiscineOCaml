(*

  Exercise 04: DNA -> Nucleotides
Turn-in directory : ex04/
Files to turn in : nucleotides.ml
Allowed functions : None
The very beginning of DNA takes place in a structure consisting of a phosphate group
linked to a deoxyribose sugar, which is itself linked with a nucleobase. A list of many
structures is called a helix, and two of them make a DNA sample. Helix: P - D - Base,
P - D - Base, . . .
• Create the type phosphate, which is an alias for the string type.
• Create the type deoxyribose, which is also an alias for the string type.
• Create the variant type nucleobase. Its constructors are A, T, C, G, and None.
• Write the nucleotide type that contains three elements: one phosphate, one deoxyribose,
and one nucleobase. The structure of the type nucleotide is up to you; a record
or a tuple will do the trick.
• Write a function generate_nucleotide that returns a nucleotide from a given nu-
cleobase passed as a char. The function must be typed as val generate_nucleotide
: char -> nucleotide. Set the phosphate value to "phosphate" and the deoxyribose
  value to "deoxyribose"

  *)


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


let _ =
  let nucleotide_to_string {phosphate; deoxyribose; nucleobase} =
    let nucleobase = match nucleobase with
      | A -> "A"
      | T -> "T"
      | C -> "C"
      | G -> "G"
      | None -> "None"
    in
    Printf.sprintf "{%s; %s; %s}" phosphate deoxyribose nucleobase
  in

  let assertEquals case expected actual =
    Printf.printf "TestCase %s: " case;
    let expected_str = nucleotide_to_string expected in
    let actual_str = nucleotide_to_string actual in
  if expected <> actual then
    Printf.printf "[FAIL]\nexpected:%s\nactual:%s\n" expected_str actual_str
  else
    Printf.printf "%s [OK] \n" actual_str
  in

  let default =
    {phosphate="phosphate"; deoxyribose="deoxyribose"; nucleobase=None}
  in

  let test_case tested expected_nucleobase =
    let expected = {default with nucleobase=expected_nucleobase} in
    let actual = generate_nucleotide tested in
    let case = Printf.sprintf "(%c)" tested in
    assertEquals case expected actual;
  in

  test_case 'a' A;
  test_case 'A' A;
  test_case 'c' C;
  test_case 'C' C;
  test_case 't' T;
  test_case 'T' T;
  test_case 'g' G;
  test_case 'G' G;

  test_case 'x' None;
  test_case 'X' None;

  test_case ' ' None;
  test_case '\n' None;
