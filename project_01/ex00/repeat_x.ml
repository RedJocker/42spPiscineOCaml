
(*

  Turn-in directory : ex00/
  Files to turn in : repeat_x.ml
  Allowed functions : None
  You will write a function named repeat_x, which takes an int argument named n
  and returns a string containing the character ’x’ repeated n times.
  Obviously, the type of your function will be int -> string.
  If the argument given to the function is negative, the function must return "Error".
  Example:
  # repeat_x (-1);;
  - : string = "Error"
  # repeat_x 0;;
  - : string = ""
  # repeat_x 1;;
  - : string = "x"
  # repeat_x 2;;
  - : string = "xx"
  # repeat_x 5;;
  - : string = "xxxxx"
  *)

  


let rec repeat_x = function
  | x when x < 0 -> "Error"
  | 0 -> ""
  | x -> "x"^(repeat_x (x - 1))

let () =
  let assertEquals tested expected actual =
    
    let case = Printf.sprintf "(%d)" tested in
    Printf.printf "TestCase%s: %s : " case actual;
  if expected <> actual then
    Printf.printf "[FAIL]\nexpected:%s\nactual:%s\n" expected actual
  else
    Printf.printf "[OK]\n"
  in

  let tested = 4 in
  let expected = "xxxx" in
  let actual = repeat_x tested in
  assertEquals tested expected actual;

  let tested = 3 in
  let expected = "xxx" in
  let actual = repeat_x tested in
  assertEquals tested expected actual;

  let tested = ~-1 in
  let expected = "Error" in
  let actual = repeat_x tested in
  assertEquals tested expected actual;

  let tested = 0 in
  let expected = "" in
  let actual = repeat_x tested in
  assertEquals tested expected actual;

  let tested = 10 in
  let expected = "xxxxxxxxxx" in
  let actual = repeat_x tested in
  assertEquals tested expected actual;
