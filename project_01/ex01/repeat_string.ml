(*

Turn-in directory : ex01/
Files to turn in : repeat_string.ml
Allowed functions : None
You will write a function named repeat_string, which takes two arguments:
• A string named str
• An integer named n
The function will, of course, return str repeated n times. It must be possible to omit
str, in which case your function must behave like repeat_x as stated in the previous
exercise. The type of your function will be ?str:string -> int -> string.
If the argument given to the function is negative, the function must behave like
repeat_x as stated in the previous exercise.
Example:
# repeat_string (-1);;
- : string = "Error"
# repeat_string 0;;
- : string = ""
# repeat_string ~str:"Toto" 1;;
- : string = "Toto"
# repeat_string 2;;
- : string = "xx"
# repeat_string ~str:"a" 5;;
- : string = "aaaaa"
# repeat_string ~str:"what" 3;;
- : string = "whatwhatwhat"

  *)


let rec repeat_string ?(str="x") = function
  | n when n <= 0 -> ""
  | n -> str^(repeat_string ~str:(str) (n - 1))


let () =
  let assertEquals tested expected actual =
    let case = Printf.sprintf "(%s, %d)" (Pair.fst tested) (Pair.snd tested) in
    Printf.printf "TestCase %s: " case;
  if expected <> actual then
    Printf.printf "[FAIL]\nexpected:%s\nactual:%s\n" expected actual
  else
    Printf.printf "[OK]\n"
  in

  let tested = ("-._.-", 4) in
  let expected = "-._.--._.--._.--._.-" in
  let actual = repeat_string ~str:(Pair.fst tested) (Pair.snd tested) in
  assertEquals tested expected actual;

  let tested = ("-._.-", 0) in
  let expected = "" in
  let actual = repeat_string ~str:(Pair.fst tested) (Pair.snd tested) in
  assertEquals tested expected actual;

  let tested = ("default", 4) in
  let expected = "xxxx" in
  let actual = repeat_string (Pair.snd tested) in
  assertEquals tested expected actual;

  let tested = ("default", 20) in
  let expected = "xxxxxxxxxxxxxxxxxxxx" in
  let actual = repeat_string (Pair.snd tested) in
  assertEquals tested expected actual;

