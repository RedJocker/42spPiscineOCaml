
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
  
