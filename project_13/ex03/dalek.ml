class dalek =
  let is_alphanum ch = match ch with
    | ch when ch >= '0' && ch <= '9' -> true
    | ch when ch >= 'a' && ch <= 'z' -> true
    | ch when ch >= 'A' && ch <= 'Z' -> true
    | _ -> false
  in
  let rec random_char () =
    let ch = Random.int_in_range
      ~min:48 ~max:122
      |> Char.chr 
    in
    if is_alphanum ch then ch else random_char ()
  in
object (this)
  val phrases = [
    "Explain! Explain!";
    "Exterminate! Exterminate!";
    "I obey!";
    "You are the Doctor! You are the enemy of the Daleks!"
  ]
  val name = Printf.sprintf "Dalek%c%c%c"
    (random_char ()) (random_char ()) (random_char ())
  val mutable hp = 100
  val mutable shield = true
  initializer
    Printf.printf "Spawining %s\n%!" this#to_string
  method to_string = Printf.sprintf "%s(hp=%d,shield=%b)" name hp shield
  method talk =
    let phrase = List.nth phrases
      (Random.int_in_range ~min:0 ~max:(List.length phrases - 1)) 
    in
    print_endline phrase
  method die = print_endline "Emergency Temporal Shift!"
  method get_hp = hp
  method exterminate (people : People.people) =
    shield <- not shield;
    people#die
end
