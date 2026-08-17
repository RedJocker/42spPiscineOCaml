(*

  Exercise 02: You are a good Daaaaaalek!
Turn-in directory : ex02/
Files to turn in : dalek.ml, doctor.ml, people.ml, main.ml, Makefile
Allowed functions : Pervasives, String and Random modules
• Write a class dalek that has the following attributes:
◦ A ‘name‘ attribute of type ‘string‘ randomly generated with the format: DalekXXX,
where XXX is a random set of characters (e.g., DalekSec).
◦ An ‘hp‘ attribute of type ‘int‘, initialized to 100.
◦ A ‘shield‘ attribute of type ‘bool‘, mutable, initialized to ‘true‘ and changes
its value each time the ‘exterminate‘ method is used.
◦ A ‘to_string‘ method that returns the name of the object along with attribute
values.
◦ A ‘talk‘ method that randomly prints one of the following strings to the stan-
dard output:
∗ Explain! Explain!
∗ Exterminate! Exterminate!
∗ I obey!
∗ You are the Doctor! You are the enemy of the Daleks!
◦ An ‘exterminate‘ method that takes a ‘people‘ object as an argument and kills
it instantly.
◦ A ‘die‘ method that prints the following sentence to the standard output:
Emergency Temporal Shift!

• Simulate a battle between the Doctor, a Dalek, and a human in the main to provide
sufficient testing for the evaluation. Feel free to add any setters you need.

  *)


class dalek =
  let rec random_char () =
    let ch = Random.int_in_range
      ~min:48 ~max:122
      |> Char.chr 
    in
    if Char.Ascii.is_alphanum ch then ch else random_char ()
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
