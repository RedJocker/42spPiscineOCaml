(*

Exercise 00: Do What I do. Hold tight and pretend it’s a plan!
Turn-in directory : ex00/
Files to turn in : people.ml, main.ml, Makefile
Allowed functions : Stdlib modules
• Write a class people that has the following attributes:
◦ A name attribute of type string.
◦ An hp attribute of type int, initialized to 100.
◦ A to_string method that returns a string representing the object’s name and
attribute values.
◦ A talk method that prints the following string to the standard output:
I’m [NAME]! Do you know the Doctor?
◦ A die method that prints the following sentence to the standard output:
Aaaarghh!
◦ An initializer message that indicates the object has been created (feel free to
make this message creative or descriptive!).
• You must simulate all the methods in the main.ml file to provide sufficient testing
for the defence.

  *)

class people (name: string) = object (this)

  val name = name
  val mutable hp = 100

  initializer 
    Printf.printf "Spawining %s\n%!" this#to_string
  method to_string = Printf.sprintf "%s(hp=%d)" name hp
  method talk = Printf.printf "I’m %s! Do you know the Doctor?\n%!" name
  method die = print_endline "Aaaarghh!"
  
end
