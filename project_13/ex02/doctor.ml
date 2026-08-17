(*

  Exercise 01: The Name of the Doctor!
Turn-in directory : ex01/
Files to turn in : doctor.ml, people.ml, main.ml, Makefile
Allowed functions : Stdlib modules
• Write a class doctor that has the following attributes:
◦ A ‘name‘ attribute of type ‘string‘.
◦ An ‘age‘ attribute of type ‘int‘.
◦ A ‘sidekick‘ attribute of type ‘people‘.
◦ An ‘hp‘ attribute of type ‘int‘ initialized to 100.
◦ A ‘to_string‘ method that returns the name of the object along with the values
of its attributes.
◦ A ‘talk‘ method that prints the following string to the standard output:
Hi! I’m the Doctor!
◦ An initializer which indicates that the object has been created (feel free to use
something explicit and descriptive to announce it!).
◦ A ‘travel_in_time‘ method that takes two arguments of type ‘int‘: start and
arrival, and changes the Doctor’s age logically (Think before coding any un-
usual arithmetic... Please...). This method also draws a TARDIS on the
standard output. (If you don’t know what a TARDIS is, google it!)
◦ A ‘use_sonic_screwdriver‘ method that prints the following sentence to the
standard output: Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii
◦ A private ‘regenerate‘ method that sets the Doctor’s ‘hp‘ to 100 (the maxi-
mum).

• You must simulate all the methods in the main to provide sufficient testing for the
evaluation.

  *)


class doctor (name: string) (age : int) (sidekick: People.people) =
  let default_hp = 100 in
  let tardis_ascii = {delim| 
    __
   /Tel\ 
  /___. \
  |   |\ \
  |;;;|;\|
  |   | ;|
  |   |  |
  .----\ |
   \----\|
    \----.

  |delim} in
  let sonic_noise = "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii" in 
object (this)
  val name: string = name
  val mutable age : int = age
  val mutable hp : int = default_hp
  val sidekick : People.people = sidekick
  
  initializer
    Printf.printf "This is my sideckick %s\n%!" (sidekick#to_string)   

  method to_string =
    Printf.sprintf "Doctor %s(hp=%d, age=%d)" name hp age

  method talk =
    print_endline "Hi! I’m the Doctor!"

  method travel_in_time start arrival =
    age <- (age + (arrival - start));
    print_endline tardis_ascii

  method use_sonic_screwdriver =
    print_endline sonic_noise

  method private regenerate =
    hp <- default_hp
end
