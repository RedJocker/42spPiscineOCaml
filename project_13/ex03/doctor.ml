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
