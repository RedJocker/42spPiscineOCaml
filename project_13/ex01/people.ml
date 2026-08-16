class people (name: string) = object (this)

  val name = name
  val mutable hp = 100

  initializer 
    Printf.printf "Spawining %s\n%!" this#to_string
  method to_string = Printf.sprintf "%s(hp=%d)" name hp
  method talk = Printf.printf "I’m %s! Do you know the Doctor?\n%!" name
  method die = print_endline "Aaaarghh!"
  method get_hp = hp
end
