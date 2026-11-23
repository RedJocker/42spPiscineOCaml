(*

  You will write a virtual class named atom for... an atom. Today we’re doing some
  chemistry! An atom is defined by a few things:
  • A name, of type string.
  • A symbol, of type string.
  • An atomic_number, of type int.
  All these attributes have to be methods, and you have to be able to set what they
  return through the atom’s constructor. Of course, feel free to look up what an atom is if
  you don’t understand what these are.
  Your class will contain at least a to_string method to briefly describe your atom
  and an equals method to compare atoms; aside from that, you can implement anything
  you think is suitable or may need in the next exercises.
  To go along with your atom virtual class, you will also write some real atoms, which
  will, of course, inherit your atom class. You have to write at least the following classes:
  • hydrogen
  • carbon
  • oxygen
  • And three more atoms of your choice, as long as they really exist.

  *)

class virtual atom (name: string) (symbol: string) (atomic_number: int) = object (this)

  val name = name
  val symbol = symbol
  val atomic_number = atomic_number
  
  method to_string = Printf.sprintf "Atom(%s, %s, %d)" this#name this#symbol this#atomic_number
  method name = name
  method symbol = symbol
  method atomic_number = atomic_number
    
end

let compare (atom_a: atom) (atom_b: atom) = match (atom_a#symbol, atom_b#symbol) with
  | a, b when a = b -> 0
  | "C", _ -> -1
  | _ , "C" -> 1
  | "H", _ -> -1
  | _ , "H" -> 1
  | a, b -> Stdlib.compare a b

class hydrogen = object(this)
  inherit atom "Hydrogen" "H" 1
end

class carbon = object(this)
  inherit atom "Carbon" "C" 6
end

class oxygen = object(this)
  inherit atom "Oxygen" "O" 8
end

class lithium = object(this)
  inherit atom "Lithium" "Li" 3
end

class sodium = object(this)
  inherit atom "Sodium" "Na" 11
end

class potassium = object(this)
  inherit atom "Potassium" "K" 19
end
