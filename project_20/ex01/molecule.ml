(*
  Turn-in directory : ex01/
  Files to turn in : *.ml, Makefile
  Forbidden functions : None
  Now that you have your atoms, you can make some molecules! If you don’t know
  what a molecule is, please look it up before beginning this exercise.
  Your molecule class is virtual, and it needs at least the following elements:
  • A name, of type string.
  • A formula, of type string.
  All these attributes have to be methods. Of course, feel free to look up what a molecule
  is if you don’t understand what these are.
  One very important constraint: you have to store the molecule’s atoms internally, and
  this atom storage must be used to compute the molecule’s chemical formula. Also, the
  chemical formula must be formatted using the Hill notation. As such, your constructor
  will only accept a name and a list of atoms.
  The chemical formula is simple: it’s a small string that describes what atoms and how
  many of them are contained in the molecule. Let’s build an example with Trinitrotoluene
  (or TNT for short). We get the list of atoms, and we end up with:
  • 3 atoms of Nitrogen
  • 5 atoms of Hydrogen
  • 6 atoms of Oxygen
  • 7 atoms of Carbon
  We know the symbols of these atoms are N, H, O and C. All we have to do is enumerate
  their symbols and their quantities, right? So you would get something like: N3H5O6C7.
 
  But NO! WAIT! That’s not how it works. The Hill notation says we get carbon, then
  hydrogen, then everything else in alphabetical order. So the result is: C7H5N3O6. Of
  course, you can’t really write subscripts in a terminal, so writing it behind the symbol as
  it is is fine, like so: C7H5N3O6.
  Your class will contain at least a to_string method to briefly describe what your
  molecule is, and an equals method to compare molecules; aside from that, you can
  implement anything you think is suitable or may need in the next exercises.
  To go along with your molecule class, you will write some real molecules, which will,
  of course, inherit your molecule class. You have to write at least the following classes:
  • Water (H2O)
  • Carbon dioxide (CO2)
  • And three more molecules of your choice, as long as they really exist.
  Some elements, like salts, ions, and other things, are not considered
  as molecules in their strictest meaning. However, for today we’ll
  keep it simple and assume that a molecule is simply an aggregation of
  atoms, as long as its overall electrical charge is neutral
  *)

class virtual molecule (name: string) (atoms: Atom.atom list) =
object(this)
  val name = name
  val atoms = List.sort (fun a b -> Atom.compare b a) atoms
  method name = name
  method formula =
    this#atom_count_assoc
    |> List.map (fun (symbol, count) -> symbol ^ (string_of_int count))
    |> String.concat "" 
    
  method to_string = Printf.sprintf "Molecule(%s: %s)" this#name this#formula
  method equals (other: molecule) = this#formula == other#formula
  method private atom_count_assoc  =
    let rec count_helper to_count (current_symbol, count) counted =
      match to_count with
      | [] -> (current_symbol, count)::counted
      | next::rest ->
         if next#symbol = current_symbol then
           count_helper rest (current_symbol, count + 1) counted
         else
           count_helper rest (next#symbol, 1) ((current_symbol, count)::counted)
    in
    match atoms with
    | [] -> []
    | head::tail -> count_helper tail (head#symbol, 1) [] 
end

class water =
  object(this)
    inherit molecule
              "Water"
              [new Atom.oxygen; new Atom.hydrogen; new Atom.hydrogen]    
  end

class carbon_dioxyde =
  object(this)
    inherit molecule
              "Carbon Dioxyde"
              [new Atom.carbon; new Atom.oxygen; new Atom.oxygen]
  end

class tnt =
  object(this)
    inherit molecule
              "TNT"
              [new Atom.nitrogen;
               new Atom.nitrogen;
               new Atom.nitrogen;
               new Atom.hydrogen;
               new Atom.hydrogen;
               new Atom.hydrogen;
               new Atom.hydrogen;
               new Atom.hydrogen;
               new Atom.carbon;
               new Atom.carbon;
               new Atom.carbon;
               new Atom.carbon;
               new Atom.carbon;
               new Atom.carbon;
               new Atom.carbon;
               new Atom.oxygen;
               new Atom.oxygen;
               new Atom.oxygen;
               new Atom.oxygen;
               new Atom.oxygen;
               new Atom.oxygen]
  end

class ethanol =
  object(this)
    inherit molecule
              "Ethanol"
              [new Atom.hydrogen;
               new Atom.hydrogen;
               new Atom.hydrogen;
               new Atom.hydrogen;
               new Atom.hydrogen;
               new Atom.hydrogen;
               new Atom.carbon;
               new Atom.carbon;
               new Atom.oxygen]
  end

class methane =
  object(this)
    inherit molecule
              "Methane"
              [new Atom.hydrogen;
               new Atom.hydrogen;
               new Atom.hydrogen;
               new Atom.hydrogen;
               new Atom.carbon]
  end
