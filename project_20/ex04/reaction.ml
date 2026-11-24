(*

  Exercise : 03
  Neanderthals developed tools, we built a wall (we built the pyramids!)
  Turn-in directory : ex03/
  Files to turn in : *.ml, Makefile
  Forbidden functions : None
  Now we’re doing interesting things! Chemical reactions. A chemical reaction is a
  simple thing, really. You have some molecules at the start of your reaction, and you
  have... some molecules at the end. There is only one really important rule to follow,
  which is: you must have the same exact amount of atoms at the start and at the end
  of your reaction. “Nothing is lost, nothing is created, everything is transformed.” If
  you’ve never heard that sentence, look up Antoine de Lavoisier and what he has done for
  chemistry.
  Basically, you can instantiate a new chemical reaction with a list of molecules. Then
  you can call a get_result method to get the result.
  All that explanation was good, but now let’s talk about code! You will write a virtual
  class named reaction, which will be instantiated with two collections of molecules, one
  for the start and one for the end of your reaction.
  Your class will also provide the following virtual methods:
  • get_start: (molecule * int) list
  • get_result: (molecule * int) list
  • balance: reaction
  • is_balanced: bool
  Your class doesn’t have to do anything: all you have to provide is the structure. But
  if you think hard enough, you might realize that some behavior is common to all possible
  chemical reactions. Feel free to implement those you feel useful to implement. *wink
  wink*
  
 *)

class virtual reaction
                (start : (Molecule.molecule * int) list)
                (result: (Molecule.molecule * int) list) = object(this)
  val start = start  
  val result = result

  method get_start = start
  method get_result = result
  method virtual balance: reaction
  method virtual is_balanced: bool
  method to_string =
    let start = start
                |> List.map (fun (m,c) -> (string_of_int c)^"("^m#formula^")")
                |> String.concat " + "  
    in
    let result = result
                 |> List.map (fun (m,c) -> (string_of_int c)^"("^m#formula^")")
                 |> String.concat " + "  
    in
    let balanced = if this#is_balanced
                   then " --balanced--> "
                   else " --unbalanced--> "
    in
    start ^ balanced ^ result
end
