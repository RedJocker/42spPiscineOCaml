(*

Exercise 03
Exercise 03: The Day of The Doctor!
Turn-in directory : ex03/
Files to turn in : army.ml, dalek.ml, doctor.ml, people.ml, main.ml, Makefile
Allowed functions : Stdlib and List modules
• Write a parameterized class army that has the following attributes:
◦ A ‘members‘ attribute of type ‘’a list‘ which contains a list of instances of one
of the three previous classes.
◦ An ‘add‘ method that adds an instance to the list (either to the front or back).
◦ A ‘delete‘ method that removes the head of the ‘members‘ list (either from the
front or back).
• Simulate the construction and destruction of an army of each type in the main to
provide sufficient testing for the evaluation.
  This exercise is not mandatory.

  *)



class ['a] army =
object (this)
  val mutable members : 'a list = []
  method add (soldier : 'a) =
    members <- (soldier :: members);
    members
  method delete = match members with
  | [] -> (None, members)
  | soldier::soldiers ->
    members <- soldiers;
    (Some soldiers, members)

  method to_string =
    let members_str =
      members
      |> List.map (fun soldier -> "\t"^soldier#to_string)
      |> List.fold_left ( ^ ) ""
    in
    "ARMY:\n"^members_str
    
end
