(*
  Exercise : 04
  Math, science, history unraveling the mystery...
  Turn-in directory : ex04/
  Files to turn in : *.ml, Makefile
  Forbidden functions : None
  Now that we know what a chemical reaction is, let’s do a real one! Do you remember
  the alkanes I had you write a few exercises ago? Because now we’re going to burn them.
  Our class will be named alkane_combustion, and it will be a subtype of reaction (what
  did you expect?).
  But first, let’s clear up some theory. An alkane combustion means you start with
  an alkane and some molecular oxygen, and you end up with carbon dioxide and water.
  Always. That means our reaction will be (for example, with methane):
  CH4 + O2 → CO2 + H2O
  But wait! It’s not actually that simple. I did say you had to have the same exact
  amount of atoms at the start and at the end, right? And if you look at what I just wrote,
  that doesn’t really add up: you start with 1 carbon, 4 hydrogen, and 2 oxygen, and you
  end with one carbon, 2 hydrogen, and 3 oxygen; so you end up with not enough hydrogen
  and too much oxygen.
  That’s where stoichiometric coefficients come in. Of course, don’t forget to
  look up the word if it sounded like Hebrew to you. But basically, they are coefficients
  you add to the molecules in your reaction, so that you have the same amount of atoms
  on both sides. As a result:
  CH4 + 2 O2 → CO2 + 2 H2O
  Now on both sides you have 1 carbon, 4 hydrogen, and 4 oxygen. Good, everything
  works!
  Now let’s talk about code. You will construct your class with a list of alkane objects,
  and it will implement the reaction class’s methods:
  
  • get_start: returns the list of molecules at the beginning of the reaction. Throws
  an exception if the reaction is not balanced.
  • get_result: returns the list of molecules at the end of the reaction. Throws an
  exception if the reaction is not balanced.
  • balance: returns a new alkane combustion, with the right (and smallest possible)
  stoichiometric coefficients so that your combustion is balanced.
  • is_balanced: true if your reaction is balanced, false otherwise.
  Your balance method will have to process the list of alkanes, possibly removing du-
  plicates, and then compute all the adequate coefficients for your reaction to work

 *)


class alkane_combustion
        ?(coeficients : (int list * int * int * int) option)
        (alkanes : Alkane.alkane list)
         = object(this)
  initializer
    if List.length alkanes = 0 then
      raise (Invalid_argument "cannot make an alkane combustion without alkanes")
    else match coeficients with
         | Some (cofs, _, _, _) when List.length cofs <> List.length alkanes ->
            raise (Invalid_argument
                     "size of coeficients is not equal to size of alkanes")
         | _ -> ()

  
  inherit Reaction.reaction (
    let (cofs, oxygen_cof)  = match coeficients with 
      | Some (cofs, o, c, w) -> (cofs, o)
      | _ -> (List.init (List.length alkanes) (fun a -> 1)), 1
    in
    let start = alkanes 
      |> List.map (fun al_mol -> ((al_mol :> Molecule.molecule), 1)) 
      |> List.append [new Molecule.oxygen, oxygen_cof]
    in   
    start
  ) (
    let (carbon_cof, water_cof)  = match coeficients with 
      | Some (_, _, c, w) -> (c, w)
      | _ -> 1, 1
    in
    [(new Molecule.water, water_cof); (new Molecule.carbon_dioxyde, carbon_cof)]
  ) as super

  method! get_start = if this#is_balanced
                      then super#get_start
                      else failwith "reaction is not balanced"
  method! get_result = if this#is_balanced
                      then super#get_result
                      else failwith "reaction is not balanced"
  
  method is_balanced = false
  method balance = match alkanes with
    | [] -> failwith "unexpected empty alkanes on balance"
    | [alkane] ->
       let n = alkane#get_n in
       let calc_oxygen_cof alkane_cof =
         float_of_int ((3 * alkane_cof * n) + alkane_cof) /. 2.0 
       in
       let alkane_cof =
         let rec helper a =
           let oxygen_cof = calc_oxygen_cof a in
           if (Float.floor oxygen_cof) = oxygen_cof
           then a
           else helper a + 1
         in
         helper 1
       in
       let oxygen_cof = int_of_float (calc_oxygen_cof alkane_cof) in
       let carbon_dioxyde_cof =  n * alkane_cof in
       let water_cof = (n + 1) * alkane_cof in
       let result = new alkane_combustion
         ~coeficients:([alkane_cof], oxygen_cof, carbon_dioxyde_cof, water_cof)
         alkanes
       in
       (result :> Reaction.reaction)
    | _ -> failwith "TODO2"
  
  
end
