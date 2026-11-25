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
  = 
  let coeficients_norm =
    match coeficients with
      | Some all_cofs -> all_cofs
      | _ -> (List.init (List.length alkanes) (fun a -> 1)), 1, 1, 1
  in
  let _ = if List.length alkanes = 0 then
        raise (Invalid_argument "cannot make an alkane combustion without alkanes")
      else match coeficients_norm with
         | (alk_cofs, _, _, _) when List.length alk_cofs <> List.length alkanes ->
            raise (Invalid_argument
                     "size of coeficients is not equal to size of alkanes")
         | _ -> ()
  in
  let alkanes_with_cofs = let (alk_cofs, _, _, _)  = coeficients_norm in
                 List.map2 (fun a c -> a, c) alkanes alk_cofs
                 |> List.sort_uniq
                      (fun (a1, _) (a2, _) -> a1#compare (a2 :> Molecule.molecule))
  in
  let coeficients = let (_, o, c, w) = coeficients_norm in
                    let alk_cofs = List.map (fun (_, c) -> c) alkanes_with_cofs in
                    (alk_cofs, o, c, w)
  in
  let alkanes = List.map (fun (a, c ) -> a) alkanes_with_cofs in
  object(this)

  inherit Reaction.reaction (
    let (_, oxygen_cof, _, _)  = coeficients in
    let start =  
      List.map (fun (a, c) -> (a :> Molecule.molecule), c) alkanes_with_cofs
      |> List.append [new Molecule.oxygen, oxygen_cof]
    in   
    start
  ) (
    let (_, _, carbon_cof, water_cof)  = coeficients
    in
    [(new Molecule.water, water_cof); (new Molecule.carbon_dioxyde, carbon_cof)]
  ) as super

  method! get_start = if this#is_balanced
                      then super#get_start
                      else failwith "reaction is not balanced"
  method! get_result = if this#is_balanced
                      then super#get_result
                      else failwith "reaction is not balanced"
  
  method is_balanced = match alkanes with
    | [] -> failwith "unexpected empty alkanes on balance"
    | ([alkane])->
       let (alkane_cof, oxygen_cof, carbon_dioxyde_cof, water_cof) =
         this#calculate_coeficients alkane
       in
       ([alkane_cof], oxygen_cof, carbon_dioxyde_cof, water_cof) = coeficients
    | _ ->
       let cofs =
         this#calculate_coeficients_many alkanes
       in
       cofs = coeficients
           
  method balance = match alkanes with
    | [] -> failwith "unexpected empty alkanes on balance"
    | [alkane] ->
       let (alkane_cof, oxygen_cof, carbon_dioxyde_cof, water_cof)  =
         this#calculate_coeficients alkane
       in
       let result = new alkane_combustion
         ~coeficients:([alkane_cof], oxygen_cof, carbon_dioxyde_cof, water_cof)
         alkanes
       in
       (result :> Reaction.reaction)
    | _ ->
       let cofs =
         this#calculate_coeficients_many alkanes
       in
       let result = new alkane_combustion
         ~coeficients:(cofs)
         alkanes
       in
       (result :> Reaction.reaction)
    
  method private calculate_coeficients alkane =
    let n = alkane#get_n in
    let calc_oxygen_cof alkane_cof =
      float_of_int ((3 * alkane_cof * n) + alkane_cof) /. 2.0 
    in
    let alkane_cof =
      let rec helper a =
        let oxygen_cof = calc_oxygen_cof a in
        if (Float.floor oxygen_cof) = oxygen_cof
        then a
        else helper (a + 1)
      in
      helper 1
    in
    let oxygen_cof = int_of_float (calc_oxygen_cof alkane_cof) in
    let carbon_dioxyde_cof =  n * alkane_cof in
    let water_cof = (n + 1) * alkane_cof in
    (alkane_cof, oxygen_cof, carbon_dioxyde_cof, water_cof)

  method private calculate_coeficients_many alkanes =
    let (c_as, c_o, c_c, c_w) =
         List.map (fun alkane -> this#calculate_coeficients alkane) alkanes
         |> List.fold_left
              (fun ((acc_a : int list) , acc_o, acc_c, acc_w) (cur_a, cur_o, cur_c, cur_w) ->
                (cur_a::acc_a), acc_o + cur_o, acc_c + cur_c, acc_w + cur_w
              ) ([], 0, 0, 0)
    in
    (List.rev c_as, c_o, c_c, c_w)
end
