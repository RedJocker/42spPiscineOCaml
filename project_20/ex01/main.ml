let _ =
  
  let water = new Molecule.water in
  Printf.printf "%s\n" water#to_string;
  print_endline "";

  let carbon_dioxyde = new Molecule.carbon_dioxyde in
  Printf.printf "%s\n" carbon_dioxyde#to_string;
  print_endline "";

  let tnt = new Molecule.tnt in
  Printf.printf "%s\n" tnt#to_string;
  print_endline "";

  let ethanol = new Molecule.ethanol in
  Printf.printf "%s\n" ethanol#to_string;
  print_endline "";

  let methane = new Molecule.methane in
  Printf.printf "%s\n" methane#to_string;
  print_endline "";

  Printf.printf "methane == ethanol? %B\n" (methane#equals ethanol);
  Printf.printf "ethanol == methane? %B\n" (ethanol#equals methane);
  print_endline "";
  
  let tnt2 = new Molecule.tnt in
  Printf.printf "tnt  == tnt2? %B\n" (tnt#equals tnt2);
  Printf.printf "tnt2 == tnt ? %B\n" (tnt2#equals tnt);
  print_endline "";
