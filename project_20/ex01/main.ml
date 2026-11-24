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
