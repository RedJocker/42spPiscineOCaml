let _ =
  
  let methane = new Alkane.methane in
  Printf.printf "%s\n" methane#to_string;
  print_endline "";

  let ethane = new Alkane.ethane in
  Printf.printf "%s\n" ethane#to_string;
  print_endline "";

  let propane = new Alkane.propane in
  Printf.printf "%s\n" propane#to_string;
  print_endline "";

  let butane = new Alkane.butane in
  Printf.printf "%s\n" butane#to_string;
  print_endline "";

  let octane = new Alkane.octane in
  Printf.printf "%s\n" octane#to_string;
  print_endline "";

  let methane_mol = new Molecule.methane in
  
  Printf.printf "methane     == methane_mol ? %B\n" (methane#equals methane_mol);
  Printf.printf "ethanol_mol == methane     ? %B\n" (methane_mol#equals methane);
  print_endline "";

  let octane_inline =
    object(this)
      inherit Alkane.alkane "Inline Octane" 8
    end
  in

  Printf.printf "octane        == octane_inline ? %B\n" (octane#equals octane_inline);
  Printf.printf "octane_inline == octane        ? %B\n" (octane_inline#equals octane);
  print_endline "";
