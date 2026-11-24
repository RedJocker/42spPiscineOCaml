let _ =
  
  let methane = new Alkane.methane in
  let ethane = new Alkane.ethane in

  let comb = new AlkaneCombustion.alkane_combustion [methane] in
  Printf.printf "%s\n" comb#to_string;
  let comb = comb#balance in
  Printf.printf "%s\n" comb#to_string;
  print_endline "";

  let comb = new AlkaneCombustion.alkane_combustion [ethane] in
  Printf.printf "%s\n" comb#to_string;
  let comb = comb#balance in
  Printf.printf "%s\n" comb#to_string;
  print_endline "";
  
  
  let comb = new AlkaneCombustion.alkane_combustion [methane; ethane] in
  Printf.printf "%s\n" comb#to_string;
  let comb = comb#balance in
  Printf.printf "%s\n" comb#to_string;
  print_endline "";
