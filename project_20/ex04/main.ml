let _ =
  
  let methane = new Alkane.methane in

  let comb = new AlkaneCombustion.alkane_combustion [methane] in
  Printf.printf "%s\n" comb#to_string;
  print_endline "";
  
  let comb = comb#balance in
  Printf.printf "%s\n" comb#to_string;
  print_endline "";
