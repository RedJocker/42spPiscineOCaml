let _ =
  let person = new People.people "Horace" in
  person#talk;
  person#die;
  print_endline person#to_string;
  print_endline "";
  let person = new People.people "Hortence" in
  person#talk;
  person#die;
  print_endline person#to_string;
  print_endline "";
