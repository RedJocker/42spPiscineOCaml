let _ =
  Random.self_init ();
  let person = new People.people "Horace" in
  person#talk;
  person#die;
  print_endline person#to_string;
  print_endline "";

  let dalek = new Dalek.dalek in
  dalek#talk;
  dalek#exterminate person;
  dalek#die;
  print_endline dalek#to_string;
  print_endline "";
  
  let doctor = new Doctor.doctor "Hortence" 30 person in
  doctor#talk;
  print_endline doctor#to_string;
  doctor#use_sonic_screwdriver;
  doctor#travel_in_time 2026 2030;
  print_endline doctor#to_string;
  print_endline "";
