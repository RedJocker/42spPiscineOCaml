let _ =
  Random.self_init ();
  let person = new People.people "Horace" in
  person#talk;
  person#die;
  print_endline person#to_string;
  print_endline "";

  let doctor = new Doctor.doctor "Hortence" 30 person in
  doctor#talk;
  print_endline doctor#to_string;
  doctor#use_sonic_screwdriver;
  doctor#travel_in_time 2026 2030;
  print_endline doctor#to_string;
  print_endline "";

  let dalek = new Dalek.dalek in
  dalek#talk;
  dalek#exterminate person;
  print_endline person#to_string;
  dalek#die;
  print_endline dalek#to_string;
  print_endline "";

  let persons = new Army.army in
  let _ = persons#add person in

  let person = new People.people "Horland" in
  let _ = persons#add person in
  print_endline persons#to_string;


  let daleks = new Army.army in
  let _ = daleks#add dalek in
  print_endline daleks#to_string;

  let doctors = new Army.army in
  let _ = doctors#add doctor in
  print_endline doctors#to_string
