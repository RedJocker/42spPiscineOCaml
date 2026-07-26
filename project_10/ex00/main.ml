let _ =
  (List.map Color.toString Color.all)
  |> List.iter print_endline;
  List.map Color.toStringVerbose Color.all
  |> List.iter print_endline;
