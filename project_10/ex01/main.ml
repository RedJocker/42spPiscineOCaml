let _ =
  (List.map Value.toString Value.all)
  |> List.iter print_endline;

  print_endline "====";
  List.map Value.toStringVerbose Value.all
  |> List.iter print_endline;

  print_endline "====";
  let try_next value =
    let value_str = Value.toString value in
    let next_str = try
        Value.toString (Value.next value)
      with Invalid_argument exception_str -> "exception: "^exception_str
    in
    Printf.sprintf "value %s -> next %s" value_str next_str
  in

  Value.all
  |> List.map try_next
  |> List.iter print_endline;

  print_endline "====";
  let try_previous value =
    let value_str = Value.toString value in
    let next_str = try
        Value.toString (Value.previous value)
      with Invalid_argument exception_str -> "exception: "^exception_str
    in
    Printf.sprintf "value %s -> previous %s" value_str next_str
  in
  Value.all
  |> List.map try_previous
  |> List.iter print_endline;
