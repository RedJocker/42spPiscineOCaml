module Joke : sig
  val print_random_joke : unit -> unit
end = struct
  type joke = { id: int; content: string }
  type repository = { mutable jokes: joke array }

  let default_jokes = [|
    {id=0; content="joke0"};
    {id=1; content="joke1"};
    {id=2; content="joke2"};
    {id=3; content="joke3"};
    {id=4; content="joke4"};
  |]

  let repository = { jokes=default_jokes }

  let joke_from_string string_joke =
    Scanf.sscanf string_joke "%d,%s" (fun id joke ->
      {id; content=joke}
    )

  let print_joke joke =
    print_endline joke.content

  let jokefile_read jokefile =
    let read_file_in_channel ic =
      let jokefile_contents = In_channel.input_lines ic in

      let jokes = match jokefile_contents with
      | "id,joke"::jokes -> jokes
      | _ -> (Printf.printf "Invalid header on file %s\n%!" jokefile; [])
      in

      if jokes = []
      then
	(print_endline "File was invalid or had no jokes, using default jokes";
	repository)
      else
	(Printf.printf "Loading jokes from file: %s\n%!" jokefile;
	try
	  (let jokes = List.map joke_from_string jokes in
	  {jokes=Array.of_list jokes})
	with _ ->
	  Printf.printf "Failed to read jokes, using default jokes\n%!";
	  repository)
    in
    try
      In_channel.with_open_text jokefile read_file_in_channel
    with _ -> (
      Printf.printf "Failed to open file %s, using default jokes\n%!" jokefile;
      repository)

  let () =
    let jokefile =
      if Array.length Sys.argv = 2 then
	let arg1 = Array.get Sys.argv 1 in
	arg1
      else
	(print_endline "Using default jokefile jokes.jk";
	"jokes.jk")
  in
    let load_repository = jokefile_read jokefile in
    repository.jokes <- load_repository.jokes


  let print_random_joke () =
    let index = Random.int_in_range
      ~min:0
      ~max:((Array.length repository.jokes) - 1)
    in
    let joke = Array.get repository.jokes index in
    print_joke joke
end


let _ =
  Random.self_init ();
  print_endline "";
  Joke.print_random_joke ()
