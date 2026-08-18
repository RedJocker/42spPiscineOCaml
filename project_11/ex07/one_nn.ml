(*
This is very interesting! But you have to do it anyway.
Turn-in directory : ex07/
Files to turn in : one_nn.ml
Forbidden functions : None
Here we go! Now that we have our examples, we can make some predictions. This
exercise is where you’re going to implement the K-nearest neighbors algorithm — or
K-nn for short.
Do you remember our radars? Our radars can be either good or bad. Our objective
will be to use the stats describing the radars. Let’s say you’re trying to guess the type
of a radar A. You know that A is bad. You know of a radar named B that looks a lot like
A. That means B is probably bad, right? That’s the spirit of the K-nn algorithm. You
pick the K nearest radars to the one you’re trying to guess, and you can say good or bad
depending on whether there are more good radars or bad radars.
And how do you know two radars are close to each other? Duh! You know how to
compute a Euclidean distance, right? . . . Right?
But right now, that sounds like a lot to do. It’s complicated, and I know you’re tired.
So we’re going to implement that with just ONE nearest neighbor. 1-nn. Your function
will do just that: guess if the radar you give it is good or bad, using the type of the
nearest radar.
That means your function’s type will be radar list -> radar -> string, with
type radar = float array * string. But I bet you already figured that out yourself.
You are not required to handle the case when the radars have different vector lengths or
when the training set is empty.

As usual, don’t forget your tests. It could be interesting to show
that your one-nn can guess correctly but also make mistakes! Feel
free to use your examples_of_file function to provide examples, or
write in-memory examples if you couldn’t solve the previous exercise.
Your one-nn has to be able to handle any class (not just g or b) and
any vector length, as long as it’s the same for all radars.
This exercise is not mandatory

 *)


let eu_dist arr0 arr1 =
  Array.map2 ( -. ) arr0 arr1
  |> Array.map abs_float
  |> Array.map (fun x -> x ** 2.0)
  |> Array.fold_left ( +. ) 0.0


let examples_of_file path =
  let line_parse line =
    String.split_on_char ',' line
    |> List.map String.trim
    |> begin
        fun split_line ->
          let len_lst = List.length split_line in
          let numbers =
            List.take (len_lst - 1) split_line
            |> List.map float_of_string
            |> Array.of_list 
          in
          let clazz = List.drop (len_lst - 1) split_line |> List.hd in
          (numbers, clazz)
      end
  in
  let read_file_in_channel ic =
    let file_contents = In_channel.input_lines ic in

    let split_commas = List.map line_parse file_contents in
    split_commas
  in
  try
    In_channel.with_open_text path read_file_in_channel
  with _ -> begin
      []
    end

type radar = float array * string

let one_nn (radars: radar list) ((p_arr, p_c): radar) : string =
  let sorted =
    radars
    |> List.map begin fun ((l_arr, l_c) as l_radar) -> 
         let dist = eu_dist l_arr p_arr in
         (dist, l_radar)
         end
    |> List.sort begin fun (dist_a, _) (dist_b, _) ->
           Float.compare dist_a dist_b
         end
  in
  match sorted with
  | [] -> "empty"
  | (_,(_,label))::_ -> label 
