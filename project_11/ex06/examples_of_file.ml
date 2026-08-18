(*

This is boring. But you have to do it anyway.
Turn-in directory : ex06/
Files to turn in : examples_of_file.ml, *.csv
Forbidden functions : None
I said we would be doing some machine learning today, and now it is time to give you
more details about what we will be doing. We are going to implement an algorithm for
supervised classification. If you don’t know what “supervised” and “classification” mean,
look it up on the Internet—your hipster entrepreneur friend who loves Node.js probably
doesn’t know either.
In this exercise, you will write a function named examples_of_file that takes a file
path as its argument and returns a set of examples read from the input file, formatted as
CSV.
Each line in the input describes a radar used to detect free electrons in the ionosphere.
A radar is described by a set of float fields, representing some complicated stats that
are not essential to understand, and a letter at the end of each line specifying whether
the radar detected evidence of free electrons in the ionosphere. Essentially, you only need
to know that a radar is defined by a vector of complicated stats and a class in the form
of a character. If you don’t know what a vector is, ask your 15-year-old sibling—they
probably know.
In other words, the type of an example will be float array * string, and your
function’s type will be string -> (float array * string) list.
For instance, 1.0,0.5,0.3,g will be converted to ([|1.0; 0.5 ;0.3 |], "g").
You can use the files named ionosphere.test.csv and/or
ionosphere.train.csv for your tests, or any file you choose, as
long as it has the same format (it doesn’t have to contain the same
number of float columns)

  
 *)


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
