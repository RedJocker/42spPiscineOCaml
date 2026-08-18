(*

A tribute to polyph...poly...you know, that polysleep thingie.

Turn-in directory : ex00/
Files to turn in : micronap.ml
Allowed functions : The Sys and Array modules
You will write a program that takes an integer command line argument. This argu-
ment will be the number of seconds your program will wait before exiting. An invalid
or missing argument will cause the program to quit immediately; no specific output is
expected.
You must use the following function my_sleep to do the actual waiting:
let my_sleep () = Unix.sleep 1
You will need to include this function along with your work. Feel free to sleep while
your program is running, if you need to.
  You might have to do something “special” to compile this exercise

  *)



let my_sleep () = Unix.sleep 1

let micronap = function
  | seconds when seconds <= 0 -> ()
  | seconds ->
     for i = 1 to seconds do
       my_sleep ()
     done

let _ =
  let seconds = try (
    if Array.length Sys.argv = 2 then    
      let arg1 = Array.get Sys.argv 1 in
      int_of_string arg1
    else
      0
  ) with _ -> 0
  in
  micronap seconds

(* ocamlopt -I +unix unix.cmxa micronap.ml && ./a.out 3 *)
