(*
Exercise 05
You didn’t actually think I was that kind, did you?
Turn-in directory : ex05/
Files to turn in : eu_dist.ml
Allowed functions : The Array module
I mean come on, you know I’m much meaner than that. Now let’s do some funny
stuff. In the next series of exercises we’ll try to do some machine learning. If you don’t
know what machine learning is, look it up on Wikipedia or ask your hipster entrepreneur
NodeJS friend.
But first, we need to do the basic things. You will write a function named eu_dist
which takes two points and calculates the Euclidian distance between them. If you don’t
know what the Euclidian distance is, here it is: if we consider a a point as an array of
coordinates a1, a2, a3...an and b another point as an array of coordinates b1, b2, b3...bn, the
Euclidian distance between a and b is:
                   n
eu_dist(a, b) = √ (∑ (ai − bi)^2)
                   i=1

Our model for a point will be an array of floating-point numbers, with each cell con-
taining the coordinate in a given dimension.
Your function’s domain will be: eu_dist : RD × RD → R+, D ∈ N∗.
Your function’s type will be: float array -> float array -> float. You don’t
have to handle cases with two vectors having different lengths.
Okay, now you should start to understand that machine learning is not just a buzz
word. It’s mostly math. And it’s just the beginning. Still with me ?

 *)


let eu_dist arr0 arr1 =
  Array.map2 ( -. ) arr0 arr1
  |> Array.map abs_float
  |> Array.map (fun x -> x ** 2.0)
  |> Array.fold_left ( +. ) 0.0
