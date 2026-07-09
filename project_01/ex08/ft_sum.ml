(*

ft_sum.ml
Allowed functions : None
Starting from now, we’re going to do some maths, for a change. You’ve seen some
math before in your life, right? We’re going to do a summation function, named ft_sum,
which works like Σ. If you don’t know what the big scary E-like symbol is, it’s called a
sigma, and you can look it up. Your function will accept the following arguments:
1. An expression to add: since its value usually depends on the index, that means it
will be a function taking the index as a parameter
2. The index’s lower bound of summation
3. The index’s upper bound of summation
Your function’s type will be: (int -> float) -> int -> int -> float, and it will
be tail-recursive. No tail recursion, no points. For example, the following expression:
10∑
i=1
i2
Will be computed using your function as:
# ft_sum (fun i -> float_of_int (i * i)) 1 10;;
- : float = 385.
If the upper bound is less than the lower bound, ft_sum must return nan.
22
OCaml Recursion and higher-order functions - 0
  This exercise is not mandatory.

  *)

let ft_sum f lower upper =
  let rec loop i acc = match i with
  | i when i > upper -> acc
  | i -> loop (i + 1) (acc + (f i))
  in
  loop lower 0


(*
  TODO TEST


# val ft_sum : (int -> int) -> int -> int -> int = <fun>
# ft_sum (fun x -> x + 1) 1 2
- : int = 5
# ft_sum (fun x -> x ) 0 1
- : int = 1
# ft_sum (fun x -> x ) 0 2
- : int = 3
# ft_sum (fun x -> x ) 0 3
- : int = 6
# ft_sum (fun x -> x ) 0 4
- : int = 10
# ft_sum (fun x -> x ) 0 5
- : int = 15
# Seq.ints 1 |> Seq.take 10 |> Seq.map (ft_sum (fun x -> x * x) 0) |> List.of_seq;;
- : int list = [1; 5; 14; 30; 55; 91; 140; 204; 285; 385]
  #

  *)
