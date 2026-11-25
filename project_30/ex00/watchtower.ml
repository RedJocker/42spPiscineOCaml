(*
Exercise : 00
Exercise 00: All Along the Watchtower
Turn-in directory : ex00/
Files to turn in : *.ml, Makefile
Forbidden functions : None
In this exercise, you will implement a basic monoid named Watchtower. This monoid
is an implementation of Brian Beckman’s concept of clock monoid.
Your monoid will contain:
• A type hour as an alias for type int.
• A zero.
• An add rule to add hours according to the concept of a 12-hour clock.
• A sub rule to subtract hours according to the concept of a 12-hour clock.
Your monoid will have the following signature:

module Watchtower :
  sig
    type hour = int
    val zero : hour
    val add : hour -> hour -> hour
    val sub : hour -> hour -> hour
  end

You must provide sufficient testing for your evaluation session.

  *)

type hour = int

let zero = 0

let add (a : hour) (b : hour) =
  let (%) x y = ((x mod y) + y) mod y in
  (a + b) % 12

let sub (a : hour) (b : hour) =
  let (%) x y = ((x mod y) + y) mod y in
  let inverse x = match (x % 12) with
    (* add x (inverse x) = zero *)
    | 0 -> 0
    | 1 -> 11
    | 2 -> 10
    | 3 -> 9
    | 4 -> 8
    | 5 -> 7
    | 6 -> 6
    | 7 -> 5
    | 8 -> 4
    | 9 -> 3
    | 10 -> 2
    | 11 -> 1
    | _ -> failwith "impossible"
  in
  let b_inverse = inverse b in
  add a b_inverse
