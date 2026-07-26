(*
Exercise 00: Cards colors
Turn-in directory : ex00/
Files to turn in : Color.ml and main.ml
Allowed functions : Nothing
Regular playing cards fit nicely as a programming topic when dealing with modules
and nested modules. Colors, values, cards, and decks, all tied together in a smart design.
As a start, we need to represent cards colors, namely spade, heart, diamond, and
club, as an OCaml type and equip that type with relevant values and functions.
Write the file Color.ml that respects the following interface:
type t = Spade | Heart | Diamond | Club
val all : t list (** The list of all values of type t *)
val toString : t -> string (** "S", "H", "D" or "C" *)
val toStringVerbose : t -> string (** "Spade", "Heart", etc *)
Provide some tests in the file main.ml to demonstrate that your Color module works
as intended.
  *)

(* module type Suit = sig *)
(*   type t = Spade | Heart | Diamond | Club *)
(*   val all : t list (\** The list of all values of type t *\) *)
(*   val toString : t -> string (\** "S", "H", "D" or "C" *\) *)
(*   val toStringVerbose : t -> string (\** "Spade", "Heart", etc *\) *)
(* end *)


type t = Spade | Heart | Diamond | Club
let all = [Spade; Heart; Diamond; Club]
let toString = function
  | Spade -> "S"
  | Heart -> "H"
  | Diamond -> "D"
  | Club -> "C"
let toStringVerbose = function
  | Spade -> "Spade"
  | Heart -> "Heart"
  | Diamond -> "Diamond"
  | Club -> "Club"
