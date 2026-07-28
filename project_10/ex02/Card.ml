(*

Exercise 02: Cards
Turn-in directory : ex02/
Files to turn in : Card.ml and main.ml
Allowed functions : invalid_arg, Printf.sprintf and the List module
We have colors and values; now we can have cards! Write the file Card.ml that adheres
to the interface below. Several points to note regarding this interface:
• The Card module embeds the Color and Value modules. Just copy your previous
code in the corresponding structures.
• The type Card.t is abstract. That means you’re free to implement it as you want.
Choose wisely, some solutions are better than otters. And otters are cute.
• All values’ and functions’ types and identifiers are self explainatory. Just read and
use your brain, no tricks here.
• The function toString : t -> string returns strings like: "2S", "10H", "KD",
...
• The function toStringVerbose : t -> string returns strings like: "Card(7,
Diamond)", "Card(Jack, Club)", "Card(As, Spade)", ...
• The function compare : t -> t -> int behaves like the Pervasives compare
function.
• The functions max and min return the first parameter if the two cards are equal.
• The function best : t list -> t calls invalid_arg if the list is empty. If two or
more cards are equal in value, return the first one. True coders use List.fold_left
to do this function.
Provide some tests in the file main.ml to demonstrate that your Card, Card.Color,
and Card.Value modules function as intended.


module Color :
sig
type t = Spade | Heart | Diamond | Club
val all : t list
val toString : t -> string
val toStringVerbose : t -> string
end

module Value :
sig
type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As
val all : t list
val toInt : t -> int
val toString : t -> string
val toStringVerbose : t -> string
val next : t -> t
val previous : t -> t
end

type t
val newCard : Value.t -> Color.t -> t
val allSpades : t list
val allHearts : t list
val allDiamonds : t list
val allClubs : t list
val all : t list
val getValue : t -> Value.t
val getColor : t -> Color.t
val toString : t -> string
val toStringVerbose : t -> string
val compare : t -> t -> int
val max : t -> t -> t
val min : t -> t -> t
val best : t list -> t
val isOf : t -> Color.t -> bool
val isSpade : t -> bool
val isHeart : t -> bool
val isDiamond : t -> bool
  val isClub : t -> boo


  *)


module Color = struct
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
end

module Value = struct
  type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

  (** The list of all values of type t *)
  let all = [T2 ; T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King ; As]

  (** Interger representation of a card value, from 1 for T2 to 13 for As *)
  let toInt = function
    | T2 -> 1
    | T3 -> 2 
    | T4 -> 3
    | T5 -> 4
    | T6 -> 5
    | T7 -> 6
    | T8 -> 7
    | T9 -> 8
    | T10 -> 9
    | Jack -> 10
    | Queen -> 11
    | King -> 12
    | As -> 13
  
  (** returns "2", ..., "10", "J", "Q", "K" or "A" *)
  let toString = function
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | As -> "A"
  
  (** returns "2", ..., "10", "Jack", "Queen", "King" or "As" *)
  let toStringVerbose = function
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "Jack"
    | Queen -> "Queen"
    | King -> "King"
    | As -> "Ace"

  (** Returns the next value, or calls invalid_arg if argument is As *)
  let next = function
    | T2 -> T3
    | T3 -> T4
    | T4 -> T5
    | T5 -> T6
    | T6 -> T7
    | T7 -> T8
    | T8 -> T9
    | T9 -> T10
    | T10 -> Jack
    | Jack -> Queen
    | Queen -> King
    | King -> As
    | As -> invalid_arg "As is the highest card"

  (** Returns the previous value, or calls invalid_arg if argument is T2 *)
  let previous = function
    | T2 -> invalid_arg "2 is the lowest card"
    | T3 -> T2
    | T4 -> T3
    | T5 -> T4
    | T6 -> T5
    | T7 -> T6
    | T8 -> T7
    | T9 -> T8
    | T10 -> T9
    | Jack -> T10
    | Queen -> Jack
    | King -> Queen
    | As -> King

end


type t = {value: Value.t; color: Color.t}
let newCard : Value.t -> Color.t -> t = fun value color ->
  {value; color}
  
let allSpades : t list =
  Value.all
  |> List.map (fun value -> newCard value Color.Spade)

let allDiamonds : t list =
  Value.all
  |> List.map (fun value -> newCard value Color.Diamond)

let allClubs : t list =
  Value.all
  |> List.map (fun value -> newCard value Color.Club)

let allHearts : t list =
  Value.all
  |> List.map (fun value -> newCard value Color.Heart)

let all : t list =
  Color.all
  |> List.concat_map
    (fun color -> match color with
    | Color.Spade -> allSpades
    | Color.Heart -> allHearts
    | Color.Diamond -> allDiamonds
    | Color.Club -> allClubs)

let getValue {value: Value.t} = value
let getColor {color: Color.t} = color

let toString {value: Value.t; color: Color.t} =
  let colorStr = Color.toString color in
  let valueStr = Value.toString value in
  Printf.sprintf "Card(%s;%s)" colorStr valueStr

let toStringVerbose {value: Value.t; color: Color.t} =
  let colorStr = Color.toStringVerbose color in
  let valueStr = Value.toStringVerbose value in
  Printf.sprintf "Card(%s;%s)" colorStr valueStr

let compare card_a card_b = match (card_a, card_b) with
| ({value=va}, {value=vb}) when va > vb -> ~-1
| ({value=va}, {value=vb}) when va < vb -> 1
| ({color=ca}, {color=cb}) when ca > cb -> ~-1
| ({color=ca}, {color=cb}) when ca < cb -> 1
| _ -> 0

let max card_a card_b = match compare card_a card_b with
| comp when comp <= 0 -> card_a
| _ -> card_b

let min card_a card_b = match compare card_a card_b with
| comp when comp >= 0 -> card_a
| _ -> card_b

let rec best = function
  | [] -> invalid_arg "should call with a non empty list of cards"
  | [card] -> card
  | card :: rest -> max card (best rest)

let isOf {color=color_card} color = color_card = color

let isSpade card = isOf card Color.spade
let isHeart card = isOf card Color.heart
let isDiamond card = isOf card Color.Diamond
let isClub card = isOf card Color.club
