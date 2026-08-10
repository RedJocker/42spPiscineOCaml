module type Color = sig
  type t = Spade | Heart | Diamond | Club
  val all : t list
  val toString : t -> string
  val toStringVerbose : t -> string
end

module type Value = sig
  type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

  (** The list of all values of type t *)
  val all : t list

  (** Interger representation of a card value, from 1 for T2 to 13 for As *)
  val toInt : t -> int

  (** returns "2", ..., "10", "J", "Q", "K" or "A" *)
  val toString : t -> string

  (** returns "2", ..., "10", "Jack", "Queen", "King" or "As" *)
  val toStringVerbose : t -> string

  (** Returns the next value, or calls invalid_arg if argument is As *)
  val next : t -> t

  (** Returns the previous value, or calls invalid_arg if argument is T2 *)
  val previous : t -> t

end

module type Card = sig


  module Value : Value

  module Color : Color

  type t = {value: Value.t; color: Color.t}
  val newCard : Value.t -> Color.t -> t

  val allSpades : t list

  val allDiamonds : t list

  val allClubs : t list

  val allHearts : t list

  val all : t list

  val getValue : t -> Value.t
  val getColor : t -> Color.t

  val toString : t -> string

  val toStringVerbose : t -> string

  val compare : t -> t -> int

  val max : t -> t -> t

  val min : t -> t -> t

  val best : t list -> t

  val isOf : t-> Color.t -> bool

  val isSpade : t -> bool
  val isHeart : t -> bool
  val isDiamond : t -> bool
  val isClub : t -> bool

end

module Color : Color = struct
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

module Value : Value = struct
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

module Card : Card = struct
  module Value = Value
  module Color = Color
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

  let isSpade card = isOf card Color.Spade
  let isHeart card = isOf card Color.Heart
  let isDiamond card = isOf card Color.Diamond
  let isClub card = isOf card Color.Club

end

type t = Card.t list

let shuffle lst =
  List.sort (fun _ _ -> Random.int_in_range ~min:~-1 ~max:1) lst

let newDeck () =
  Card.all
  |> shuffle

let toStringList deck =
  List.map Card.toString deck

let toStringListVerbose deck =
  List.map Card.toStringVerbose deck

let drawCard = function
  | [] -> failwith ""
  | head::tail -> (head, tail)
