(*

exercise 03: Deck
Turn-in directory : ex03/
Files to turn in : Deck.mli, Deck.ml and main.ml
Allowed functions : Allowed functions and modules from the previous
exercices, plus raise and the Random module
We have cards, and it’s time to organize them into a deck represented by the Deck
module. First, write the interface for that module in the file Deck.mli according to the
following statements:
• The Deck module embeds the Card module from the previous exercice.
• The Deck module exposes an abstract type t that represents a deck. Its definition
is up to you.
• The Deck module exposes a function newDeck that takes no argument and returns
a deck of the 52 cards (i.e. the type t) in random order. This means that upon
two different calls to the function newDeck, the order of the deck will be different.
• The Deck module exposes a function toStringList that takes a deck as a parameter
and returns a list of the string representations of each card.
• The Deck module exposes a function toStringListVerbose that takes a deck as a
parameter and returns a list of the verbose string representations of each card.
• The Deck module exposes a function drawCard that takes a deck as a parameter
and returns a couple composed of the first card of the deck and the rest of the deck.
If the deck is empty, raise the exception Failure with a relevant error message.
Now implement the Deck module in the file Deck.ml according to its interface.
9ComentárioDestaque
Ocaml OCaml’s modules language - 1
Provide some tests in the file main.ml to demonstrate that your Deck, Deck.Card,
Deck.Card.Color, and Deck.Card.Value modules work as intended.
This exercise is not mandatory.


 *)

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


type t
module Card : Card

val newDeck : unit -> t
val toStringList : t -> string list
val toStringListVerbose : t -> string list
val drawCard : t -> Card.t * t
