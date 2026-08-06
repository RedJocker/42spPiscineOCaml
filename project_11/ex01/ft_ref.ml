(*




 Atribute to something you should normally never use.
Turn-in directory : ex01/
Files to turn in : ft_ref.ml
Forbidden functions : ref
Create a type ft_ref to reproduce the ref type, and implement the following func-
tions:
• return: ’a -> ’a ft_ref: creates a new reference.
• get: ’a ft_ref -> ’a: Dereferences a reference.
• set: ’a ft_ref -> ’a -> unit: Assigns a reference’s value.
• bind: ’a ft_ref -> (’a -> ’b ft_ref) -> ’b ft_ref: This one is a bit more
complicated. It applies a function to a reference to transform it. You can think of
it as a more complicated set function.
The use of the standard type ref is obviously forbidden, but playing with it in the
interpreter should tell you how it is implemented internally. Your goal is to do the same
thing. Oh, and by the way, after this exercise, you will have created your first monad.
Monads are a kind of ancient black magic you’ll get to play with soon enough. See you
  on d09...


  *)


type 'a ft_ref = {mutable contents: 'a}

let return value = {contents=value}
let get {contents} = contents
let set ft_ref value = ft_ref.contents <- value
let bind {contents} (f : 'a -> 'b ft_ref) = f contents

