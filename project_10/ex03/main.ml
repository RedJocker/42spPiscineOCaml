let _ =
  let deck_a = Deck.newDeck () in
  Deck.toStringListVerbose deck_a
  |> List.iter print_endline;

  let value = Deck.Card.Value.T2 in
  print_endline (Deck.Card.Value.toString value)
