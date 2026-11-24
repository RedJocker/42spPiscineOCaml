let _ =
  
  let methane = new Alkane.methane in

  let nop_reaction = object(this)
     inherit Reaction.reaction [(methane, 1)] [(methane, 1)]
     method is_balanced = true
     method balance = this
  end in

  if nop_reaction#is_balanced then
    print_endline "balanced"
  else
    ();
