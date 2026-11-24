
(*

  The Earth began to cool, the autotrophs began to drool...
  Turn-in directory : ex02/
  Files to turn in : *.ml, Makefile
  Forbidden functions : None
  Molecules are cool, but today we’re focusing on a particular type of molecule, which is
  alkanes. Alkanes are a family of simple molecules composed of just carbon and hydrogen,
  which means we can create alkanes easily! The formula of an acyclic alkane is CnH2n+2.
  The name of the alkane simply depends on the value you give to n.
  You don’t have to handle every possible alkane (because actually, you
  can’t). Your data should be able to handle 1 ≤ n ≤ 12; you won’t be
  tested (and have the right to refuse to be tested) with n > 12.
  Note that an alkane is still a molecule, which means you still have to provide the
  following methods:
  • name
  • formula
  • to_string
  • equals
  To go along with your alkane class, you will write... some real alkanes! As usual, they
  will inherit your alkane class, and you will write at least the following ones:
  • methane
  • ethane
  • octane

 *)


class virtual alkane (name: string) (n: int) =
object(this)
  inherit Molecule.molecule name (
    let carbons = List.init n (fun a -> new Atom.carbon) in
    let hydrogens = List.init ((2 * n) + 2) (fun a -> new Atom.hydrogen) in
    List.append carbons hydrogens
  )
end              

class methane = object(this)
  inherit alkane "Methane" 1
end

class ethane = object(this)
  inherit alkane "Ethane" 2
end

class propane = object(this)
  inherit alkane "Propane" 3
end

class butane = object(this)
  inherit alkane "Butane" 4
end

class pentane = object(this)
  inherit alkane "Pentane" 5
end

class hexane = object(this)
  inherit alkane "Hexane" 6
end

class heptane = object(this)
  inherit alkane "Heptane" 7
end

class octane = object(this)
  inherit alkane "Octane" 8
end

class nonane = object(this)
  inherit alkane "Nonane" 9
end

class decane = object(this)
  inherit alkane "Decane" 10
end

class undecane = object(this)
  inherit alkane "Undecane" 11
end

class duodecane = object(this)
  inherit alkane "Duodecane" 12
end
