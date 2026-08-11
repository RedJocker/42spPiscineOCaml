(*

  Exercise 03
Exercise 03: Fixed point
Turn-in directory : ex03/
Files to turn in : ex03.ml
Allowed functions : The Stdlib module
As OCaml lacks fixed point numbers, you’re going to add them yourself today. I’d
recommend this article from Berkeley as a start. If it’s good for them, it’s good for you.
If you have no idea what Berkeley is, read this section of their wikipedia page.
Write in the file "ex03.ml" a functor Make implementing the functor signature MAKE,
that takes as input modules implementing the signature FRACTIONNAL_BITS and outputs
modules that implement the signature FIXED. The signature FIXED is defined as follows:


module type FIXED = sig
type t
val of_float : float -> t
val of_int : int -> t
val to_float : t -> float
val to_int : t -> int
val to_string : t -> string
val zero : t
val one : t
val succ : t -> t
val pred : t -> t
val min : t -> t -> t
val max : t -> t -> t
val gth : t -> t -> bool
val lth : t -> t -> bool
val gte : t -> t -> bool
val lte : t -> t -> bool
val eqp : t -> t -> bool (** physical equality *)
val eqs : t -> t -> bool (** structural equality *)
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val foreach : t -> t -> (t -> unit) -> unit
end

Add the following code to your file "ex03.ml":

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)
let () =
let x8 = Fixed8.of_float 21.10 in
let y8 = Fixed8.of_float 21.32 in
let r8 = Fixed8.add x8 y8 in
print_endline (Fixed8.to_string r8);
Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))

The output must be:

$> ocamlopt ex03.ml && ./a.out
42.421875
0.
0.0625
0.125
0.1875
0.25
0.3125
0.375
0.4375
0.5
0.5625
0.625
0.6875
0.75
0.8125
0.875
0.9375
1.
$>
9
Ocaml Functors - 1
You MUST also provide some additionnal test code to proove that EVERY
requested functions in the signature FIXED work as intended. This
will be checked during peer-evaluation




  *)


module type FIXED = sig
  type t
  val of_float : float -> t
  val of_int : int -> t
  val to_float : t -> float
  val to_int : t -> int
  val to_string : t -> string
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val gth : t -> t -> bool
  val lth : t -> t -> bool
  val gte : t -> t -> bool
  val lte : t -> t -> bool
  val eqp : t -> t -> bool (** physical equality *)
  val eqs : t -> t -> bool (** structural equality *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTIONNAL_BITS = sig
  val bits : int
end

module Make = functor (M : FRACTIONNAL_BITS) -> struct
  let () = if M.bits < 0 || M.bits > 30 then
    failwith "Invalid bit precision"
  type t = Int32.t

  
  let of_float f =
    let int_part = int_of_float f in
    let float_part = f -. (float_of_int int_part) in
    let fixed_point = Int32.of_float (float_part *. (float_of_int (Int.shift_left 1 M.bits))) in
    Int32.add (Int32.shift_left (Int32.of_int int_part) M.bits) fixed_point
  let of_int i = Int32.shift_left (Int32.of_int i) M.bits
  let to_int n = Int32.shift_right n M.bits |> Int32.to_int

  let to_float n =
    let int_part = to_int n in
    let float_part =
       let rec loop i cur acc =
	 if i >= M.bits then
	   acc
	 else
	   begin
	     let bit = Int64.to_float(Int64.logand cur Int64.one) in
	     let acc_update = (acc +. bit) /. 2.0 in
	     let next = Int64.shift_right cur 1 in
	     loop (i + 1) next acc_update
	   end
       in
       loop 0 (Int64.of_int32 n) 0.0
    in
    float_part +. (float_of_int int_part)
  
  let to_string n =
    let int_part = to_int n in
    let float_part =
       let rec loop i cur acc =
	 if i >= M.bits then
	   acc 
	 else
	   begin
	     let bit = Int64.to_float(Int64.logand cur Int64.one) in
	     let acc_update = (acc +. bit) /. 2.0 in
	     let next = Int64.shift_right cur 1 in
	     loop (i + 1) next acc_update
	   end
       in
       loop 0 (Int64.of_int32 n) 0.0
    in
    let float_part_str = if float_part = 0.0 then "" else
      Printf.sprintf "%.*f" M.bits float_part
      |> String.drop_first 2
      |> String.drop_last_while ((=) '0')
    in
    
    Printf.sprintf "%d.%s" int_part float_part_str

  let zero = Int32.zero
  let one = Int32.( shift_left one M.bits )  
  let succ n = Int32.add n Int32.one
  let pred n = Int32.sub n Int32.one
  let min n0 n1 = Int32.min n0 n1
  let max n0 n1 = Int32.max n0 n1
  let gth n0 n1 = n0 > n1
  let lth n0 n1 = n0 < n1
  let gte n0 n1 = n0 >= n1
  let lte n0 n1 = n0 <= n1
  let eqp n0 n1 = n0 == n1
  let eqs n0 n1 = n0 = n1
  let add n0 n1 = Int32.add n0 n1
  let sub n0 n1 = Int32.sub n0 n1
  let mul n0 n1 =
    let n0 = Int64.of_int32 n0 in
    let n1 = Int64.of_int32 n1 in
    Int64.mul n0 n1
    |> (fun n -> Int64.shift_right n M.bits)
    |> Int64.to_int32
  let div n0 n1 =
    let n0 = Int64.of_int32 n0 in
    let n1 = Int64.of_int32 n1 in
    let numerator = Int64.shift_left n0 M.bits in
    Int64.div numerator n1 |> Int64.to_int32
  let rec foreach n0 n1 f =
    if gth n0 n1 then
      ()
    else
      begin
	f n0;
	foreach (succ n0) n1 f
      end
end

module Fixed4 : FIXED = Make (struct let bits = 30 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  Fixed4.foreach
    (Fixed4.zero)
    (Fixed4.one)
    (fun f -> print_endline (Fixed4.to_string f))



(*

  https://web.archive.org/web/20180613014335/http://inst.eecs.berkeley.edu/~cs61c/sp06/handout/fixedpt.html

  
  Introduction to Fixed Point Number Representation
CS61c Spring 2006
Author: 	Hayden So
Last Modified:	2/28/06
Introduction: Real numbers in Real world

  In real life, we deal with real numbers -- numbers with fractional part.
  Most modern computer have native (hardware) support for floating point numbers.
  However, the use of floating point is not necessarily the only way to represent fractional numbers.
  This article describes the fixed point representation of real numbers.
  The use of fixed point data type is used widely in digital signal processing (DSP) and game applications,
  where performance is sometimes more important than precision.
  As we will see later, fixed point arithmetic is much faster than floating point arithmetic.

  It All Starts With an Integer

Recall that a binary number:

    1101012

represents the value:

    1 * 25 + 1 * 24 + 0 * 23 + 1 * 22 + 0* 21 + 1 * 20

    = 32 + 16 + 4 + 1

    = 5310

  Now, if we divide the number 53 by 2,
  we know the the result should be 26.5.

  However, how do we represent it if we only had integer representations?

  The Binary Point


  The key to represent fractional numbers, like 26.5 above,
  is the concept of binary point.
  A binary point is like the decimal point in a decimal system.
  It acts as a divider between the integer and the fractional part of a number.


  In a decimal system, a decimal point denotes
  the position in a numeral that the coefficient should multiply by 100 = 1.
  For example, in the numeral 26.5, the coefficient 6 has a weight of 100 = 1.
  But what happen to the 5 to the right of decimal point?
  We know from our experience, that it carries a weight of 10-1.
  We know the numeral "26.5" represents the value "twenty six and a half" because

    2 * 10^1 + 6 * 10^0 + 5 * 10^-1 = 26.5

  The very same concept of decimal point can be applied to our binary representation,
  making a "binary point".
  As in the decimal system, a binary point represents
  the coefficient of the term 2^0 = 1.
  All digits (or bits) to the left of the binary point
  carries a weight of 2^0, 2^1, 2^2, and so on.
  Digits (or bits) on the right of binary point carries a weight of 2^-1, 2^-2, 2^-3, and so on.
  For example, the number:

    11010.1

represents the value:

        2^5 	2^4 	2^3 	2^2 	2^1 	2^0 	2^-1 	2^-2 	2-3
        ... 	1 	1 	0 	1 	0 	1 	0 	...

    = 1 * 2^4 + 1 * 2^3 + 0 * 2^2 + 1 * 21^ + 0 * 2^0 + 1 * 2^-1

    = 16 + 8 + 2 + 0.5

    = 26.5

Shifting Is The Key

  A careful reader should now realize
  the bit pattern of 53 and 26.5 is exactly the same.
  The only difference, is the position of binary point.

  In the case of 5310, there is "no" binary point.
  Alternatively, we can say the binary point is located at the far right, at position 0.
  (Think in decimal, 53 and 53.0 represents the same number.)

    2^5 2^4 	2^3 	2^2 	2^1 	2^0 	Binary Point 	2^-1 	2^-2 	2^-3
    1 	1 	0 	1 	0 	1 	. 	        0 	0 	0

In the case of 26.510, binary point is located one position to the left of 5310:

    2^5 2^4 	2^3 	2^2 	2^1 	2^0 	Binary Point 	2^-1 	2^-2 	2^-3
    0 	1 	1 	0 	1 	0 	. 	        1 	0 	0

  Now, recall in class, we discuss shifting an integer to the right by 1 bit position
  is equivalent to dividing the number by 2.
  In the case of integer, since we don't have a fractional part,
  we simply cannot represent digit to the right of a binary point,
  making this shifting process an integer division.
  However, it is simply a limitation of integer representations of binary number.


  In general, mathematically, given a fixed binary point position,
  shifting the bit pattern of a number to the right by 1 bit
  always divide the number by 2.
  Similarly, shifting a number to the left by 1 bit multiplies the number by 2.
  
Fixed Point Number Representation


  The shifting process above is the key to understand fixed point
  number representation.
  To represent a real number in computers (or any hardware in general),
  we can define a fixed point number type simply by implicitly
  fixing the binary point to be at some position of a numeral.
  We will then simply adhere to this implicit convention when we represent numbers.

To define a fixed point type conceptually, all we need are two parameters:

        width of the number representation, and
        binary point position within the number

We will use the notation fixed<w,b> for the rest of this article, where w denotes the number of bits used as a whole (the Width of a number), and b denotes the position of binary point counting from the least significant bit (counting from 0).

For example, fixed<8,3> denotes a 8-bit fixed point number, of which 3 right most bits are fractional. Therefore, the bit pattern:

    0 	0 	0 	1 	0 	1 	1 	0

represents a real number:

        00010.1102

    = 1 * 21 + 1 * 2-1 + 1 * 2-1

    = 2 + 0.5 + 0.25

    = 2.75

Note that on a computer, a bit patter can represents anything. Therefore the same bit pattern, if we "cast" it to another type, such as a fixed<8,5> type, will represents the number:

        000.101102

    = 1 * 2-1 + 1 * 2-3 + 1 * 2-4

    = 0.5 + 0.125 + 0.0625

    = 0.6875

If we treat this bit patter as integer, it represents the number:

        101102

    = 1 * 24 + 1 * 22 + 1 * 21

    = 16 + 4 + 2

    = 22

Negative Numbers

So far we talked about positive numbers, but we do want to represent negative numbers, don't we? How do we represent fixed point negative numbers then?

In computer, we use 2's complement to represent negative numbers. One of the property of 2's complement numbers is that arithmetic operations of either positive of negative numbers are identical. It includes, addition, subtraction, and not surprisingly, shifting. We can divide negative 2's complement numbers by 2 via a simple 1 bit right shift with sign extension as we can do so with positive numbers.

Recall in the beginning of this article we discuss how fixed point numbers are simply a shifted version of an integer (by setting binary point to a non-zero position). Combining with the observation that shift operation applies to 2's complement negative number as well as positive numbers, we can easily see how we can represent negative number in fixed point: Use 2's complement.

As an illustration, below are all the numbers representable with 4-bits 2's complement:

    Bit Pattern 	Number Represented (n) 	n / 2
    1 	1 	1 	1 	-1 	-0.5
    1 	1 	1 	0 	-2 	-1
    1 	1 	0 	1 	-3 	-1.5
    1 	1 	0 	0 	-4 	-2
    1 	0 	1 	1 	-5 	-2.5
    1 	0 	1 	0 	-6 	-3
    1 	0 	0 	1 	-7 	-3.5
    1 	0 	0 	0 	-8 	-4
    0 	1 	1 	1 	7 	3.5
    0 	1 	1 	0 	6 	3
    0 	1 	0 	1 	5 	2.5
    0 	1 	0 	0 	4 	2
    0 	0 	1 	1 	3 	1.5
    0 	0 	1 	0 	2 	1
    0 	0 	0 	1 	1 	0.5
    0 	0 	0 	0 	0 	0

Looking at this table, we can then easily realize we can represent the number -2.5 with bit pattern "1011", IF we assume the binary point is at position 1.
Pros and Cons of Fixed Point Number Representation

By now, you should find that fixed point numbers are indeed a close relative to integer representation. The two only differs in the position of binary point. In fact, you might even consider integer representation as a "special case" of fixed point numbers, where the binary point is at position 0. All the arithmetic operations a computer can operate on integer can therefore be applied to fixed point number as well.

Therefore, the benefit of fixed point arithmetic is that they are as straight-forward and efficient as integers arithmetic in computers. We can reuse all the hardware built to for integer arithmetic to perform real numbers arithmetic using fixed point representation. In other word, fixed point arithmetic comes for free on computers.

The disadvantage of fixed point number, is than of course the loss of range and precision when compare with floating point number representations. For example, in a fixed<8,1> representation, our fractional part is only precise to a quantum of 0.5. We cannot represent number like 0.75. We can represent 0.75 with fixed<8,2>, but then we loose range on the integer part.
Using Fixed Point Number in C

C does not have native "type" for fixed point number. However, due to the nature of fixed point representation, we simply don't need one. Recall all arithmetics on fixed point numbers are the same as integer, we can simply reuse the integer type int in C to perform fixed point arithmetic. The position of binary point only matters in cases when we print it on screen or perform arithmetic with different "type" (such as when adding int to fixed<32,6>).
Conclusion

Fixed point is a simple yet very powerful way to represent fractional numbers in computer. By reusing all integer arithmetic circuits of a computer, fixed point arithmetic is orders of magnitude faster than floating point arithmetic. This is the reason why it is being used in many game and DSP applications. On the other hand, it lacks the range and precision that floating point number representation offers. You, as a programmer or circuit designer, must do the tradeoff.






  *)
