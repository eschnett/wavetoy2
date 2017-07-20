Haskell's number system
=======================

::

  Eq => Num (+, -, *)

  Num => Integral (div, mod)
  Num => Fractional (/)

  Fractional => Floating (sqrt)

  Ord, Num => Real
  Real, Fractional => Realfrac
  Real, Floating => Realfloat

  Integral: Int, Integer
  Realfrac: Rational, Ratio
  Realfloat: Float, Double
  Floating: Complex
