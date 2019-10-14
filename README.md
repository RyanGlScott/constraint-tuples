# `constraint-tuples`
[![Hackage](https://img.shields.io/hackage/v/constraint-tuples.svg)][Hackage: constraint-tuples]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/constraint-tuples.svg)](http://packdeps.haskellers.com/reverse/constraint-tuples)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build](https://img.shields.io/travis/RyanGlScott/constraint-tuples.svg)](https://travis-ci.org/RyanGlScott/constraint-tuples)

[Hackage: constraint-tuples]:
  http://hackage.haskell.org/package/constraint-tuples
  "constraint-tuples package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

This library provides classes that emulate the behavior of GHC's constraint
tuple syntax. Unlike GHC's built-in constraint tuples, the classes in this
library can be partially applied.

This library exposes two different modules that are semantically identical, but
compile to slightly different Core:

* `Data.Tuple.Constraint`: A `CTupleN` class compiles to a dictionary data type
   with `N` fields.
* `Data.Tuple.Constraint.ClassNewtype`: A `CTupleN` class compiles to a newtype
   around the corresponding built-in constraint tuple type with `N` arguments.
