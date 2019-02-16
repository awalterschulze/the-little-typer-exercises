# Exercises for the book - [The Little Typer](http://thelittletyper.com/)

Exercises were lovingly curated by Paul Cadman
https://github.com/paulcadman/the-little-typer

## Getting Started

  1. Install [Racket](https://racket-lang.org/)
  2. Open DrRacket
  3. File -> Install Package
  4. In the package name field, type pie and then click the `Install` button.
  5. To use Pie, begin a file with `#lang pie` in DrRacket.
  6. [Testing Pie Programs](https://docs.racket-lang.org/pie/index.html#%28form._%28%28lib._pie%2Fmain..rkt%29._check-same%29%29)

  - Haskell exercises are done using recursion schemes: https://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/

## Other Observations

  - `rec-Nat` and `rec-List` are paramorphisms, see [Nat.hs](https://github.com/awalterschulze/the-little-typer-exercises/blob/master/pie-haskell/src/Pie/Nat.hs) and [List.hs](https://github.com/awalterschulze/the-little-typer-exercises/blob/master/pie-haskell/src/Pie/List.hs).  Exercises from Chapter 3, 4 and 5 were rewritten using these implementations.
  
    For more information on recursion schemes, see [An introduction to recursion schemes - Patrick Thomson](https://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/)

  - `same` is a Functor and `cong` is fmap, see [Equal.hs](https://github.com/awalterschulze/the-little-typer-exercises/blob/master/pie-haskell/src/Pie/Equal.hs)
