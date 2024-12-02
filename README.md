# Exercises for the book - [The Little Typer](http://thelittletyper.com/)

Exercises were lovingly curated by [Paul Cadman](https://github.com/paulcadman/the-little-typer)

## Getting Started

  1. Install [Racket](https://racket-lang.org/)
  2. Open DrRacket
  3. File -> Install Package
  4. In the package name field, type pie and then click the `Install` button.
  5. To use Pie, begin a file with `#lang pie` in DrRacket.
  6. [Testing Pie Programs](https://docs.racket-lang.org/pie/index.html#%28form._%28%28lib._pie%2Fmain..rkt%29._check-same%29%29)

## Exercises

* [Chapter 3](./Chapter3.md)
* [Chapter 4](./Chapter4.md)
* [Chapter 5](./Chapter5.md)
* [Chapter 7](./Chapter7.md)
* [Chapter 8](./Chapter8.md)
* [Chapter 9](./Chapter9.md)
* [Chapter 10](./Chapter10.md)
* [Chapter 11](./Chapter11.md)
* [Chapter 12](./Chapter12.md)
* [Chapter 13](./Chapter13.md)

## Help

  - `replace` in Chapter 9 was quite hard for me to grasp, but I wasn't the only one.  [Andrew Helwer](https://github.com/ahelwer) has written up a blog post as a [preamble to Chapter 9](https://ahelwer.ca/post/2022-10-13-little-typer-ch9/), that I would recommend reading before reading Chapter 9. I also finally came up with a way to understand and use it and it seems like everyone in the study group came up with their own unique way.  See [discussion in the Google Group](https://groups.google.com/forum/#!msg/the-little-typer-study-group-london/759LwPE6E_g/s_nRUSbuBAAJ) or [a print out of the discussion](https://github.com/awalterschulze/the-little-typer-exercises/issues/1) if you don't have permission.

  - Exercise 10.2 and 10.3 might be easier after reading Chapter 12.

## Other Observations

  - `rec-Nat` and `rec-List` are paramorphisms, see [Nat.hs](https://github.com/awalterschulze/the-little-typer-exercises/blob/master/pie-haskell/src/Pie/Nat.hs) and [List.hs](https://github.com/awalterschulze/the-little-typer-exercises/blob/master/pie-haskell/src/Pie/List.hs).  Exercises from Chapter 3, 4 and 5 were rewritten using these implementations.
  
    For more information on recursion schemes, see [An introduction to recursion schemes - Patrick Thomson](https://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/)

  - `same` is a Functor and `cong` is fmap, see [Equal.hs](https://github.com/awalterschulze/the-little-typer-exercises/blob/master/pie-haskell/src/Pie/Equal.hs)
