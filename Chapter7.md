# Chapter 7 Exercises

## Exercise 0

Define a function called zip that takes an argument of type `(Vec A n)` and a
second argument of type `(Vec B n)` and evaluates to a vlue of type `(Vec (Pair A B) n)`,
the result of zipping the first and second arguments.

[Answer](./chapter7-0-zip-vec.rkt)

## Exercise 1

Define a function called `append` that takes an argument of type `(Vec E m)` and an
argument of type `(Vect E n)` and evaluates to a value of type `(Vec (+ m n))`, the
result of appending the elements of the second argument to the end of the first.

[Answer](./chapter7-1-append-vec.rkt)

## Exercise 2

Define a function called `drop-last-k` that takes an argument of type `(Vec E ?)` and
evaluates to a value of type `(Vec E ?)`, the result of dropping the last `k` elements
from the first argument.
NB: The type of the function should guarantee that we can't drop more elements
than there are in the first argument.

[Answer](./chapter7-2-drop-last-k-vec.rkt)