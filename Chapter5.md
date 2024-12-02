# Chapter 5 Exercises

## Exercise 1

Define a function called `sum-List` that takes one `List Nat` argument and
evaluates to a Nat, the sum of the Nats in the list.

[Answer](./chapter5-1-sum-list.rkt)

## Exercise 2

Define a function called maybe-last which takes (in addition to the type
argument for the list element) one (`List E`) argument and one `E` argument and
evaluates to an `E` with value of either the last element in the list, or the
value of the second argument if the list is empty.

[Answer](./chapter5-2-maybe-last.rkt)

## Exercise 3

Define a function called filter-list which takes (in addition to the type
argument for the list element) one `(-> E Nat)` argument representing a
predicate and one `(List E)` argument.

The function evaluates to a `(List E)` consisting of elements from the list
argument where the predicate is true.

Consider the predicate to be false for an element if it evaluates to zero,
and true otherwise.

[Answer](./chapter5-3-filter-list.rkt)

## Exercise 4

Define a function called `sort-List-Nat` which takes one `(List Nat)` argument
and evaluates to a `(List Nat)` consisting of the elements from the list
argument sorted in ascending order.

[Answer](./chapter5-4-sort-list.rkt)