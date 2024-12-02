# Chapter 8 Exercises

## Exercise 1

Define a function called `zero+n=n` that states and proves that
`0+n = n` for all Nat `n`.

[Answer](./chapter8-1-zero-plus-n.rkt)

## Exercise 2

Define a function called `a=b->a+n=b+n` that states and proves that
`a = b` implies `a+n = b+n` for all Nats `a`, `b`, `n`.

[Answer](./chapter8-2-plus-n.rkt)

## Exercise 3

Define a function called `plus-assoc` that states and proves that
`+` is associative.

```
(claim plus-assoc
 (Pi ((n Nat) (m Nat) (k Nat))
   (= Nat (+ k (+ n m)) (+ (+ k n) m))))
```

[Answer](./chapter8-3-plus-assoc.rkt)