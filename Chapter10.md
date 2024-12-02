# Chapter 10 Exercises

## Exercise 1

Define a function called `list-length-append-dist` that states and proves that
if you append two lists, `l1` and `l2`, and then the length of the result is
equal to the sum of the lengths of `l1` and `l2`.

[Answer](./chapter10-1-list-length-append.rkt)

## Exercise 2

In the following exercises we'll use the function called `<=` that takes two
Nat arguments `a`, `b` and evaluates to a type representing the proposition
that `a` is less than or equal to `b`.

```
(claim <=
       (-> Nat Nat
           U))

(define <=
  (λ (a b)
    (Σ ([k Nat])
       (= Nat (+ k a) b))))
```

### Exercise 2.1

Using `<=`, state and prove that 1 is less than or equal to 2.

[Answer](./chapter10-2-1-onelessthantwo.rkt)

### Exercise 2.2

Define a funciton called `<=-simplify` to state and prove that for all
Nats `a`, `b`, `n` we have that `n+a <= b` implies `a <= b`

NB: You may need to use `plus-assoc` that was proved in Exercise 8.3.

[Answer](./chapter10-2-2-alessthanb.rkt)

### Exercise 2.3

Define a function called `<=-trans` that states and proves that `<=` is
transitive.

```
(claim <=-trans
       (Π ([a Nat]
           [b Nat]
           [c Nat])
          (-> (<= a b)
              (<= b c)
              (<= a c))))
```

[Answer](./chapter10-2-3-smallertrans.rkt)

## Exercise 3

Define a function called `length-filter-list` that states and proves that
filtering a list (in the sense of `filter-list` from Exercise 5.3) evaluates
to a a list no longer than the original list.

```
(claim length-filter-list
  (Pi ((E U)
       (l (List E))
       (p (-> E Nat)))
       (<= (length E (filter-list E p l))
           (length E l))))
```

[Answer](./chapter10-3-filter-length.rkt)