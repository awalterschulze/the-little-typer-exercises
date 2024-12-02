# Chapter 11 Exercises

## Exercise 1

Use `ind-Vec` to define a function called `unzip` that takes unzips
a vector of pairs into a pair of vectors.

```
(claim unzip
      (Π ([A U]
          [B U]
          [n Nat])
         (-> (Vec (Pair A B) n)
             (Pair (Vec A n) (Vec B n)))))
```

[Answer](./chapter11-1-unzip-indvec.rkt)