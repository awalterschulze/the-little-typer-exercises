#lang pie

;; Define a function called drop-last-k that takes an argument of type (Vec E ?) and
;; evaluates to a value of type (Vec E ?), the result of dropping the last k elements
;; from the first argument.
;;
;; NB: The type of the function should guarantee that we can't drop more elements
;; than there are in the first argument.

(claim + (-> Nat Nat Nat))
(define +
  (lambda (i j)
    (rec-Nat i
      j
      (lambda (i-1 sum-i-1)
        (add1 sum-i-1)
      ))))

{claim mot-drop-last-k 
    (Pi [(E U) (k Nat) (n Nat)] U)
}

{define mot-drop-last-k
    {lambda [E k n]
        (-> (Vec E (+ n k)) (Vec E n))
    }
}

{claim base-drop-last-k
    (Pi [(E U) (k Nat)]
        (mot-drop-last-k E k 0)
    )
}

{define base-drop-last-k
    {lambda [E k]
        {lambda [es]
            vecnil
        }
    }
}

{claim step-drop-last-k
    (Pi [(E U) (k Nat) (n-1 Nat)]
        (-> (mot-drop-last-k E k n-1)
            (mot-drop-last-k E k (add1 n-1))
        )
    )
}

{define step-drop-last-k
    {lambda [E k n-1]
        {lambda [drop-last-k-1]
            {lambda [es]
                (vec::
                    (head es)
                    (drop-last-k-1 (tail es))
                )
            }
        }
    }
}

{claim drop-last-k
    (Pi [(E U) (k Nat) (n Nat)]
        (mot-drop-last-k E k n)
    )
}

{define drop-last-k
    {lambda [E k n]
        (ind-Nat n
            (mot-drop-last-k E k)
            (base-drop-last-k E k)
            (step-drop-last-k E k)
        )
    }
}

(check-same (Vec Atom 3)
    (vec:: 'a (vec:: 'b (vec:: 'c vecnil)))
    (drop-last-k Atom 2 3
        (vec:: 'a (vec:: 'b (vec:: 'c (vec:: 'd (vec:: 'e vecnil))))))
)

(check-same (Vec Atom 0)
    vecnil
    (drop-last-k Atom 5 0
        (vec:: 'a (vec:: 'b (vec:: 'c (vec:: 'd (vec:: 'e vecnil))))))
)

(check-same (Vec Atom 5)
    (vec:: 'a (vec:: 'b (vec:: 'c (vec:: 'd (vec:: 'e vecnil)))))
    (drop-last-k Atom 0 5
        (vec:: 'a (vec:: 'b (vec:: 'c (vec:: 'd (vec:: 'e vecnil))))))
)