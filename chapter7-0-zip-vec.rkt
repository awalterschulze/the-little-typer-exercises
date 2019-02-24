#lang pie

;; Define a function called zip that takes an argument of type (Vec A n) and a
;; second argument of type (Vec B n) and evaluates to a vlue of type (Vec (Pair A B) n),
;; the result of zipping the first and second arguments.

{claim mot-zip
    (Pi [(A U) (B U) (n Nat)] U)}

{define mot-zip
    (lambda [A B n]
    (-> (Vec A n) (Vec B n) (Vec (Pair A B) n)))
}

{claim base-zip
    (Pi [(A U) (B U)]
    (mot-zip A B 0))
}

{define base-zip
    (lambda [A B]
        (lambda [as bs]
            (the (Vec (Pair A B) 0) vecnil)))}

{claim step-zip
    (Pi [(A U) (B U) (n-1 Nat)]
        (-> 
            (mot-zip A B n-1)
            (mot-zip A B (add1 n-1))
        ))}

{define step-zip
    (lambda [A B n-1]
        (lambda (zip-n-1)
            (lambda (as bs)
                (vec:: 
                    (cons (head as) (head bs))
                    (zip-n-1 (tail as) (tail bs))))))}

{claim zip
    (Pi [(A U) (B U) (n Nat)]
        (mot-zip A B n))
}

{define zip
    (lambda [A B n]
        (ind-Nat n
            (mot-zip A B)
            (base-zip A B)
            (step-zip A B)
        )
    )
}

(check-same (Vec (Pair Nat Atom) 2)
    (vec:: (cons 1 'a) (vec:: (cons 2 'b) vecnil))
    (zip Nat Atom 2
        (vec:: 1 (vec:: 2 vecnil))
        (vec:: 'a (vec:: 'b vecnil))
    )
)
