-- CharRegex was created as teaching material for readers of The Little Typer
-- This package includes notes on `#lang pie`, which references the pie language from The Little Typer.
-- We provide a demonstration of how to prove decidability of regular expressions.
-- This is based on https://github.com/katydid/regex-deriv-lean , but
-- we redefined Lang so that everything is contained in one file.
-- and the type parameters of the Regex are fixed to Char and no predicate.

import Mathlib.Tactic.CongrM -- (split_ands, congrm)

namespace CharRegex

-- Language is a set of strings
-- Set String = (-> String Bool)
--  => (-> String Prop)
-- String = List Char
--  => (-> (List Char) Prop)

-- Prop = U
-- Type = U
-- List Char = List Nat
-- #lang pie
-- (claim Lang U)
-- (define Lang (-> (List Nat) U)) ; cannot express in Pie
def Lang: Type := List Char -> Prop

-- False = Absurd
-- #lang pie
-- (claim emptyset (-> (List Nat) U))
-- (define emptyset (lambda (_) Absurd))
def Lang.emptyset : Lang :=
  fun _ => False

-- #lang pie
-- (claim emptystr (-> (List Nat) U))
-- (define emptystr (lambda (xs) (= (List Nat) xs nil)))
def Lang.emptystr : Lang :=
  fun xs => xs = []

-- (claim char (-> Nat (List Nat) U))
-- (define char (lambda (x xs) (= (List Nat) xs (:: x nil))))
def Lang.char (x : Char): Lang :=
  fun xs => xs = [x]

-- #lang pie
-- (claim char (-> (-> (List Nat) U) (-> (List Nat) U) (List Nat) U))
-- (define char (lambda (p q xs) (Either (p xs) (q xs))))
def Lang.or (P : Lang) (Q : Lang) : Lang :=
  fun xs => P xs \/ Q xs

-- #lang pie
-- (claim append
--   (Pi ((E U))
--     (-> (List E) (List E) (List E))))
-- (define append
--   (lambda (_)
--     (lambda (l1 l2)
--       (rec-List l1
--         l2
--         (lambda (e1 _ es1:l2)
--           (:: e1 es1:l2))))))
-- (claim concat (-> (-> (List Nat) U) (-> (List Nat) U) (List Nat) U))
-- (define concat (lambda (p q xs)
--    (Sigma ((xs1 (List Nat)) (xs2 (List Nat)))
--      (Pair
--       (= (List Nat) xs (append Nat xs1 xs2))
--       (Pair (p xs1) (q xs2))))))
def Lang.concat (P : Lang) (Q : Lang) : Lang :=
  fun (xs : List Char) =>
    ∃ (xs1 : List Char) (xs2 : List Char), P xs1 /\ Q xs2 /\ xs = (xs1 ++ xs2)

inductive Lang.star (R: Lang): Lang where
  | zero: star R []
  | more: ∀ (x: Char) (xs1 xs2 xs: List Char),
    xs = (x::xs1) ++ xs2
    -> R (x::xs1)
    -> star R xs2
    -> star R xs

inductive Regex: Type where
  | emptyset : Regex
  | emptystr : Regex
  | char : (c: Char) → Regex
  | or : Regex → Regex → Regex
  | concat : Regex → Regex → Regex
  | star : Regex → Regex

def denote (r: Regex): Lang :=
  match r with
  | Regex.emptyset => Lang.emptyset
  | Regex.emptystr => Lang.emptystr
  | Regex.char c => Lang.char c
  | Regex.or x y => Lang.or (denote x) (denote y)
  | Regex.concat x y => Lang.concat (denote x) (denote y)
  | Regex.star x => Lang.star (denote x)

def Lang.onlyif (cond : Prop) (R : Lang) : Lang :=
  fun xs => cond /\ R xs

-- def Not (a : Prop) : Prop := a → False
-- #lang pie
-- (claim Not (-> U U))
-- (define Not (Pi ((A U)) (-> A Absurd))) ; cannot express in Pie
def Lang.not (R: Lang): Lang :=
  fun xs => (Not (R xs))

def Lang.containsEmptyStr (R: Lang): Prop :=
  R []

def Lang.stripPrefix (R: Lang) (xs: List Char): Lang :=
  λ ys => R (xs ++ ys)

theorem Lang.validate' (R: Lang) (xs: List Char):
  containsEmptyStr (stripPrefix R xs) = R xs := by
  unfold stripPrefix
  unfold containsEmptyStr
  simp only [List.append_nil]

def Lang.stripChar (R: Lang) (x: Char): Lang :=
  Lang.stripPrefix R [x]

-- Lang.containsEmptyStr
def Lang.null (R: Lang): Prop :=
  R []

-- Lang.stripPrefix
def Lang.derives (R: Lang) (xs: List Char): Lang :=
  λ ys => R (xs ++ ys)

-- Lang.stripChar
def Lang.derive (R: Lang) (x: Char): Lang :=
  Lang.derives R [x]

theorem Lang.validate (R: Lang) (xs: List Char):
  null (derives R xs) = R xs := by
  unfold null
  unfold derives
  simp only [List.append_nil]

def Regex.null (r: Regex): Bool :=
  match r with
  | Regex.emptyset => false
  | Regex.emptystr => true
  | Regex.char _ => false
  | Regex.or x y => null x || null y
  | Regex.concat x y => null x && null y
  | Regex.star _ => true

def Regex.onlyif (cond: Prop) [dcond: Decidable cond] (r: Regex): Regex :=
  if cond then r else Regex.emptyset

def Regex.derive (r: Regex) (a: Char): Regex :=
  match r with
  | Regex.emptyset => Regex.emptyset
  | Regex.emptystr => Regex.emptyset
  | Regex.char c => Regex.onlyif (a = c) Regex.emptystr
  | Regex.or x y => Regex.or (derive x a) (derive y a)
  | Regex.concat x y =>
      Regex.or
        (Regex.concat (derive x a) y)
        (Regex.onlyif (null x) (derive y a))
  | Regex.star x =>
      Regex.concat (derive x a) (Regex.star x)

def Regex.derives (r: Regex) (xs: List Char): Regex :=
  (List.foldl Regex.derive r) xs

def Regex.validate (r: Regex) (xs: List Char): Bool :=
  null (derives r xs)

theorem Lang.null_emptyset:
  null emptyset = False :=
  rfl

theorem Lang.null_emptystr:
  null emptystr = True := by
  unfold emptystr
  unfold null
  simp only

theorem Lang.null_iff_char {c: Char}:
  null (char c) <-> False :=
  Iff.intro nofun nofun

theorem Lang.null_char {c: Char}:
  null (char c) = False := by
  rw [null_iff_char]

theorem Lang.null_or {P Q: Lang}:
  null (or P Q) = ((null P) \/ (null Q)) :=
  rfl

theorem Lang.null_iff_concat {P Q: Lang}:
  null (concat P Q) <-> ((null P) /\ (null Q)) := by
  refine Iff.intro ?toFun ?invFun
  case toFun =>
    intro ⟨x, y, hx, hy, hxy⟩
    simp only [List.nil_eq, List.append_eq_nil] at hxy
    simp only [hxy] at hx hy
    exact ⟨hx, hy⟩
  case invFun =>
    exact fun ⟨x, y⟩ => ⟨[], [], x, y, rfl⟩

theorem Lang.null_concat {P Q: Lang}:
  null (concat P Q) = ((null P) /\ (null Q)) := by
  rw [null_iff_concat]

theorem Lang.null_iff_star {R: Lang}:
  null (star R) <-> True :=
  Iff.intro
    (fun _ => True.intro)
    (fun _ => star.zero)

theorem Lang.null_star {R: Lang}:
  null (star R) = True := by
  rw [null_iff_star]

theorem null_commutes (r: Regex):
  ((Regex.null r) = true) = Lang.null (denote r) := by
  induction r with
  | emptyset =>
    unfold denote
    rw [Lang.null_emptyset]
    unfold Regex.null
    apply Bool.false_eq_true
  | emptystr =>
    unfold denote
    rw [Lang.null_emptystr]
    unfold Regex.null
    simp only
  | char c =>
    unfold denote
    rw [Lang.null_char]
    unfold Regex.null
    apply Bool.false_eq_true
  | or p q ihp ihq =>
    unfold denote
    rw [Lang.null_or]
    unfold Regex.null
    rw [<- ihp]
    rw [<- ihq]
    rw [Bool.or_eq_true]
  | concat p q ihp ihq =>
    unfold denote
    rw [Lang.null_concat]
    unfold Regex.null
    rw [<- ihp]
    rw [<- ihq]
    rw [Bool.and_eq_true]
  | star r ih =>
    unfold denote
    rw [Lang.null_star]
    unfold Regex.null
    simp only

theorem Lang.derive_emptyset {a: Char}:
  (derive emptyset a) = emptyset :=
  rfl

theorem Lang.derive_iff_emptystr {a: Char} {w: List Char}:
  (derive emptystr a) w <-> emptyset w :=
  Iff.intro nofun nofun

theorem Lang.derive_emptystr {a: Char}:
  (derive emptystr a) = emptyset := by
  funext
  rw [derive_iff_emptystr]

theorem Lang.derive_iff_char {a: Char} {c: Char} {w: List Char}:
  (derive (char c) a) w <-> (onlyif (a = c) emptystr) w := by
  refine Iff.intro ?toFun ?invFun
  case toFun =>
    intro D
    cases D with | refl =>
    exact ⟨ rfl, rfl ⟩
  case invFun =>
    intro ⟨ H1 , H2  ⟩
    cases H1 with | refl =>
    cases H2 with | refl =>
    exact rfl

theorem Lang.derive_char {a: Char} {c: Char}:
  (derive (char c) a) = (onlyif (a = c) emptystr) := by
  funext
  rw [derive_iff_char]

theorem Lang.derive_or {a: Char} {P Q: Lang}:
  (derive (or P Q) a) = (or (derive P a) (derive Q a)) :=
  rfl

theorem Lang.derive_iff_concat {x: Char} {P Q: Lang} {xs: List Char}:
  (derive (concat P Q) x) xs <->
    (or (concat (derive P x) Q) (onlyif (null P) (derive Q x))) xs := by
  refine Iff.intro ?toFun ?invFun
  case toFun =>
    simp only [or, concat, derive, derives, null, onlyif]
    intro d
    guard_hyp d: ∃ x_1 y, P x_1 ∧ Q y ∧ [x] ++ xs = x_1 ++ y
    guard_target = (∃ x_1 y, P ([x] ++ x_1) ∧ Q y ∧ xs = x_1 ++ y) ∨ P [] ∧ Q ([x] ++ xs)
    match d with
    | Exists.intro ps (Exists.intro qs (And.intro hp (And.intro hq hs))) =>
    guard_hyp hp : P ps
    guard_hyp hq : Q qs
    guard_hyp hs : [x] ++ xs = ps ++ qs
    match ps with
    | List.nil =>
      guard_hyp hp : P []
      guard_hyp hq : Q qs
      refine Or.inr ?r
      guard_target = P [] ∧ Q ([x] ++ xs)
      rw [List.nil_append] at hs
      rw [hs]
      exact And.intro hp hq
    | List.cons p ps =>
      guard_hyp hp : P (p :: ps)
      guard_hyp hq : Q qs
      guard_hyp hs : [x] ++ xs = p :: ps ++ qs
      refine Or.inl ?l
      guard_target = ∃ x_1 y, P ([x] ++ x_1) ∧ Q y ∧ xs = x_1 ++ y
      simp only [List.cons_append, List.cons.injEq] at hs
      match hs with
      | And.intro hpx hs =>
        rw [hpx]
        rw [List.nil_append] at hs
        rw [hs]
        guard_hyp hs : xs = ps ++ qs
        guard_target = ∃ x y, P ([p] ++ x) ∧ Q y ∧ ps ++ qs = x ++ y
        exact Exists.intro ps (Exists.intro qs (And.intro hp (And.intro hq rfl)))
  case invFun =>
    simp only [or, concat, derive, derives, null, onlyif]
    intro e
    guard_hyp e : (∃ x_1 y, P ([x] ++ x_1) ∧ Q y ∧ xs = x_1 ++ y) ∨ P [] ∧ Q ([x] ++ xs)
    guard_target = ∃ x_1 y, P x_1 ∧ Q y ∧ [x] ++ xs = x_1 ++ y
    match e with
    | Or.inl e =>
      guard_hyp e: ∃ x_1 y, P ([x] ++ x_1) ∧ Q y ∧ xs = x_1 ++ y
      guard_target = ∃ x_1 y, P x_1 ∧ Q y ∧ [x] ++ xs = x_1 ++ y
      match e with
      | Exists.intro ps (Exists.intro qs (And.intro hp (And.intro hq hs))) =>
        guard_hyp hp: P ([x] ++ ps)
        guard_hyp hq: Q qs
        guard_hyp hs: xs = ps ++ qs
        rw [hs]
        guard_target = ∃ x_1 y, P x_1 ∧ Q y ∧ [x] ++ (ps ++ qs) = x_1 ++ y
        exact Exists.intro ([x] ++ ps) (Exists.intro qs (And.intro hp (And.intro hq rfl)))
    | Or.inr e =>
      guard_hyp e : P [] ∧ Q ([x] ++ xs)
      guard_target = ∃ x_1 y, P x_1 ∧ Q y ∧ [x] ++ xs = x_1 ++ y
      match e with
      | And.intro hp hq =>
        exact Exists.intro [] (Exists.intro (x :: xs) (And.intro hp (And.intro hq rfl)))

theorem Lang.derive_concat {x: Char} {P Q: Lang}:
  (derive (concat P Q) x) =
    (or (concat (derive P x) Q) (onlyif (null P) (derive Q x))) := by
  funext
  rw [derive_iff_concat]

theorem Lang.derive_iff_star {x: Char} {R: Lang} {xs: List Char}:
  (derive (star R) x) xs <-> (concat (derive R x) (star R)) xs := by
  refine Iff.intro ?toFun ?invFun
  case toFun =>
    intro deriveStar
    unfold derive at deriveStar
    unfold derives at deriveStar
    cases deriveStar with
    | more x' xs1 xs2 _ hxs Rxxs1 starRxs2 =>
      unfold concat
      exists xs1
      exists xs2
      simp only [List.singleton_append, List.cons_append, List.cons.injEq] at hxs
      cases hxs with
      | intro hxs1 hxs2 =>
      rw [hxs1]
      split_ands
      · exact Rxxs1
      · exact starRxs2
      · exact hxs2
  case invFun =>
    intro concatDerive
    unfold concat at concatDerive
    cases concatDerive with
    | intro xs1 concatDerive =>
    cases concatDerive with
    | intro xs2 concatDerive =>
    cases concatDerive with
    | intro deriveRxxs1 concatDerive =>
    cases concatDerive with
    | intro starRxs2 hxs =>
    unfold derive
    unfold derives
    refine star.more x xs1 xs2 ?hxs ?e ?f ?g
    · rw [hxs]
      simp only [List.singleton_append, List.cons_append, List.nil_append]
    · apply deriveRxxs1
    · exact starRxs2

theorem Lang.derive_star {x: Char} {R: Lang}:
  (derive (star R) x) = (concat (derive R x) (star R)) := by
  funext
  rw [derive_iff_star]

def denote_onlyif (condition: Prop) [dcond: Decidable condition] (r: Regex):
  denote (Regex.onlyif condition r) = Lang.onlyif condition (denote r) := by
  unfold Lang.onlyif
  unfold Regex.onlyif
  funext xs
  split_ifs
  case pos hc =>
    simp only [eq_iff_iff, iff_and_self]
    intro d
    assumption
  case neg hc =>
    simp only [eq_iff_iff]
    rw [denote]
    rw [Lang.emptyset]
    simp only [false_iff, not_and]
    intro hc'
    contradiction

theorem derive_commutes (r: Regex) (x: Char):
  denote (Regex.derive r x) = Lang.derive (denote r) x := by
  induction r with
  | emptyset =>
    simp only [denote]
    rw [Lang.derive_emptyset]
  | emptystr =>
    simp only [denote]
    rw [Lang.derive_emptystr]
  | char c =>
    simp only [denote]
    rw [Lang.derive_char]
    unfold Regex.derive
    rw [denote_onlyif]
    simp only [denote]
  | or p q ihp ihq =>
    simp only [denote]
    rw [Lang.derive_or]
    unfold Lang.or
    rw [ihp]
    rw [ihq]
  | concat p q ihp ihq =>
    simp only [denote]
    rw [Lang.derive_concat]
    rw [<- ihp]
    rw [<- ihq]
    rw [denote_onlyif]
    congrm (Lang.or (Lang.concat (denote (Regex.derive p x)) (denote q)) ?_)
    rw [null_commutes]
  | star r ih =>
    simp only [denote]
    rw [Lang.derive_star]
    guard_target =
      Lang.concat (denote (Regex.derive r x)) (Lang.star (denote r))
      = Lang.concat (Lang.derive (denote r) x) (Lang.star (denote r))
    congrm ((Lang.concat ?_ (Lang.star (denote r))))
    guard_target = denote (Regex.derive r x) = Lang.derive (denote r) x
    exact ih

theorem Lang.derives_strings (R: Lang) (xs ys: List Char):
  derives R (xs ++ ys) = derives (derives R xs) ys :=
  match xs with
  | [] => rfl
  | (x :: xs) => derives_strings (derive R x) xs ys

theorem Lang.derives_step (R: Lang) (x: Char) (xs: List Char):
  derives R (x :: xs) = derives (derive R x) xs := by
  simp only [derive]
  rw [<- derives_strings]
  congr

theorem Lang.derives_foldl (R: Lang) (xs: List Char):
  (derives R) xs = (List.foldl derive R) xs := by
  revert R
  induction xs with
  | nil =>
    unfold derives
    simp only [List.nil_append, List.foldl_nil, implies_true]
  | cons x xs ih =>
    simp
    intro R
    rw [derives_step]
    rw [ih (derive R x)]

theorem derives_commutes (r: Regex) (xs: List Char):
  denote (Regex.derives r xs) = Lang.derives (denote r) xs := by
  unfold Regex.derives
  rw [Lang.derives_foldl]
  revert r
  induction xs with
  | nil =>
    simp only [List.foldl_nil]
    intro h
    exact True.intro
  | cons x xs ih =>
    simp only [List.foldl_cons]
    intro r
    have h := derive_commutes r x
    have ih' := ih (Regex.derive r x)
    rw [h] at ih'
    exact ih'

theorem validate_commutes (r: Regex) (xs: List Char):
  (Regex.validate r xs = true) = (denote r) xs := by
  rw [<- Lang.validate (denote r) xs]
  unfold Regex.validate
  rw [<- derives_commutes]
  rw [<- null_commutes]

theorem not_false_eq_true : Not (false = true) := by
  intro h
  nomatch h

theorem false_eq_true : (false = true) = False := by
  simp only [eq_iff_iff]
  apply Iff.intro
  · case mp =>
    intro h
    nomatch h
  · case mpr =>
    intro h
    contradiction

def decidableDenote (r: Regex): DecidablePred (denote r) := by
  unfold DecidablePred
  intro xs
  -- validate_commutes: (Regex.validate r xs = true) = (denote r) xs
  rw [<- validate_commutes]
  -- Goal: Decidable (r.validate xs = true)
  exact (
    match r.validate xs with
    | false => -- r.validate xs = false
      -- Goal: Decidable (false = true)
      -- Eq.refl has no constructor with two different values
      -- nomatch h says we have a hypothesis that was impossible construct, just like False
      let f: Not (false = true) := fun (h: false = true) => nomatch h
      -- Not (false = true)
      -- = Not False, see false_eq_true
      -- = False -> False, see Not
      -- = x -> x, generalized
      -- = id
      -- Decidable.isFalse (h : Not p) : Decidable p
      Decidable.isFalse f
    | true => -- r.validate xs = true
      -- Goal: Decidable (true = true)
      -- Decidable.isTrue (h : p) : Decidable p
      -- same true = (true = true)
      Decidable.isTrue (Eq.refl true)
  )

def decidableDenote'' (r: Regex): DecidablePred (denote r) := by
  unfold DecidablePred
  intro xs
  rw [<- validate_commutes]
  apply Bool.decEq

-- decidableDenote shows that the derivative algorithm is decidable
-- https://leanprover.zulipchat.com/#narrow/channel/270676-lean4/topic/restricting.20axioms
def decidableDenote' (r: Regex): DecidablePred (denote r) :=
  fun xs => decidable_of_decidable_of_eq (validate_commutes r xs)

inductive Bool : Type where
  /-- The boolean value `false`, not to be confused with the proposition `False`. -/
  | false : Bool
  /-- The boolean value `true`, not to be confused with the proposition `True`. -/
  | true : Bool

-- True = Trivial
-- True.intro = sole
inductive True : Prop where
  /-- `True` is true, and `True.intro` (or more commonly, `trivial`)
  is the proof. -/
  | intro : True

-- #lang pie
-- Absurd
inductive False : Prop

-- if a is False, then I can return False
-- if a is False, it is not True
-- if a is True, it cannot be Not a, since I can't return False
-- Not: (-> A Absurd)
def Not (a : Prop) : Prop := a → False

/-- A decidable predicate. See `Decidable`. -/
abbrev DecidablePred {α : Sort u} (r : α → Prop) :=
  (a : α) → Decidable (r a)

abbrev DecidableLang (r : List Char → Prop) :=
  (xs : List Char) → Decidable (r xs)

-- #lang pie
-- (claim Dec
--   (-> U U))
-- (define Dec
--   (lambda (X)
--     (Either
--       (-> X Absurd)
--       X
-- )))
class inductive Decidable (p : Prop) where
  /-- Prove that `p` is decidable by supplying a proof of `¬p` -/
  | isFalse (h : Not p) : Decidable p
  /-- Prove that `p` is decidable by supplying a proof of `p` -/
  | isTrue (h : p) : Decidable p

-- Eq.refl has no constructor with two different values
-- nomatch h says we have a hypothesis that was impossible construct, just like False

/-- Decidable equality for Bool -/
def Bool.decEq (a b : Bool) : Decidable (Eq a b) :=
  match a, b with
  | false, false => Decidable.isTrue (Eq.refl false)
  | false, true  => Decidable.isFalse (fun h: (false = true) => nomatch h)
  | true, false  => Decidable.isFalse (fun h: (true = false) => nomatch h)
  | true, true   => Decidable.isTrue (Eq.refl true)

-- if the Principle of the Excluded Middle is not false, why isn’t it true?

-- There are, however, some statements that are either true or false.
-- These statements are called decidable because there is a function that decides whether they are true orfalse.
