open Nat
open List

namespace Chapter9

-- The Law of replace for Pie
-- If target is an
--  (= X from to),
-- mot is an
--  (-> X U),
-- and base is a
--  (mot from)
-- then
--  (replace target mot base)
-- is a
--  (mot to).
theorem replace {X: Type} {FROM: X} {TO: X} (target: (FROM = TO)) (MOT: X -> Prop) (base: MOT FROM): MOT TO :=
  -- https://leanprover-community.github.io/mathlib4_docs/Init/Prelude.html#Eq.subst is replace for Lean.
  -- theorem Eq.subst {α : Sort u}  {motive : α → Prop}  {a b : α}
  --   (h₁ : a = b) (h₂ : motive a) : motive b
  @Eq.subst X MOT FROM TO target base

-- Lean can infer all the parameters in curly braces {}.
-- This means we only need target and base, the mot can be inferred.
theorem replace' {X: Type} {FROM: X} {TO: X} (target: (FROM = TO)) (MOT: X -> Prop) (base: MOT FROM): MOT TO :=
  Eq.subst target base

theorem same {X: Type} (a: X): a = a :=
  Eq.refl a

-- (claim same-cons
--   (Pi ((t U) (x t) (as (List t)) (bs (List t)))
--     (-> (= (List t) as bs)
--         (= (List t) (:: x as) (:: x bs)))))
theorem same_cons:
  ∀ (t: Type) (x: t) (as: List t) (bs: List t),
  (as = bs) -> (x :: as) = (x :: bs) := by
  sorry -- sorry is used, since we do not want to prove it yet.

-- The proof in Pie:
-- (define same-cons
--   (lambda (t x as bs as=bs)
--     (replace
--       as=bs
--       (lambda (xs) (= (List t) (:: x as) (:: x xs)))
--       (same (:: x as))
--       )))
theorem same_cons_replace:
  ∀ (t: Type) (x: t) (as: List t) (bs: List t),
  (as = bs) -> (x :: as) = (x :: bs) :=
  λ _t x as _bs as_eq_bs =>
    replace
      as_eq_bs
      (λ xs => (x :: as) = (x :: xs))
      (same (x :: as))

theorem same_cons_Explicit_Eq_subst:
  ∀ (t: Type) (x: t) (as: List t) (bs: List t),
  (as = bs) -> (x :: as) = (x :: bs) :=
  λ t x as bs as_eq_bs =>
    @Eq.subst (List t) (λ xs => (x :: as) = (x :: xs)) as bs as_eq_bs (Eq.refl (x :: as))

theorem same_cons_Implicit_Eq_subst_with_named:
  ∀ (t: Type) (x: t) (as: List t) (bs: List t),
  (as = bs) -> (x :: as) = (x :: bs) :=
  λ _t x as _bs as_eq_bs =>
    Eq.subst (motive := (λ xs => (x :: as) = (x :: xs))) as_eq_bs (Eq.refl (x :: as))

theorem same_cons_rewrite_same:
  ∀ (t: Type) (x: t) (as: List t) (bs: List t),
  (as = bs) -> (x :: as) = (x :: bs) := by
  intro t x as bs h
  rewrite [h]
  exact (same (x :: bs))

theorem same_cons_rewrite_rfl:
  ∀ (t: Type) (x: t) (as: List t) (bs: List t),
  (as = bs) -> (x :: as) = (x :: bs) := by
  intro t x as bs h
  rewrite [h]
  rfl

theorem same_cons_rw:
  ∀ (t: Type) (x: t) (as: List t) (bs: List t),
  (as = bs) -> (x :: as) = (x :: bs) := by
  intro t x as bs h
  rw [h]

theorem same_cons_simp:
  ∀ (t: Type) (x: t) (as: List t) (bs: List t),
  (as = bs) -> (x :: as) = (x :: bs) := by
  simp

theorem same_cons_simp_only:
  ∀ (t: Type) (x: t) (as: List t) (bs: List t),
  (as = bs) -> (x :: as) = (x :: bs) := by
  simp only [cons.injEq, true_and, imp_self, implies_true] -- found with simp?

#print same_cons_simp
#print same_cons_Implicit_Eq_subst_with_named

-- Advanced

#print same_cons_rewrite_rfl

theorem same_cons_mpr : ∀ (t : Type) (x : t) (as bs : List t),
  as = bs → x :: as = x :: bs :=
  fun t x as bs h => by
    refine Eq.mpr ?_ (Eq.refl (x :: bs))
    have h' := congrArg (fun _a => x :: _a = x :: bs) h
    exact h'
