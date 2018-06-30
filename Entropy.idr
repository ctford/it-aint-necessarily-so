module Entropy

%default total

data Probability : a -> Nat -> Type where
  Occurred : (x : a) -> Probability x bits

start : Probability (the (List Nat) []) 0
start = Occurred []

update : Probability x bits' -> Probability x (bits + bits')
update (Occurred x) = Occurred x

(>>=) : {x : a} -> {y : a} -> Probability x bits -> ((x : a) -> Probability y bits') -> Probability y (bits + bits')
(Occurred x) >>= bGivenA = update $ bGivenA x

model : Nat -> List Nat -> Nat
model x xs = x

followedBy : (x : Nat) -> (xs : List Nat) -> Probability (x :: xs) (model x xs)
followedBy x xs = Occurred (x :: xs)

so : Probability (the (List Nat) [4]) 4
so = start >>= (?followedBy 4)
