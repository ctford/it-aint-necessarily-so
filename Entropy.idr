module Entropy

%default total

data Probability : (x : a) ->  (a : Type) -> Nat -> Type where
  Occurred : (x : a) -> Probability x a bits

start : Probability (the (List Nat) []) (List Nat) 0
start = Occurred []

update : Probability x y bits' -> Probability x y (bits + bits')
update (Occurred x) = Occurred x

(>>=) : Probability x a bits -> (a -> Probability y a bits') -> Probability y a (bits + bits')
(Occurred x) >>= bGivenA = update $ bGivenA x

model : Nat -> List Nat -> Nat
model x xs = x

followedBy : (x : Nat) -> (xs : List Nat) -> Probability (x :: xs) (List Nat) (model x xs)
followedBy x xs = Occurred (x :: xs)

so : Probability (the (List Nat) [4]) (List Nat) 4
so = start >>= (?followedBy 4)
