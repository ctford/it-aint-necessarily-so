module Entropy

%default total

data Probability : a -> Nat -> Type where
  Occurred : (x : a) -> Probability x bits

begin : Probability [] 0
begin = Occurred []

extract : {x : a} -> Probability x bits' -> a
extract {x} _ = x

(>>=) : {x : List Nat} -> {y : List Nat} -> Probability x bits -> ((x : List Nat) -> Probability y bits') -> Probability (y ++ x) (bits + bits')
(>>=) (Occurred x) bGivenA = Occurred $ (++ x) $ extract $ bGivenA x

-- Imagine this gives some kind of probabilistic score.
model : Nat -> List Nat -> Nat
model x xs = x

followedBy : (x : Nat) -> (xs : List Nat) -> Probability [x] (model x xs)
followedBy x xs = Occurred [x]

mi : List Nat -> Probability (the (List Nat) [2]) 2
mi = followedBy 2

so : List Nat -> Probability (the (List Nat) [4]) 4
so = followedBy 4

fa : List Nat -> Probability (the (List Nat) [3]) 3
fa = followedBy 3

melody : Probability (the (List Nat) [2, 3, 4]) 9
melody = begin >>= so >>= fa >>= mi
