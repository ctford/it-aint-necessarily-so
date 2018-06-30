module Entropy

%default total

data Probability : a -> Nat -> Type where
  Occurred : (x : a) -> Probability x bits

begin : Probability [] 0
begin = Occurred []

(>>=) : {x : List Nat} -> {y : List Nat} -> Probability x bits -> ((x : List Nat) -> Probability y bits') -> Probability (y <+> x) (bits + bits')
(>>=) {x} {y} _ _ = Occurred (y <+> x)

-- Imagine this gives some kind of probabilistic score.
model : Nat -> List Nat -> Nat
model x xs = x+1

followedBy : (x : Nat) -> (xs : List Nat) -> Probability [x] (model x xs)
followedBy x xs = Occurred [x]

doe : List Nat -> Probability (the (List Nat) [0]) 1
doe = followedBy 0

re : List Nat -> Probability (the (List Nat) [1]) 2
re = followedBy 1

mi : List Nat -> Probability (the (List Nat) [2]) 3
mi = followedBy 2

fa : List Nat -> Probability (the (List Nat) [3]) 4
fa = followedBy 3

so : List Nat -> Probability (the (List Nat) [4]) 5
so = followedBy 4

la : List Nat -> Probability (the (List Nat) [5]) 6
la = followedBy 5

ti : List Nat -> Probability (the (List Nat) [6]) 7
ti = followedBy 6

melody : Probability (the (List Nat) [2, 3, 4, 4, 0]) 18
melody = begin >>= doe >>= so >>= so >>= fa >>= mi
