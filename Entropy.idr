module Entropy

%default total

data Probability : Type -> Nat -> Type where
    Occurred : a -> Probability a bits

empty : Probability (List Nat) 0
empty = Occurred []

so : Probability (List Nat) 1
so = Occurred [4]

re : Probability (List Nat) 2
re = Occurred [1]

and : Probability (List Nat) bits -> List Nat -> Probability (List Nat) bits
and (Occurred xs) ys = Occurred (xs ++ ys)

up : List Nat -> Probability (List Nat) 1
up xs = Occurred (3 :: xs)

(>>=) : Probability a entropyA -> (a -> Probability b entropyB) -> Probability b (entropyA + entropyB)
(Occurred a) >>= bGivenA = let (Occurred b) = bGivenA a in (Occurred b)

run : Probability (List Nat) 4
run = so >>= up >>= (and re)
