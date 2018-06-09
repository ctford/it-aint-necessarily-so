module Entropy

%default total

data Surprise : Type -> Nat -> Type where
  Occurred : a -> Surprise a n

empty : Surprise (List Nat) 0
empty = Occurred []

so : Surprise (List Nat) 1
so = Occurred [4]

re : Surprise (List Nat) 2
re = Occurred [1]

and : Surprise (List Nat) n -> List Nat -> Surprise (List Nat) n
and (Occurred xs) ys = Occurred (xs ++ ys)

up : List Nat -> Surprise (List Nat) 1
up xs = Occurred (3 :: xs)

(>>=) : Surprise a entropyA -> (a -> Surprise b entropyB) -> Surprise b (entropyA + entropyB)
(Occurred a) >>= bGivenA = let (Occurred b) = bGivenA a in (Occurred b)

run : Surprise (List Nat) 4
run = so >>= up >>= (and re)
