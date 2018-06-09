module Entropy

%default total

data Probability : Type -> Nat -> Type where
  Occurred : a -> Probability a bits

pure : a -> Probability a bits
pure = Occurred

extract : Probability a bits -> a
extract (Occurred x) = x

(>>=) : Probability a bits -> (a -> Probability b bits') -> Probability b (bits + bits')
(Occurred a) >>= bGivenA = pure $ extract $ bGivenA a

nothing : Probability (List Nat) 0
nothing = pure []

so : Probability (List Nat) 1
so = Occurred [4]

re : Probability (List Nat) 2
re = Occurred [1]

and : Probability (List Nat) bits -> List Nat -> Probability (List Nat) bits
and (Occurred xs) ys = Occurred (xs ++ ys)

up : List Nat -> Probability (List Nat) 1
up xs = Occurred (3 :: xs)

run : Probability (List Nat) 4
run = so >>= up >>= (and re)
