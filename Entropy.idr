module Entropy

%default total

data Surprise a = Occurred a Nat

empty : Surprise (List Nat)
empty = Occurred [] 0

so : Surprise (List Nat)
so = Occurred [4] 1

re : Surprise (List Nat)
re = Occurred [1] 2

and : Surprise (List Nat) -> List Nat -> Surprise (List Nat)
and (Occurred xs n) ys = Occurred (xs ++ ys) n

up : List Nat -> Surprise (List Nat)
up xs = Occurred (3 :: xs) 1

(>>=) : Surprise a -> (a -> Surprise b) -> Surprise b
(Occurred x n) >>= f = let (Occurred y n') = f x in (Occurred y (n + n'))

run : Surprise (List Nat)
run = so >>= up >>= (and re)
