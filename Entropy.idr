module Entropy

%default total

data Probability : (x : a) ->  (a : Type) -> Nat -> Type where
  Occurred : (x : a) -> Probability x a bits

start : Probability (the (List Integer) []) (List Integer) 0
start = Occurred []

update : Probability x y bits' -> Probability x y (bits + bits')
update (Occurred x) = Occurred x

(>>=) : Probability x a bits -> (a -> Probability y a bits') -> Probability y a (bits + bits')
(Occurred x) >>= bGivenA = update $ bGivenA x

model : Integer -> List Integer -> Nat
model 1 xs = 1
model 2 xs = 2
model 3 xs = 3
model 4 xs = 4
model x xs = 5

followedBy : (x : Integer) -> (xs : List Integer) -> Probability (x :: xs) (List Integer) (model x xs)
followedBy x xs = Occurred (x :: xs)

so : Probability [4] (List Integer) 4
so = start >>= (?followedBy 4)
