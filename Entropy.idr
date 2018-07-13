module Entropy
import Data.List

%default total

data Probability : a -> Nat -> Type where
  Occurred : (x : a) -> Probability x bits

begin : Probability [] 0
begin = Occurred []

(>>=) : {xs : List Nat} ->
        Probability xs bits ->
        (List Nat -> Probability ys bits') ->
        Probability (ys <+> xs) (bits + bits')
(>>=) {xs} {ys} _ _ = Occurred (ys <+> xs)

-- Imagine this gives some kind of probabilistic score.
model : Nat -> List Nat -> Nat
model Z xs = 1
model (S k) xs = S (S k)

followedBy : (x : Nat) -> (xs : List Nat) -> Probability [x] (model x xs)
followedBy x xs = Occurred [x]

doe : (xs : List Nat) -> Probability (the (List Nat) [0]) (model 0 xs)
doe = followedBy 0

re : (xs : List Nat) -> Probability (the (List Nat) [1]) (model 1 xs)
re = followedBy 1

mi : (xs : List Nat) -> Probability (the (List Nat) [2]) (model 2 xs)
mi = followedBy 2

fa : (xs : List Nat) -> Probability (the (List Nat) [3]) (model 3 xs)
fa = followedBy 3

so : (xs : List Nat) -> Probability (the (List Nat) [4]) (model 4 xs)
so = followedBy 4

la : (xs : List Nat) -> Probability (the (List Nat) [5]) (model 5 xs)
la = followedBy 5

ti : (xs : List Nat) -> Probability (the (List Nat) [6]) (model 6 xs)
ti = followedBy 6

melody : Probability (the (List Nat) [3, 2, 1, 0]) 10
melody = begin >>= doe >>= re >>= mi >>= fa

unconventionalMelody : Probability (the (List Nat) [3, 2, 6, 0]) 15
unconventionalMelody = begin >>= doe >>= ti >>= mi >>= fa
