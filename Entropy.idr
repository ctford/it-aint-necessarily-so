||| Provide session types derived from specifications.
module Data.FSM.Entropy

import Data.List

%default total

%access public export

data Solfege = Do | Re | Mi | Fa | So | La | Ti

entropy : Solfege -> Solfege -> Nat
entropy Do Do = 1
entropy Do Re = 2
entropy Re Do = 2
entropy Re Re = 1
entropy Re Mi = 2
entropy Mi Re = 2
entropy Mi Mi = 1
entropy Mi Fa = 2
entropy Fa Mi = 2
entropy Fa Fa = 1
entropy Fa So = 2
entropy So Fa = 2
entropy So So = 1
entropy So La = 2
entropy La So = 2
entropy La La = 1
entropy La Ti = 2
entropy Ti La = 2
entropy Ti Ti = 1
entropy _ _ = 4

data Melody : Nat -> Solfege -> Solfege -> Type where
  Then : (b : Solfege) -> Melody (entropy a b) a b
  (>>=) : Melody bits a b ->
       ((bits : Nat) -> Melody bits' b c) ->
       Melody (bits + bits') a c

conventional : Melody 8 Do So
conventional = do Then Re; Then Mi; Then Fa; Then So

unconventional : Melody 12 Do So
unconventional = do Then Ti; Then Mi; Then Fa; Then So
