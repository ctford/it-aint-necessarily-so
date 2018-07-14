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
entropy _ _ = 3

data Melody : Nat -> Solfege -> Solfege -> Type where
  Then : (s : Solfege) -> Melody (entropy a b) a b
  (>>=) : Melody bits a b ->
       ((bits : Nat) -> Melody bits' b c) ->
       Melody (bits + bits') a c

doe : Melody (entropy a Do) a Do
doe = Then Do

re : Melody (entropy a Re) a Re
re = Then Re

mi : Melody (entropy a Mi) a Mi
mi = Then Mi

so : Melody (entropy a So) a So
so = Then So

fa : Melody (entropy a Fa) a Fa
fa = Then Fa

ti : Melody (entropy a Ti) a Ti
ti = Then Ti

conventional : Melody 9 Do Fa
conventional = do re; mi; so; fa

unconventional : Melody 11 Do Fa
unconventional = do ti; mi; so; fa
