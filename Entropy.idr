module Data.Entropy
import Data.Vect

%default total

data Solfege = Do | Re | Mi | Fa | So | La | Ti

entropy : Solfege -> Solfege -> Nat
entropy Do Do = 2
entropy Re Do = 2
entropy Mi Do = 3
entropy Fa Do = 7
entropy So Do = 3
entropy La Do = 5
entropy Ti Do = 8 -- boosted from 2 because of wrap-around affect

entropy Do Re = 2
entropy Re Re = 2
entropy Mi Re = 2
entropy Fa Re = 3
entropy So Re = 5
entropy La Re = 5
entropy Ti Re = 4

entropy Do Mi = 3
entropy Re Mi = 2
entropy Mi Mi = 2
entropy Fa Mi = 1
entropy So Mi = 3
entropy La Mi = 7
entropy Ti Mi = 7

entropy Do Fa = 6
entropy Re Fa = 4
entropy Mi Fa = 3
entropy Fa Fa = 3
entropy So Fa = 2
entropy La Fa = 4
entropy Ti Fa = 8

entropy Do So = 3
entropy Re So = 4
entropy Mi So = 3
entropy Fa So = 2
entropy So So = 2
entropy La So = 1
entropy Ti So = 4

entropy Do La = 4
entropy Re La = 6
entropy Mi La = 6
entropy Fa La = 4
entropy So La = 3
entropy La La = 2
entropy Ti La = 1

entropy Do Ti = 8 -- boosted from 2 because of wrap-around affect
entropy Re Ti = 4
entropy Mi Ti = 9
entropy Fa Ti = 6
entropy So Ti = 6
entropy La Ti = 3
entropy Ti Ti = 4

Range : Type
Range = (Nat, Nat)

data Series : (t -> t -> Nat) -> (t, t) -> Range -> Type where
  Pure   : (x : t2) -> Series cost (x, x) (0, 0)
  (>>=)  : Series cost (w, x) (l, u) ->
           (() -> Series cost (y, z) (l', u')) ->
           Series cost (w, z) (cost x y + l + l', (cost x y + u + u'))
  Widen  : Series cost (x, y) (l' + dl, u) -> Series cost (x, y) (l', u + du)

Interval : Type
Interval = (Solfege, Solfege)

Melody : Interval -> Range -> Type
Melody = Series entropy

conventional : Melody (Do, So) (8, 16)
conventional = Widen $ do Pure Do; Pure Re; Pure Mi; Pure Fa; Pure So

unconventional : Melody (Do, So) (12, 24)
unconventional = Widen $ do Pure Do; Pure Ti; Pure Mi; Pure Fa; Pure So

song : Melody (Do, So) (20, 50)
song = Widen $ do conventional; unconventional
