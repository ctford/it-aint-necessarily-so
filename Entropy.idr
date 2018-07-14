||| Provide session types derived from specifications.
module Data.FSM.Entropy

import Data.List

%default total

%access public export

data Solfege = Beginning | End | Do | Re | Mi | Fa | So | La | Ti

surprise : Solfege -> Solfege -> Nat
surprise Beginning Do = 1
surprise Beginning _  = 2
surprise Do End       = 1
surprise _ End        = 2
surprise Do Do = 1
surprise Do Re = 2
surprise Re Do = 2
surprise Re Re = 1
surprise Re Mi = 2
surprise Mi Re = 2
surprise Mi Mi = 1
surprise Mi Fa = 2
surprise Fa Mi = 2
surprise Fa Fa = 1
surprise Fa So = 2
surprise So Fa = 2
surprise So So = 1
surprise So La = 2
surprise La So = 2
surprise La La = 1
surprise La Ti = 2
surprise Ti La = 2
surprise Ti Ti = 1
surprise _ _ = 3

data Melody : Nat -> Solfege -> Solfege -> Type where
  FollowedBy : (s : Solfege) -> Melody (surprise a b) a b
  (>>=) : Melody bits a b ->
       ((bits : Nat) -> Melody bits' b c) ->
       Melody (bits + bits') a c

doe : Melody (surprise a Do) a Do
doe = FollowedBy Do

re : Melody (surprise a Re) a Re
re = FollowedBy Re

mi : Melody (surprise a Mi) a Mi
mi = FollowedBy Mi

so : Melody (surprise a So) a So
so = FollowedBy So

ti : Melody (surprise a Ti) a Ti
ti = FollowedBy Ti

end : Melody (surprise a End) a End
end = FollowedBy End

conventional : Melody 10 Beginning End
conventional = do
  doe
  re
  mi
  so
  end

unconventional : Melody 12 Beginning End
unconventional = do
  doe
  ti
  mi
  so
  end
