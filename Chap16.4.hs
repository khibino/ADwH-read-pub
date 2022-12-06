
-- 16.4

import Data.List (sort, tails)
import Data.Array (Array, listArray, (!))
import qualified Data.Set as S
import qualified Data.Text as T

import Chap16PSQ

---

type Nat = Int

type Position = Nat
type State = (T.Text,Position)
perm :: State -> String
perm (xs,j) = T.unpack xs
posn0 :: State -> Position
posn0 (xs,j) = j

---

istate,fstate :: State
istate = (T.pack "083256147",0)
fstate = (T.pack "123456780",8)

istate2 :: State
istate2 = (T.pack "032871456", 0)

{-
ghci> :set +s
ghci>  mstar h1 istate fstate
Just [3,6,7,8,5,4,1,0,3,6,7,4,5,8]
(0.01 secs, 2,555,200 bytes)
ghci>  mstar h2 istate fstate
Just [3,6,7,8,5,4,1,0,3,6,7,4,5,8]
(0.00 secs, 1,216,000 bytes)
ghci>  mstar h1 istate2 fstate
Just [1,4,3,6,7,4,1,0,3,4,5,2,1,4,5,8,7,6,3,0,1,4,7,8]
(0.60 secs, 320,906,248 bytes)
ghci>  mstar h2 istate2 fstate
Just [1,4,3,6,7,4,1,0,3,4,5,2,1,4,5,8,7,6,3,0,1,4,7,8]
(0.09 secs, 63,711,384 bytes)
 -}

---

type Move = Nat
moves:: State -> [Move]
moves st = moveTable ! (posn0 st)
moveTable ::Array Nat [Nat]
moveTable = listArray (0,8) [[1,3], [0,2,4], [1,5],
                             [0,4,6],[1,3,5,7],[2,4,8],
                             [3,7], [4,6,8], [5,7]]

move :: State -> Move -> State
move (xs,i) j = (T.replace ty t0 (T.replace t0 tx (T.replace tx ty xs)),j)
                where t0 = T.singleton '0'
                      ty = T.singleton '?'
                      tx = T.singleton (T.index xs j)

---

icparity :: State -> Bool
mhparity :: State -> State -> Bool

icparity st = even $ sum [ length $ filter (h >) cs | h:cs  <- tails (perm st) ]

mhparity istate fstate = even $ d (zpoint istate) (zpoint fstate)
  where d (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)

---

possible :: State -> State -> Bool
possible is fs = (mhparity is fs == (icparity is == icparity fs))

---

type Heuristic = State -> State -> Nat
h1 :: Heuristic
h1 is fs = length (filter p (zip (perm is) (perm fs)))
           where p (c,d) = c /= '0' && c /= d

---

type Coord = (Nat,Nat)

---

zpoint :: State -> Coord
zpoint st = gridpoints !! posn0 st
  where gridpoints = map (`divMod` 3) [0..8]

---

coords :: State -> [Coord]
coords = tail . map snd . sort . addCoords
         where addCoords st = zip (perm st) gridpoints
               gridpoints = map (`divMod` 3) [0..8]

---

h2 :: Heuristic
h2 is fs = sum (zipWith d (coords is) (coords fs))
           where d (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)

---

type Path = ([Move],Nat,State)
key :: Path -> State
key (ms,k,st) = st

---

mstar :: Heuristic -> State -> State -> Maybe [Move]
mstar h istate fstate =
  if possible istate fstate then msearch S.empty start else Nothing
  where start = insertQ key ([],0,istate) (h istate fstate) emptyQ
        msearch vs ps | st == fstate = Just (reverse ms)
                      | S.member st vs = msearch vs qs
                      | otherwise = msearch (S.insert st vs) rs
          where ((ms, k, st), qs) = removeQ ps
                rs = addListQ key (succs h fstate (ms, k, st) vs) qs

---

succs :: Heuristic -> State -> Path -> S.Set State -> [(Path,Nat)]
succs h fstate (ms,k,st) vs =
  [((m:ms, k+1, st'), k + 1 + h st' fstate)
  | m <- moves st, let st' = move st m, not (S.member st' vs) ]
