
-- 16.4

import Data.List (sort)
import Data.Array (Array, listArray, (!))
import qualified Data.Set as S
import qualified Data.Text as T

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

---

type Move = Nat
moves:: State -> [Move]
moves st = moveTable !(posn0 st)
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

icparity = undefined
mhparity = undefined

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

coords :: State -> [Coord]
coords = tail . map snd . sort . addCoords
         where addCoords st = zip (perm st) gridpoints
               gridpoints = map (divMod 3) [0..8]

---

h2 :: Heuristic
h2 is fs = sum (zipWith d (coords is) (coords fs))
           where d (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)

---

type Path = ([Move],Nat,State)
key :: Path -> State
key (ms,k,st) = st

---

-- key がはさまっていると型が合わないので削除した
mstar :: Heuristic -> State -> State -> Maybe [Move]
mstar h istate fstate =
  if possible istate fstate then msearch S.empty start else Nothing
  where start = insertQ ([],0,istate) (h istate fstate) emptyQ
        msearch vs ps | st == fstate = Just (reverse ms)
                      | S.member st vs = msearch vs qs
                      | otherwise = msearch (S.insert st vs) rs
          where ((ms, k,st),qs) = removeQ ps
                rs = addListQ (succs h fstate (ms, k,st) vs) qs

---

succs :: Heuristic -> State -> Path -> S.Set State -> [(Path,Nat)]
succs h fstate (ms,k,st) vs =
  [((m:ms, k+1,st'), k+1+h st' fstate)
  | m <- moves st, let st' = move st m, not (S.member st' vs) ]

---

data PQ a p = Null | Fork Rank a p (PQ a p) (PQ a p)
type Rank = Nat

insertQ :: Ord p => a -> p -> PQ a p -> PQ a p
addListQ :: Ord p => [(a,p)] -> PQ a p -> PQ a p
deleteQ :: Ord p => PQ a p -> ((a,p),PQ a p)
emptyQ :: PQ a p
nullQ :: PQ a p -> Bool

---

removeQ  :: Ord p => PQ a p -> (a,PQ a p)
removeQ q1 = (x,q2) where ((x,_),q2) = deleteQ q1

---

toListQ :: Ord p => PQ a p -> [(a,p)]
toListQ Null = []
toListQ (Fork _ x p t1 t2) = (x,p):mergeOn snd (toListQ t1) (toListQ t2)

fork :: a -> p -> PQ a p -> PQ a p -> PQ a p
fork x p t1 t2
  | r2 <= r1 = Fork (r2 + 1) x p t1 t2
  | otherwise = Fork (r1 + 1) x p t2 t1
  where r1 = rank t1; r2 = rank t2
rank ::PQ a p -> Rank
rank Null = 0
rank (Fork r _ _ _ _) = r

combineQ :: Ord p => PQ a p -> PQ a p -> PQ a p
combineQ Null t = t
combineQ t Null = t
combineQ (Fork k1 x1 p1 l1 r1) (Fork k2 x2 p2 l2 r2)
  | p1 <= p2  = fork x1 p1 l1 (combineQ r1 (Fork k2 x2 p2 l2 r2))
  | otherwise = fork x2 p2 l2 (combineQ (Fork k1 x1 p1 l1 r1) r2)

insertQ x p t = combineQ (fork x p Null Null) t

deleteQ (Fork _ x p t1 t2) = ((x,p), combineQ t1 t2)
deleteQ  Null              = error "deleteQ: empty queue."

mergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeOn f = disp
  where
    disp []        ys   = ys
    disp xs@(_:_)  []   = xs
    disp xxs@(x:xs) yys@(y:ys)
      | f x <= f y      = x : disp xs yys
      | otherwise       = y : disp xxs ys

emptyQ = Null

nullQ Null      = True
nullQ (Fork {}) = False

addListQ ps q = foldr (uncurry insertQ) q ps
