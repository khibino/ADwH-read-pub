
import qualified Data.Set as S

-- 16.2

---

type Nat = Int
type Vertex = ()

---

type Cost = Nat
type Graph = Vertex -> [(Vertex,Cost)]
type Heuristic = Vertex -> Cost
type Path = ([Vertex],Cost)

---

end :: Path -> Vertex
end = head . fst
cost :: Path -> Cost
cost = snd
extract :: Path -> Path
extract (vs, c) = (reverse vs, c)

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

---

mstar :: Graph -> Heuristic -> (Vertex -> Bool) -> Vertex -> Maybe Path
mstar g h goal source = msearch S.empty start
  where start = insertQ ([source],0) (h source) emptyQ
        msearch vs ps | nullQ ps = Nothing
                      | goal (end p) = Just (extract p)
                      | seen (end p) = msearch vs qs
                      | otherwise = msearch (S.insert (end p) vs) rs
          where seen v = S.member v vs
                (p,qs) = removeQ ps
                rs = addListQ (succs g h vs p) qs

---

succs :: Graph -> Heuristic -> S.Set Vertex -> Path -> [(Path,Cost)]
succs g h vs p = [extend p v d | (v,d) <- g (end p),not (S.member v vs)]
  where extend (vs, c) v d = ((v:vs, c+d), c+d + h v)
