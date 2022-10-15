
import qualified Data.Map as M

---

-- 16.1

---

data ExampleVs
  = A | B | C | D
  deriving (Eq, Ord, Show)

type Nat = Int
type Vertex = ExampleVs

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

tstar :: Graph -> Heuristic -> (Vertex -> Bool) -> Vertex -> Maybe Path
tstar g h goal source = tsearch start
  where start = insertQ ([source],0) (h source) emptyQ
        tsearch ps | nullQ ps = Nothing
                   | goal (end p) = Just (extract p)
                   | otherwise = tsearch rs
          where (p,qs) = removeQ ps
                rs = addListQ (succs g h p) qs

---

succs :: Graph -> Heuristic -> Path -> [(Path,Cost)]
succs g h (u:vs, c) = [((v:u:vs, c + d), c + d + h v) | (v,d) <- g u]
succs _ _ ([]  , _) = error "succs: null path."

---

goalC :: Vertex -> Bool
goalC C = True
goalC _ = False

h1 :: Heuristic
h1 = const 0

g1 :: Graph
g1 A = [(B, 1)]
g1 B = [(A, 1)]
g1 C = []
g1 D = error "undefined vertex D"

g2 :: Graph
g2 A = [(B, 1)]
g2 B = [(A, 1), (C, 100)]
g2 C = []
g2 D = error "undefined vertex D"

g3 :: Graph
g3 A = [(B, 1), (D, 1)]
g3 B = [(A, 1), (C, 100)]
g3 C = []
g3 D = [(A, 1)]

goalD :: Vertex -> Bool
goalD D = True
goalD _ = False

h161 :: Heuristic
h161 v = case v of
  A -> 9
  B -> 1
  C -> 5
  D -> 0

g161 :: Graph
g161 A = [(B, 5), (C, 2)]
g161 B = [(D, 5)]
g161 C = [(B, 2)]
g161 D = []

---

astar :: Graph -> Heuristic -> (Vertex -> Bool) -> Vertex -> Maybe Path
astar g h goal source = asearch M.empty start
  where start = insertQ ([source],0) (h source) emptyQ
        asearch vcmap ps | nullQ ps = Nothing
                         | goal (end p) = Just (extract p)
                         | better p vcmap = asearch vcmap qs
                         | otherwise = asearch (add p vcmap) rs
                         where (p,qs) = removeQ ps
                               rs = addListQ (succs g h p) qs
better :: Path -> M.Map Vertex Cost -> Bool
better (v:_vs, c) vcmap = query (M.lookup v vcmap)
                         where query Nothing = False
                               query (Just c') = c' <= c
better ([],   _) _     = error "better: null path."
add :: Path -> M.Map Vertex Cost -> M.Map Vertex Cost
add (v:_vs,c) vcmap = M.insert v c vcmap
add ([]   ,_) _ = error "add: null path."
