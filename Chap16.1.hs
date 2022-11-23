
import qualified Data.Map as M

import Chap16PQ (insertQ, addListQ, removeQ, emptyQ, nullQ)

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
