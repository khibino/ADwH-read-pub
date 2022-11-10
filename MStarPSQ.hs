
import qualified Data.Set as S

import Chap16PSQ (insertQ, addListQ, emptyQ, nullQ, removeQ)

-- 16.2

---

data ExampleVs
  = A | B | C | D | E | F
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

mstar :: Graph -> Heuristic -> (Vertex -> Bool) -> Vertex -> Maybe Path
mstar g h goal source = msearch S.empty start
  where start = insertQ end ([source],0) (h source) emptyQ
        msearch vs ps | nullQ ps = Nothing
                      | goal (end p) = Just (extract p)
                      | seen (end p) = msearch vs qs
                      | otherwise = msearch (S.insert (end p) vs) rs
          where seen v = S.member v vs
                (p,qs) = removeQ ps
                rs = addListQ end (succs g h vs p) qs

---

succs :: Graph -> Heuristic -> S.Set Vertex -> Path -> [(Path,Cost)]
succs g h vs p = [extend p v d | (v,d) <- g (end p),not (S.member v vs)]
  where extend (vs, c) v d = ((v:vs, c+d), c+d + h v)


---

h :: Heuristic
h v = case v of
  A -> 10
  B -> 10
  C -> 5
  D -> 5
  E -> 0
  F -> 0

g :: Graph
g A = [(B, 3), (C, 10), (D, 20), (E, 20)]
g B = [(A, 3), (C, 5), (D, 8), (E, 20)]
g C = [(A, 10), (B, 5), (D, 2), (E, 10)]
g D = [(A, 20), (B, 8), (C, 2), (E, 6)]
g E = [(A, 20), (B, 20), (C, 10), (D, 6), (F, 1)]
g F = []
