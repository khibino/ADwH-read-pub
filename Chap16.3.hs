
-- 16.3

---

import Data.Array (listArray, (!), (//))
import qualified Data.Set as S

---

type Nat = Int

type Coord = Nat
type Vertex = (Coord,Coord)
type Box = Vertex
type Grid = (Nat,Nat,[Box])
boxes :: Grid -> [Box]
boxes (_,_,bs) = bs

---

corners :: Box -> [Vertex]
corners (x,y) = [(x, y), (x+1,y), (x+1,y-1), (x,y-1)]

---

type Graph = Vertex -> [Vertex]
neighbours :: Grid -> Graph
neighbours grid = filter (free grid) . adjacents
adjacents :: Vertex -> [Vertex]
adjacents (x,y) = [(x-1, y-1),(x-1, y),(x-1, y+1),
                   (x, y-1),           (x, y+1),
                   (x+1, y-1),(x+1,y),(x+1, y+1)]
free ::Grid -> Vertex -> Bool
free (m,n,bs)=(a!)
  where a = listArray ((0,0),(m+1,n+1)) (repeat True)
            // [((x, y),False) | x <- [0..m+1],y <- [0,n+1]]
            // [((x, y),False) | x <- [0,m+1],y <- [1..n]]
            // [((x, y),False) | b <- bs,(x,y) <- corners b]

---

type Dist = Float
type Path = ([Vertex],Dist)
end :: Path -> Vertex
end = head . fst

---

extract ::Path -> Path
extract (vs,d)=(reverse vs,d)

---

fpath :: Grid -> Vertex -> Vertex -> Maybe Path
fpath grid source target = mstar (neighbours grid) source target

---

mstar :: Graph -> Vertex -> Vertex -> Maybe Path
mstar = undefined

---

succs :: Graph -> Vertex -> S.Set Vertex -> Path -> [(Path,Dist)]
succs g target visited p = [extend p v | v <- g (end p), not (S.member v visited)]
  where extend (u:vs, d) v = ((v:u:vs, dv), dv + dist v target)
                             where dv = d + dist u v

---

-- exercise
dist :: Vertex -> Vertex -> Dist
dist = undefined

---

visible :: Grid -> Segment -> Bool
visible = undefined

---

type Segment = (Vertex,Vertex)

---

vpath ::Grid -> Vertex -> Vertex -> Maybe Path
vpath grid source target = mstarV (neighboursV grid) (visible grid) source target

---

mstarV :: Graph -> (Segment -> Bool) -> Vertex -> Vertex -> Maybe Path
mstarV = undefined

---

neighboursV :: Grid -> Graph
neighboursV (m,n,bs) (x1, y1) = [(x2,y2) | x2 <- [1..m], y2 <- [1..n],visible (m,n,bs) ((x1, y1),(x2,y2))]
