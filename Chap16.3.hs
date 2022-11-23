
-- 16.3

---

import Data.Array (listArray, (!), (//))
import qualified Data.Set as S

import qualified Chap16PQ as PQ
import qualified Chap16PSQ as PSQ

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
                   (x,   y-1),         (x,   y+1),
                   (x+1, y-1),(x+1, y),(x+1, y+1)]
free ::Grid -> Vertex -> Bool
free (m,n,bs)=(a!)
  where a = listArray ((0,0),(m+1,n+1)) (repeat True)
            // [((x, y),False) | x <- [0..m+1],y <- [0,n+1]]
            // [((x, y),False) | x <- [0, m+1],y <- [1..n]]
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

pq_mstar :: Graph -> Vertex -> Vertex -> Maybe Path
pq_mstar g source target = msearch S.empty start
  where start = PQ.insertQ ([source],0) (dist source target) PQ.emptyQ
        msearch vs ps | PQ.nullQ ps = Nothing
                      | (== target) (end p) = Just (extract p)
                      | seen (end p) = msearch vs qs
                      | otherwise = msearch (S.insert (end p) vs) rs
          where seen v = S.member v vs
                (p,qs) = PQ.removeQ ps
                rs = PQ.addListQ (succs g target vs p) qs

---

mstar :: Graph -> Vertex -> Vertex -> Maybe Path
mstar g source target = msearch S.empty start
  where start = PSQ.insertQ end ([source],0) (dist source target) PSQ.emptyQ
        msearch vs ps | PSQ.nullQ ps = Nothing
                      | (== target) (end p) = Just (extract p)
                      | seen (end p) = msearch vs qs
                      | otherwise = msearch (S.insert (end p) vs) rs
          where seen v = S.member v vs
                (p,qs) = PSQ.removeQ ps
                rs = PSQ.addListQ end (succs g target vs p) qs

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

succsV :: Graph -> (Segment -> Bool) -> Vertex -> S.Set Vertex -> Path -> [(Path,Dist)]
succsV g vtest target vs p = [extend p w | w <- g (end p),not (S.member w vs)]
  where extend (v: vs,d) w = if not (null vs) && vtest (u,w)
                             then ((w: vs,du),du+dist w target)
                             else ((w: v: vs,dw),dw+dist w target)
                             where u = head vs
                                   du = d - dist u v + dist u w
                                   dw = d + dist v w

---

neighboursV :: Grid -> Graph
neighboursV (m,n,bs) (x1, y1) = [(x2,y2) | x2 <- [1..m], y2 <- [1..n],visible (m,n,bs) ((x1, y1),(x2,y2))]
