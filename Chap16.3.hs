{-# OPTIONS_GHC -Wno-name-shadowing #-}

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

-- Exercise 16.11
dist :: Vertex -> Vertex -> Dist
dist (x1, y1) (x2, y2) = sqrt $ fromIntegral $ square (x1 - x2) + square (y1 - y2)
  where square x = x * x

---

borders :: Grid -> [Segment]
borders = concatMap (edges . corners) . boxes
  where edges [u,v,w,x] = [ (u,v), (w,v), (x,w), (x,u) ]
        edges cs        = error $ "borders.edges: wrong number of corners: " ++ show cs

near :: Segment -> Segment -> Bool
near ((x1,y1), (x2,y2)) ((x3,y3), (x4,y4)) =
    min x1 x2 <= x3 && x4 <= max x1 x2 &&
    min y1 y2 <= y3 && y4 <= max y1 y2

{-
near ((x1,y1), (x2,y2)) ((x3,y3), (x4,y4)) =
    (min x1 x2 <= x3 || x4 <= max x1 x2) &&
    (min y1 y2 <= y3 || y4 <= max y1 y2)
 -}

orientation :: Segment -> Vertex -> Int
orientation ((x1,y1),(x2,y2)) (x, y) = signum ((x-x1) * (y2-y1) - (x2-x1) * (y-y1))

crosses :: Segment -> Segment -> Bool
crosses p (q1, q2) =
  orientation p q1 * orientation p q2 <= 0

visible :: Grid -> Segment -> Bool
visible g s
  | hseg s     = all (free g) (ypoints s)
  | vseg s     = all (free g) (xpoints s)
  | dseg s     = all (free g) (dpoints s)
  | eseg s     = all (free g) (epoints s)
  | otherwise  = free g (snd s) && all (not . crosses s) es
  where
    es = filter (near s) (borders g)

    hseg (( _, y1), ( _, y2)) = y1 == y2
    vseg ((x1,  _), (x2,  _)) = x1 == x2
    dseg ((x1, y1), (x2, y2)) = x1 + y1 == x2 + y2
    eseg ((x1, y1), (x2, y2)) = x1 - y1 == x2 - y2

    ypoints ((x1, y ), (x2, _ )) = [ (x, y) | x <- [min x1 x2 .. max x1 x2] ]
    xpoints ((x , y1), (_ , y2)) = [ (x, y) | y <- [min y1 y2 .. max y1 y2] ]
    dpoints ((x1, y1), (x2, _ )) = [ (x, y) | x <- [min x1 x2 .. max x1 x2], let y = x1 + y1 - x ]
    epoints ((x1, y1), (_ , y2)) = [ (x, y) | y <- [min y1 y2 .. max y1 y2], let x = x1 - y1 + y ]
    -- epoints ((x1, y1), (x2, _ )) = [ (x, y) | x <- [min x1 x2 .. max x1 x2], let y = x1 - y1 + x ]

---

type Segment = (Vertex,Vertex)

---

vpath0 ::Grid -> Vertex -> Vertex -> Maybe Path
vpath0 grid source target = mstarV (neighbours grid) (visible grid) source target

---

vpath ::Grid -> Vertex -> Vertex -> Maybe Path
vpath grid source target = mstarV (neighboursV grid) (visible grid) source target

---

pq_mstarV :: Graph -> (Segment -> Bool) -> Vertex -> Vertex -> Maybe Path
pq_mstarV g vtest source target = msearch S.empty start
  where start = PQ.insertQ ([source],0) (dist source target) PQ.emptyQ
        msearch vs ps | PQ.nullQ ps = Nothing
                      | (== target) (end p) = Just (extract p)
                      | seen (end p) = msearch vs qs
                      | otherwise = msearch (S.insert (end p) vs) rs
          where seen v = S.member v vs
                (p,qs) = PQ.removeQ ps
                rs = PQ.addListQ (succsV g vtest target vs p) qs

---

mstarV :: Graph -> (Segment -> Bool) -> Vertex -> Vertex -> Maybe Path
mstarV g vtest source target = msearch S.empty start
  where start = PSQ.insertQ end ([source],0) (dist source target) PSQ.emptyQ
        msearch vs ps | PSQ.nullQ ps = Nothing
                      | (== target) (end p) = Just (extract p)
                      | seen (end p) = msearch vs qs
                      | otherwise = msearch (S.insert (end p) vs) rs
          where seen v = S.member v vs
                (p,qs) = PSQ.removeQ ps
                rs = PSQ.addListQ end (succsV g vtest target vs p) qs

---

succsV :: Graph -> (Segment -> Bool) -> Vertex -> S.Set Vertex -> Path -> [(Path,Dist)]
succsV g vtest target vs p = [extend p w | w <- g (end p),not (S.member w vs)]
  where extend (v:vs, d) w = if not (null vs) && vtest (u,w)
                             then ((w:vs, du), du + dist w target)
                             else ((w:v:vs, dw), dw + dist w target)
                             where u = head vs
                                   du = d - dist u v + dist u w
                                   dw = d + dist v w

---

neighboursV :: Grid -> Graph
neighboursV (m,n,bs) (x1, y1) = [(x2,y2) | x2 <- [1..m], y2 <- [1..n],visible (m,n,bs) ((x1, y1),(x2,y2))]

---

grid163 :: Grid
grid163 = (20, 10, boxes)
  where
    boxes = [                                         (10, 10), (11, 10), (12, 10),                               (18, 10)
            ,                                                                                                     (18,  9)
            , ( 2,  8), ( 3,  8), ( 7,  8),                                                                       (18,  8)
            ,                                         (10,  7),           (12,  7),                               (18,  7)
            ,                                                                                 (14,  6),           (18,  6)
            , ( 2,  5), ( 3,  5), ( 7,  5), ( 8,  5), (10,  5), (11,  5)

            ,                               ( 8,  3), (10,  3),                     (13,  3),           (17,  3), (18,  3), (19,  3)
            , ( 5,  2)
            ]

fpathG163 :: Maybe Path
fpathG163 = fpath grid163 (1, 10) (20, 1)

vpath0G163 :: Maybe Path
vpath0G163 = vpath0 grid163 (1, 10) (20, 1)

vpathG163 :: Maybe Path
vpathG163 = vpath grid163 (1, 10) (20, 1)
