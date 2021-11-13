module Partition where

{-
定義によると、空でないリストのパーティションとは、リストを空でないセグメントに分割することです。
例えば、["par", "tit", "i", "on"]は、文字列 "partition "の1つのパーティションです。

この章では、2つの例を紹介します。
1つ目は単純なスケジューリング問題、2つ目は段落を行に分割する問題です。
 -}

{-
パーティションとは、リストを空でないセグメントに分割すること

この章では Partition の応用例を 2つ扱う
 -}

{- 12.1  Ways of generating partitions -}

type Partition a = [Segment a]
type Segment a = [a]

-- concat xss = xs /\ all (not . null) xss
-- 順番も保っている必要

parts :: [a] -> [Partition a]
parts [] = [[]]
parts xs = [ys:yss|(ys,zs) <- splits xs, yss <- parts zs]

splits :: [a] -> [([a],[a])]
splits [] = []
splits (x:xs) = ([x],xs):[(x:ys,zs)|(ys,zs) <- splits xs]

partsR :: [a] -> [Partition a]
partsR = foldr (concatMap . extendl) [[]]

extendl :: a -> Partition a -> [Partition a]
extendl x [] = [cons x []]
extendl x p  = [cons x p, glue x p]

cons, glue :: a -> Partition a -> Partition a
cons x p = [x]:p
glue x (s:p) = (x:s):p

partsL :: [a] -> [Partition a]
partsL = foldl (flip (concatMap . extendr)) [[]]

extendr :: a -> Partition a -> [Partition a]
extendr x [] = [snoc x []]
extendr x p  = [snoc x p,bind x p]

snoc, bind :: a -> Partition a -> Partition a
snoc x p = p ++ [[x]]
bind x p = init p ++ [last p ++ [x]]

-- filter (all ok) . parts == foldr (concatMap . okextendl) [[]]

okextendl :: a -> Partition a -> [Partition a]
okextendl x = filter (ok . head) . extendl x

ok :: Segment a -> Bool
ok = undefined

---

partsR2 :: [a] -> [Partition a]
partsR2 = foldr step [[]]
  where
    step :: a -> [Partition a] -> [Partition a]
    step x ps = map (cons x) ps ++ concatMap cglue ps
      where cglue [] = []
            cglue p  = [glue x p]

partsR2A :: [a] -> [Partition a]
partsR2A = foldr step [[]]
  where step x [[]] = [[[x]]]
        step x ps = map (cons x) ps ++ map (glue x) ps
