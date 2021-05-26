> import Data.Ord (comparing)
> import Data.List (minimumBy)

Exercise 8.1

Consider the recurrence H(1) = 0 and H(n) = 1+H(⌈n/2⌉).
Prove by induction that H(n) = ⌈log n⌉.

漸化式 H(1) = 0 と H(n) = 1 + H(⌈n/2⌉) を考える。
帰納法で H(n) = ⌈log n⌉ であることを証明せよ。


one  : N(1)
even : N(k)   -> N(2 * k)        k ≥ 1
odd  : N(k+1) -> N(2 * k + 1)    k ≥ 1

の構造帰納法で証明する

n = 1 のとき

⌈log n⌉ = 0 で H(1) = 0 が成立。


n = 2 * k のとき

H(2 * k) =
  { H(n)の漸化式 }
1 + H(⌈(2 * k) / 2⌉) =
  { / の定義 }
1 + H(⌈k⌉) =
  { ceiling の定義 }
1 + H(k)  =
  { even の帰納法の仮定: n = k のとき H(k) = ⌈log k⌉ }
1 + ⌈log k⌉ =
  { ceiling の性質: 1 + ⌈x⌉ = ⌈1 + x⌉ }
⌈1 + log k⌉ =
  { log の性質: 1 + log x = log (2 * x) }
⌈log (2 * k)⌉

で成立。


n = 2 * k + 1 のとき

H(2 * k + 1) =
  { H(n)の漸化式 }
1 + H(⌈(2 * k + 1) / 2⌉) =
  { / の分配, / の定義  }
1 + H(⌈k + (1/2)⌉) =
  { ceiling の定義 }
1 + H(k+1) =
  { odd  の帰納法の仮定: n = k + 1 のとき H(k+1) = ⌈log (k+1)⌉ }
1 + ⌈log (k+1)⌉ =
  { ceiling の性質: 1 + ⌈x⌉ = ⌈1 + x⌉ }
⌈1 + log (k+1)⌉ =
  { log の性質: 1 + log x = log (2 * x) }
⌈log (2 * k + 2)⌉ =
  { 補題 }
⌈log (2 * k + 1)⌉


p = ⌈log (2 * k + 1)⌉ とおく (k ≥ 1 から p ≥ 2)

  { ceiling の性質 x ≤ ⌈x⌉ }
log (2 * k + 1) ≤ p
⟺ { 2^ }
2 * k + 1 ≤ 2^p
⟺ { 左辺は奇数で右辺は偶数 }
2 * k + 1 < 2^p
⟺ { +1 }
2 * k + 2 < 2^p + 1
⟺ { 整数の不等式: x < y + 1 ⟹ x ≤ y }
2 * k + 2 ≤ 2^p
⟺ { log }
log (2 * k + 2) ≤ p
⟺ { p は整数, ceiling }
⌈log (2 * k + 2)⌉ ≤ p
⟺ { p の定義 }
⌈log (2 * k + 2)⌉ ≤ ⌈log (2 * k + 1)⌉
⟹ { ceiling の性質 x < y ⟹ ⌈x⌉ ≤ ⌈y⌉ }
⌈log (2 * k + 1)⌉ ≤ ⌈log (2 * k + 2)⌉ ≤ ⌈log (2 * k + 1)⌉
⟹ { x ≤ y ≤ x ⟹ x = y }
⌈log (2 * k + 2)⌉ = ⌈log (2 * k + 1)⌉

---

Exercise 8.2

Prove that the bottom-up algorithm

8.1節の次のボトムアップのアルゴリズムが

  mktree = unwrap . until single (pairWith Node) . map Leaf

of Section 8.1 produces a tree of minimum height.

最小の高さの木を生成することを証明せよ。


Answer 紹介

長さ n のリストについて、ボトムアップアルゴリズムは
左の子が完全バランスの二分木(2^kの葉を持つ, 2^k < n ≤ 2^{k+1})
となるような木 t を生成する。
よって t の高さは k + 1 = ⌈log n⌉ であり、これは最小の高さの木となる。

---

Exercise 8.3

We claimed in Section 8.1 that minimising lcost also minimises cost.  Why is this true?

8.1節で lcost を最小化することが cost を最小化することにもなると主張した。これはなぜ正しいか?


lcost t1 < lcost t2 ⟹ cost t1 <= cost t2 を示す

lcost t1 < lcost t2
⟹ { List の < の定義 }
head (lcost t1) <= head (lcost t2)
⟹ { lcost の性質 head (lcost t) == cost t }
cost t1 <= cost t2

---

Exercise 8.4

Why is the claim rollup . spine = id not true for all possible lists of trees?

rollup . spine = id という主張が、ありうるすべての木のリストについて正しくないのはなぜか?


Answer 紹介

無限の spine を持つような Tree に対して spine は ⊥ を返す。
すると rollup の結果が ⊥ となるので等しくならない。

> infT :: Tree Nat
> infT = Node infT (Leaf 1)

---

Exercise 8.5

The (context-free) fusion rule for foldrn asserts that

foldrn の (context-free) 融合法則はすべての有限リスト xs に対して次を主張する

  foldrn f2 g2 xs <- M (foldrn f1 g1 xs)

for all finite lists xs, provided

ただし、次が成立するときに

  g2 x <- M (g1 x)
  f2 x (M y) <- M (f1 x y)

Prove this result.

これを証明せよ


foldrn f2 g2 xs <- M (foldrn f1 g1 xs) を
非空の有限リスト xs に対する帰納法で示す

xs = [x] のとき

    { 仮定 g2 x <- M (g1 x) }
g2 x <- M (g1 x)
⟺ { foldrn の定義 }
foldrn f2 g2 [x] <- M (foldrn f1 g1 [x])


xs = x:ys のとき

   { 帰納法の仮定 }
foldrn f2 g2 ys <- M (foldrn f1 g1 ys)
⟹ { f2 x 適用 }
f2 x (foldrn f2 g2 ys) <- f2 x (M (foldrn f1 g1 ys))
⟹ { 仮定 ∀ y . f2 x (M y) <- M (f1 x y), <- の 推移律  }
f2 x (foldrn f2 g2 ys) <- M (f1 x (foldrn f1 g1 ys))
⟺ { foldrn の定義 }
foldrn f2 g2 (x:ys) <- M (foldrn f1 g1 (x:ys))

証明終わり

---

Exercise 8.6

Specialise the final greedy algorithm of Section 8.1 as suggested to build a minimum-height tree.

8.1節の最終的な貪欲アルゴリズムを、最小高さ木を生成するように特殊化せよ。


> type Pair = (Tree Nat, Nat)

> mht :: [()] -> Tree Nat
> mht = rollup . map fst . foldrn hstep (wrap . const leaf)

> hstep :: a -> [Pair] ->  [Pair]
> hstep _ ts = leaf : join ts

> join :: [Pair] -> [Pair]
> join [u] = [u]
> join (u:v:ts) = if snd u < snd v
>                 then u:v:ts else join (node u v:ts)

> leaf :: Pair
> leaf = (Leaf 0,0)
> node :: Pair ->  Pair ->  Pair
> node (u,c) (v,d) = (Node u v, 1 + c `max` d)

---

Exercise 8.7

The function splits :: [a] -> [([a],[a])] splits a list xs into all pairs of lists (ys,zs) such that xs = ys++zs.
The function splitsn is similar, except that it splits a list into pairs of nonempty lists.
Give recursive definitions of splits and splitsn.

関数 splits はリスト xs を xs = ys++zs となるようなすべてのリストのペア (ys,zs) へと分割する。
関数 splitsn は、空でないリストのペアへとリストを分割することを除いて、(splitsに) 似ている。
splits と splitsn の再帰的な定義を与えよ。

> splits :: [a] -> [([a], [a])]
> splits [] = [([], [])]
> splits (x:xs) = ([], x:xs) : [ (x:ys, zs) | (ys, zs) <- splits xs ]

> splitsn :: [a] -> [([a], [a])]
> splitsn []    = []
> splitsn [x]   = []
> splitsn (x:xs) = ([x], xs) : [ (x:ys, zs) | (ys, zs) <- splitsn xs ]

---

Exercise 8.8

Using splitsn, give a recursive definition of the function mktrees of Section 8.1.
Write down a recurrence relation for the function T(n) that counts the number of trees with n leaves.
It can be shown that

splitsn を使って、8.1節の mktrees の再帰的な定義を与えよ。
n 枚の葉を持つ木の数を数える関数 T(n) の漸進的関係を書き下せ。
これは次のようになるだろう

           1     2n-2
  T(n) =  ---  (      )
           n      n-1

These values are called the Catalan numbers.

これらの値はカタラン数と呼ばれる。

> mktrees :: [a] -> [Tree a]
> mktrees [x] = [Leaf x]
> mktrees xs =
>   [ Node t1 t2
>   | (ys, zs) <- splitsn xs
>   , t1 <- mktrees ys
>   , t2 <- mktrees zs
>   ]

T(1) = 1

T(n) = Σ_{k=1}^{n-1} T(k) * T(n-k)

T(2) = 1 * 1 = 1
T(3) = T(1) * T(2) + T(2) * T(1) = 2
T(4) = T(1) * T(3) + T(2) * T(2) + T(3) * T(1) = 5
T(5) = T(1) * T(4) + T(2) * T(3) + T(3) * T(2) + T(4) * T(1) = 5 + 2 + 2 + 5 = 14


----- ここから下 Huffman 関連 -----


Exercise 8.9

Here is another way of defining the function mktrees of Section 8.1, one similar to that used in Huffman coding:

以下は 8.1節の関数 mktrees を定義するもう一つの別の方法で、これはハフマン符号化で使われたものに似ている:

   mktrees :: [a] -> [Tree a]
   mktrees = map unwrap . until (all single) (concatMap combine) .
             wrap . map Leaf
   combine :: Forest a -> [Forest a]
   combine xs = [ys ++ [Node x y] ++ zs | (ys,x:y:zs) <- splits xs]

The function combine combines two adjacent trees in a forest in all possible ways.
The process is repeated until only singleton forests remain,
forests that consist of just one tree.
Finally the trees are extracted to give a list of trees.
This method may generate the same tree more than once,
but all possible trees are nevertheless produced.
Write down the associated greedy algorithm for this version of mktrees (no justification is required).

関数 combine は forest の中の、二つの隣接した木を可能なすべての方法で結合する。
この処理は singleton の forest (ただ一つだけの木を含む forest) が残るまで繰り返される。
最後に木々は木のリストへと展開される。
この方法は同じ木を一度より多く生成するかもしれないが、
それでもすべての可能なかぎりの木々が生成される。
このバージョンの mktrees に対して連想される貪欲アルゴリズムを書き下せ(根拠付けは必要なし)


解答の紹介

> mct :: [Nat] -> Tree Nat
> mct = unwrap . until single combine . map Leaf

> combine :: Forest Nat -> Forest Nat
> combine ts = us ++ [Node u v] ++ vs
>   where (us,u:v:vs) = bestjoin ts

bestjoin は 2つ目の forest の
最初の 2つの木の combine 後のコストが最小になるように
forest を 2つに分ける関数

[Leaf 5, Leaf 3, Leaf 1, Leaf 4, Leaf 2, Leaf 2]

[Leaf 5, Leaf 3, Leaf 1, Leaf 4, Node (Leaf 2) (Leaf 2)]

[Leaf 5, Node (Leaf 3) (Leaf 1), Leaf 4, Node (Leaf 2) (Leaf 2)]

[Leaf 5, Node (Node (Leaf 3) (Leaf 1)) (Leaf 4), Node (Leaf 2) (Leaf 2)]

[Node (Leaf 5) (Node (Node (Leaf 3) (Leaf 1)) (Leaf 4)), Node (Leaf 2) (Leaf 2)]

[Node (Node (Leaf 5) (Node (Node (Leaf 3) (Leaf 1)) (Leaf 4))) (Node (Leaf 2) (Leaf 2))]

> bestjoin :: Forest Nat -> (Forest Nat, Forest Nat)
> bestjoin ts =
>     fst $ minimumBy (comparing snd) $
>     [ ((xs, ys), cost (Node u v))
>     | (xs, ys@(u:v:vs)) <- splits ts ]
>   where
>     cost :: Tree Nat -> Nat
>     cost (Leaf x) = x
>     cost (Node u v) = 1 + cost u `max` cost v


---

Exercise 8.10

In Huffman coding, why does the second, recursive definition of cost follow from the first?

ハフマン符号化において、 cost の再帰的定義の二つ目が一つ目に従うのはなぜか?

```
  cost ::Tree Elem -> Cost
  cost t = sum [w * d | ((_,w),d) <- zip (fringe t) (depths t)]
```

```
  cost (Leaf e) = 0
  cost (Node u v) = cost u + cost v + weight u + weight v
  weight :: Tree Elem -> Nat
  weight (Leaf (c,w)) = w
  weight (Node u v) = weight u + weight v
```

    fringe (Node u v)
 == fringe u ++ fringe v

    depths (Node u v)
 == from 0 (Node u v)
 == from 1 u ++ from 1 v

    from 1 u
 == [x + 1 | x <- from 0 u]
 == [x + 1 | x <- depths u]


    cost (Node u v)

 == sum [w * d | ((_,w),d) <- zip (fringe (Node u v)) (depths (Node u v))]

 == sum [w * d | ((_,w),d) <- zip (fringe u ++ fringe v) (from 1 u ++ from 1 v)]

 == sum [w * d | ((_,w),d) <- zip (fringe u) (from 1 u) ++ zip (fringe v) (from 1 v)]

 == sum ([w * d | ((_,w),d) <- zip (fringe u) (from 1 u)] ++ [w * d | ((_,w),d) <- zip (fringe v) (from 1 v)])

 == sum [w * d | ((_,w),d) <- zip (fringe u) (from 1 u)] + sum [w * d | ((_,w),d) <- zip (fringe v) (from 1 v)]

 == sum [w * (x + 1) | ((_,w),x) <- zip (fringe u) (depths u)] + sum [w * (x + 1) | ((_,w),x) <- zip (fringe v) (depths v)]

 == sum [w * x + w | ((_,w),x) <- zip (fringe u) (depths u)] + sum [w * x + w | ((_,w),x) <- zip (fringe v) (depths v)]

 == sum [w * x | ((_,w),x) <- zip (fringe u) (depths u)] + sum [w | ((_,w),_) <- zip (fringe u) (depths u)]
  + sum [w * x | ((_,w),x) <- zip (fringe v) (depths v)] + sum [w | ((_,w),_) <- zip (fringe v) (depths v)]

 == cost u + weight u + cost v + weight v

---

Exercise 8.11

Define the function insert used in Huffmans algorithm.

ハフマンアルゴリズムで利用されている関数 insert を定義せよ。



> insert :: Tree Elem -> Forest Elem -> Forest Elem
> insert s = ins where
>   w = weight s
>   ins []             =  [s]
>   ins tts@(t:ts)
>     | w <= weight t  =  s : tts
>     | otherwise      =  t : ins ts

> weight :: Tree Elem -> Nat
> weight (Leaf (c,w)) = w
> weight (Node u v) = weight u + weight v


---

Exercise 8.12

Give the two ways that the tree

次の木

  [Node (Node (Leaf 3) (Leaf 8)) (Node (Leaf 5) (Leaf 9))]

can be generated from [Leaf 3,Leaf 5,Leaf 8,Leaf 9].

が [Leaf 3,Leaf 5,Leaf 8,Leaf 9] から生成される方法を二つ与えよ。



  [Leaf 3,Leaf 5,Leaf 8,Leaf 9]
  [Leaf 5,Leaf 9,Node (Leaf 3) (Leaf 8)]
  [Node (Leaf 3) (Leaf 8),Node (Leaf 5) (Leaf 9)]
  [Node (Node (Leaf 3) (Leaf 8)) (Node (Leaf 5) (Leaf 9))]

  [Leaf 3,Leaf 5,Leaf 8,Leaf 9]
  [Leaf 3,Leaf 8,Node (Leaf 5) (Leaf 9)]
  [Node (Leaf 3) (Leaf 8), Node (Leaf 5) (Leaf 9)]
  [Node (Node (Leaf 3) (Leaf 8)) (Node (Leaf 5) (Leaf 9))]


---

Exercise 8.13

The number of trees generated in the specification of Huffmans algorithm is given for n ≥ 2 by

ハフマンアルゴリズムの仕様で生成される木の数は n ≥ 2 に対して次で与えられる

    n    n-1        2
  (   ) (   ) ... (   )
    2     2         2

Show that this number equals

この数が次に等しいことを示せ

   n!(n-1)!
  ----------
   2^{n-1}



自然数 n ≥ 2 についての帰納法で示す

n = 2 のとき、

   2
 (   ) = 1
   2

  2! 1!
 ------ = 1
   2^1

で成立。


n = k+1 のとき

    k    k-1        2      k!(k-1)!
  (   ) (   ) ... (   ) = ----------
    2     2         2      2^{k-1}

が成立しているとする。

このとき、

    k+1    k    k-1        2
   (   ) (   ) (   ) ... (   )
     2     2     2         2

    k+1     k!(k-1)!
 = (   ) · ----------
     2      2^{k-1}

   (k+1)k    k!(k-1)!
 = ------ · ----------
     2       2^{k-1}

    (k+1)!k!
 = ----------
      2^k

となり成立。 //


---

Exercise 8.14

Define MCC k xs = MinWith cost (apply k fstep [xs]). Show that

MCC k xs = MinWith cost (apply k fstep [xs]) とおく。

MCC k (gstep xs) <- MCC (k+1) xs という条件のもと、次を示せ

  apply k gstep xs <- MCC k xs

provided MCC k (gstep xs) <- MCC (k+1) xs.



自然数 k についての帰納法で証明する。

k = 0 のとき

    apply 0 gstep xs
    {- apply の定義 -}
 == xs
    {- singleton 集合 -}
 <- MinWith cost [xs]
    {- apply の定義 -}
 == MinWith cost (apply 0 fstep [xs])
    {- MCC の定義 -}
 == MCC 0 xs

で成立。

k = n+1 のとき

    apply (n+1) gstep xs
    {- apply の最内ステップを展開 -}
 == apply n gstep (gstep xs)
    {- induction hypothesis -}
 <- MCC n (gstep xs)
    {- 条件 -}
 <- MCC (n+1) xs

となり、成立。 //


---

Exercise 8.15

Define the function singleSL :: SymList a -> Bool for determining whether a symmetric list is a singleton.

symmetric list が singleton かどうかを判定する関数 singleSL :: SymList a -> Bool を定義せよ。



> singleSL :: SymList a -> Bool
> singleSL ([_],[]) = True
> singleSL ([],[_]) = True
> singleSL  _       = False

---

Exercise 8.16

Define addListQ in terms of insertQ.



> addListQ :: Ord p => [(a,p)] -> PQ a p -> PQ a p
> addListQ ps q = foldr (uncurry insertQ) q ps

---

Exercise 8.17

Define mergeOn.



> mergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
> mergeOn f = disp
>   where
>     disp []        ys   = ys
>     disp xs@(_:_)  []   = xs
>     disp xxs@(x:xs) yys@(y:ys)
>       | f x <= f y      = x : disp xs yys
>       | otherwise       = y : disp xxs ys

---

Exercise 8.18

Show that, for the trees considered in Section 8.3, a tree of size n has rank at most ⌊log(n+1)⌋.



PQ の構造帰納法で示す。

Null の場合
サイズは 0、ランクは 0 で 0 = ⌊log (0 + 1)⌋ なので成立

Fork の場合
t1 のサイズを n1 ランク r1 とし、t2 のサイズを n2 ランクを r2 とする。
帰納法の仮定から r1 ≤ ⌊log n1⌋ ⋀ r2 ≤ ⌊log n2⌋

t1 と t2 を fork に渡すと、
サイズは n1 + n2 + 1
ランクは min r1 r2 + 1

   min r1 r2 + 1
   {- induction hypothesis -}
 ≤ min ⌊log n1⌋ ⌊log n2⌋ + 1
   {- min ⌊x⌋ ⌊y⌋ = ⌊min x y⌋ -}
 = ⌊min (log n1) (log n2)⌋ + 1
   {- ⌊x⌋ + 1 = ⌊x + 1⌋ -}
 = ⌊min (log n1) (log n2) + 1⌋
   {- 自然数 x y に対して、 min (log x) (log y) = log (min x y) -}
 = ⌊log (min n1 n2) + 1⌋
   {- log -}
 = ⌊log (2 * min n1 n2)⌋
   {- min n1 n2 + min n1 n2 ≤ n1 + n2 -}
 ≤ ⌊log (n1 + n2)⌋

 ≤ ⌊log ((n1 + n2 + 1) + 1)⌋

となり、t1 と t2 から作った fork でも成立。 //

---

Exercise 8.19

Define emptyQ and nullQ.



> emptyQ :: PQ a p
> emptyQ = Null

> nullQ :: PQ a p -> Bool
> nullQ Null      = True
> nullQ (Fork {}) = False

-----

> type Nat = Int

> type Weight = Nat
> type Elem = (Char,Weight)
> type Cost = Nat

> data Tree a = Leaf a | Node (Tree a) (Tree a)
>             deriving (Eq, Show)

> type Forest a = [Tree a]

> unwrap :: [a] -> a
> unwrap [x] = x

> wrap :: a -> [a]
> wrap x = [x]

> single :: [a] -> Bool
> single [_] = True
> single  _  = False

> pairWith :: (a -> a -> a) -> [a] -> [a]
> pairWith f [] = []
> pairWith f [x] = [x]
> pairWith f (x:y:xs) = f x y : pairWith f xs

> foldrn :: (a -> b -> b) -> (a -> b) -> [a] -> b
> foldrn f g [x]    = g x
> foldrn f g (x:xs) = f x (foldrn f g xs)

> rollup :: [Tree a] -> Tree a
> rollup = foldl1 Node

> spine :: Tree a -> [Tree a]
> spine (Leaf x)   = [Leaf x]
> spine (Node u v) = spine u ++ [v]

> type SymList a = ([a], [a])

> data PQ a p = Null | Fork Rank a p (PQ a p) (PQ a p)
> type Rank = Nat

> toListQ :: Ord p => PQ a p -> [(a,p)]
> toListQ Null = []
> toListQ (Fork _ x p t1 t2) = (x,p):mergeOn snd (toListQ t1) (toListQ t2)

> fork :: a -> p -> PQ a p -> PQ a p -> PQ a p
> fork x p t1 t2
>   | r2 <= r1 = Fork (r2 + 1) x p t1 t2
>   | otherwise = Fork (r1 + 1) x p t2 t1
>   where r1 = rank t1; r2 = rank t2
> rank ::PQ a p -> Rank
> rank Null = 0
> rank (Fork r _ _ _ _) = r

> combineQ :: Ord p => PQ a p -> PQ a p -> PQ a p
> combineQ Null t = t
> combineQ t Null = t
> combineQ (Fork k1 x1 p1 l1 r1) (Fork k2 x2 p2 l2 r2)
>   | p1 <= p2  = fork x1 p1 l1 (combineQ r1 (Fork k2 x2 p2 l2 r2))
>   | otherwise = fork x2 p2 l2 (combineQ (Fork k1 x1 p1 l1 r1) r2)

> insertQ :: Ord p => a -> p -> PQ a p -> PQ a p
> insertQ x p t = combineQ (fork x p Null Null) t
> deleteQ :: Ord p => PQ a p -> ((a,p),PQ a p)
> deleteQ (Fork _ x p t1 t2) = ((x,p), combineQ t1 t2)
