
Exercise 8.1

Consider the recurrence H(1) = 0 and H(n) = 1+H(⌈n/2⌉).
Prove by induction that H(n) = ⌈log n⌉.

漸化式 H(1) = 0 と H(n) = 1+H(⌈n/2⌉) を考える。
帰納法で H(n) = ⌈log n⌉ であることを証明せよ。


Exercise 8.2

Prove that the bottom-up algorithm

8.1節の次のボトムアップのアルゴリズムが

  mktree = unwrap . until single (pairWith Node) . map Leaf

of Section 8.1 produces a tree of minimum height.

最小の高さの木を生成することを証明せよ。


Exercise 8.3

We claimed in Section 8.1 that minimising lcost also minimises cost.  Why is this true?

8.1節で lcost を最小化することが cost を最小化することにもなると主張した。これはなぜ正しいか?


lcost t1 < lcost t2 ⟹ cost t1 <= cost t2 を示す

lcost t1 < lcost t2
⟹ { List の <= の定義 }
head (lcost t1) <= head (lcost t2)
⟹ { lcost の性質 head (lcost t) == cost t }
cost t1 <= cost t2



Exercise 8.4

Why is the claim rollup . spine = id not true for all possible lists of trees?

rollup . spine = id という主張が、ありうるすべての木のリストについて正しくないのはなぜか?


わからなかったので答えを見た

無限の spine を持つような Tree に対して spine は ⊥ を返す。
すると rollup の結果が ⊥ となるので等しくならない。

> infT :: Tree Nat
> infT = Node infT (Leaf 1)



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

[x] のとき

   { 仮定 g2 x <- M (g1 x) }
g2 x <- M (g1 x)
⟺ { foldrn の定義 }
foldrn f2 g2 [x] <- M (g1 x)
⟺ { foldrn の定義 }
foldrn f2 g2 [x] <- M (foldrn f1 g1 [x])

x:ys のとき

   { 帰納法の仮定 }
foldrn f2 g2 ys <- M (foldrn f1 g1 ys)
⟹ { f2 x 適用 }
f2 x (foldrn f2 g2 ys) <- f2 x (M (foldrn f1 g1 ys))
⟹ { 仮定 ∀ y . f2 x (M y) <- M (f1 x y), <- の 推移律  }
f2 x (foldrn f2 g2 ys) <- M (f1 x (foldrn f1 g1 ys))
⟺ { foldrn の定義 }
foldrn f2 g2 (x:ys) <- M (foldrn f1 g1 (x:ys))

証明終わり


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
> splitsn [x,y] = [([x],[y])]
> splitsn (x:xs) = ([x], xs) : [ (x:ys, zs) | (ys, zs) <- splitsn xs ]


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

これら値はカタラン数と呼ばれる。

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

-----

> type Nat = Int

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
