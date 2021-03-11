{- p.177- }

8章 - Greedy altorithms on trees - 木における貪欲アルゴリズム
=====

The next two problems are about trees, so the greedy algorithms take place in a  wood rather than on a hillside.
The problems concern the task of building a tree  with minimum cost, for two different definitions of cost.
The first problem is closely  related to the tree-building algorithms we have seen before in binary search and  sorting.
The second problem, Huffman coding trees, is of practical importance in  compressing data effectively.
Unlike the problems in the previous chapter, the two  greedy tree-building algorithms require us to reason about the nondeterministic  function MinWith in order to prove that they work.

次の2つの問題は木に関する問題なので貪欲なアルゴリズムは丘の中腹ではなく木の中で行われる。
これらの問題は2つの異なるコストの定義について最小のコストで木を構築するという課題に関係している。
最初の問題は以前にバイナリ検索やソートで見た木を構築するアルゴリズムと密接に関連している。
2つ目の問題であるハフマン符号化木はデータを効果的に圧縮する上で実用的に重要な問題である。
前章の問題とは異なり2つの貪欲な木の構築アルゴリズムはそれらが機能することを証明するために非決定論的な関数MinWithについて推論する必要がある。

8.1 Minimum-height trees - 高さが最小の木
-----

Throughout the chapter we fix attention on one type of tree, called a leaf-labelled tree:

この章ではリーフラベル付き木と呼ばれる木の一種に注目している:

> data Tree a = Leaf a | Node (Tree a) (Tree a)
>             deriving Show

A leaf-labelled tree is therefore a binary tree with information stored only at the leaves.
Essentially this species of tree, though with an additional constructor Null,  was described in Section 5.2 on Mergesort.

The size of a leaf-labelled tree is the number of its leaves:

リーフラベル付き木は葉のみに情報が格納されたバイナリ木である。
基本的にこの種の木はコンストラクタ `Null` を追加していますがMergesort の項 5.2 で説明した。

リーフラベル付き木のサイズはその葉の数となる:

> size :: Tree a -> Nat
> size (Leaf x) = 1
> size (Node u v) = size u + size v

The height of a tree is defined by

木の高さは次で定義される

> height (Leaf x) = 0
> height (Node u v) = 1 + height u `max` height v


With a leaf-labelled tree of size n and height h we have the relationship h < n ≤ 2^h, so h ≥ ceiling(log n).

{- p.178 -}

The fringe of a tree is the list of leaf labels in left-to-right order:

サイズがn で高さがh のリーフラベル付き木では
h < n ≤ 2^h の関係が成り立つので h ≥ ceiling(log n) となる。

木のフリンジとは左から右の順に葉のラベルを並べたリストである:

> fringe :: Tree a -> [a]
> fringe (Leaf x) = [x]
> fringe (Node u v) = fringe u ++ fringe v

Thus fringe is essentially the same function that we have previously called flatten.
Note that the fringe of a tree is always a nonempty list.

Consider the problem of building a tree of minimum height with a given list as  fringe.
We have already encountered two ways of solving this problem, both of  which can be implemented to take linear time.
The first solution is the divide-andconquer, or top-down, method of Section 5.2:

したがって fringe は以前に flatten と呼んだものと本質的には同じになる。
木の fringe は常に空ではないリストであることに注意しよう。

与えられたリストを fringe とし最小の高さの木を作る問題を考えてみよう。
この問題を解くための2つの方法をすでに見てきましたがどちらも線形時間をかけて実装することができる。
最初の解決法は第5.2節の分割統治, つまりトップダウンの方法である:

> mktree :: [a] -> Tree a
> mktree [x] = Leaf x
> mktree xs  = Node (mktree ys) (mktree zs)
>   where (ys,zs) = splitAt (length xs `div` 2) xs

This definition does not take linear time, but it is easy to convert it into one that does.
The trick, as we have seen in the treatment of Mergesort in Section 5.2, is to avoid repeated halving by tupling.
Second, we have the bottom-up method, also  described in Section 5.2:

この定義は線形時間を取らないが線形時間を取るものに変換するのは簡単である。
第5.2節のMergesortの扱いで見たようにコツはタプリングを使って, 半減を繰り返すのを避けることにある。
第二にセクション5.2でも説明したボトムアップ方式がある。

> mktree2 :: [a] -> Tree a
> mktree2 = unwrap . until single (pairWith Node) . map Leaf

These two ways of building a tree lead to different trees but both have minimum  height.
To show that this property holds for the first definition of mktree,
let H(n)  denote the height of mktree for an input of length n.
Then H satisfies the recurrence  H(1) = 0 and H(n) = 1+H(n/2) with solution H(n) = ceiling(log n) (see Exercise 8.1),  the minimum height possible.
The reason why the bottom-up method also produces  a minimum-height tree is left as another exercise.

Let us now change the problem slightly: given a nonempty list of natural numbers,  can we find a linear-time algorithm for building a tree with minimum cost and the  given list as fringe, where

これら2つの木の構築方法は異なる木になりますが両方とも最小の高さを持つ。
この性質がmktreeの最初の定義にも当てはまることを示すために長さnの入力に対するmktreeの高さをH(n)とする。
そしてHは , H(1) = 0, H(n) = 1 + H(ceiling(n/2)) の再帰の解 H(n) = log n を満たし(演習8.1を参照), 可能な最小の高さになる。
ボトムアップ法でも最小高さの木が得られる理由は別の問題として残しておく。

ここで問題を少し変えてみよう。
空ではない自然数のリストが与えられるたとき,
与えられたリストをフリンジとするような木を構築するための最小のコストの線形時間アルゴリズムを見つけることができるだろうか。
ここで,

> cost :: Tree Nat -> Nat
> cost (Leaf x) = x
> cost (Node u v) = 1 + cost u `max` cost v

とする。

The function cost has the same definition as height except that the height of a leaf  is the label value rather than 0.
In fact, if each leaf is replaced by a tree whose height  is given by the label value, the problem is really of the following form:
given a list  of trees together with their heights, can we find a linear-time algorithm to combine  them into a single tree of minimum height without changing the shape or order of  the component trees? To appreciate the problem consider the two trees with the  same fringe in which each node is labelled with its cost.

関数 cost は葉の高さが0ではなくラベル値であることを除いて高さと同じ定義を持っている。
実際各葉がラベル値で与えられた高さを持つ木で置き換えられた場合問題は次のような形になる。
木のリストとその高さが与えられた場合構成木の形状や順序を変えることなくそれらを最小の高さの1本の木に結合する線形時間アルゴリズムを見つけることができるだろうか?
{- p.179 -}
この問題を理解するためにそれぞれのノードがそのコストをラベルに持つ同じフリンジを持つ2本の木を考えてみよう。


図 p.179 上

        6
       / \
      /   \
     /     \
    3       5
   / \     / \
  1   2   3   4


        5
       / \
      4   4
     / \
    3   3
   / \
  1   2


The tree on the left has cost 6, but the  tree on the right has minimum cost 5.
It is not obvious how to construct a tree with minimum cost, at least not efficiently, and that is where a greedy algorithm enters  the stage.
We start off with a specification and then calculate the algorithm.
The specification is phrased as one of refinement:

左側の木はコスト6を持つが右側の木は最小コスト5を持つ。
最小コストの木をどのようにして構築するかは明らかではなく少なくとも効率的ではない。
まずは仕様を決めてからアルゴリズムを計算する。

仕様は改善されたもののうちの一つとして表現される:

 mct :: [Nat] -> Tree Nat
 mct xs <- MinWith cost (mktrees xs)

for finite nonempty lists xs, where mktrees xs is a list of all possible trees with fringe  xs.
In words, mct xs is some element of mktrees xs with minimum cost.

The function mktrees can be defined in a number of ways.
We are going to give  two inductive definitions; other possibilities are discussed in the exercises.
The first method is to define

ここで mktrees xs はフリンジ xs を持つ可能性のあるすべての木のリストである。
言い換えれば mct xs は mktrees xs の最小コストを持ついくつかの要素になる。

mktrees関数はいくつかの方法で定義することができる。
ここではつの帰納的な定義を与えますが他の可能性については演習で議論する。
最初の方法は次の定義だ

> mktrees0 :: [a] -> [Tree a]
> mktrees0 [x] = [Leaf x]
> mktrees0 (x:xs) = concatMap (extend x) (mktrees0 xs)

The function extend returns a list of all the ways in which a new element can be  added as a leftmost leaf in a tree:

関数 extend は新しい要素をツリーの左端の葉として追加することができるすべての方法のリストを返す:

> extend :: a -> Tree a -> [Tree a]
> extend x (Leaf y)   = [Node (Leaf x) (Leaf y)]
> extend x (Node u v) = [Node (Leaf x) (Node u v)] ++
>                       [Node u' v | u' <- extend x u]

For example, applying extend x to the tree

例えば次の木に extend x を適用すると

図 p.179 下

    @
   / \
  @   t2
 / \
y   t1

produces the three trees

3つの木を生成する

{- p.180 -}

図 p.180 上

    @
   / \
  x   @
     / \
    @   t2
   / \
  y   t1


      @
     / \
    @   t2
   / \
  x   @
     / \
    y   t1


        @
       / \
      @   t2
     / \
    @   t1
   / \
  x   y


We might have taken mktrees [] = [] and so defined mktrees as an instance of foldr.
But MinWith is not defined on an empty list and we have to restrict the input to  nonempty lists.
The Haskell standard library does not provide a sufficiently general  fold function for nonempty lists (the function foldr1 is not quite general enough),  but if we define foldrn by

mktrees [] = [] とし mktrees を foldr のインスタンスとして定義したかもしれない。
しかしMinWithは空リストでは定義されておらず入力を非空リストに限定しなければならない。
Haskell標準ライブラリは空でないリストに対して十分に一般的な畳み込み関数を提供していない。
(関数foldr1は十分に一般的ではない)が foldrn を次のように定義したなら

> foldrn :: (a -> b -> b) -> (a -> b) -> [a] -> b
> foldrn f g [x]    = g x
> foldrn f g (x:xs) = f x (foldrn f g xs)

then the definition of mktrees above can be recast in the form

上の mktrees の定義は次のような形で再構成できる

> mktrees = foldrn (concatMap . extend) (wrap . Leaf)

where wrap converts a value into a singleton list.

The second inductive way of building a tree is to first build a forest, a list of trees:

ここでの wrap は値をシングルトンリストに変換する

木を構成する 2つ目の帰納的な方法は最初に森(木のリスト)を構成することだ:

> type Forest a = [Tree a]

A forest can be rolled up into a tree using

森は木へ '巻き上げる' ことができる

> rollup :: [Tree a] -> Tree a
> rollup = foldl1 Node

The function foldl1 is the Haskell prelude function for folding a nonempty list from  left to right.
For example,

関数 foldl1 は Haskell のプレリュード関数で,空でないリストを左から右へ畳み込む。
たとえば,

rollup [t1, t2, t3, t4] = Node (Node (Node t1 t2) t3) t4

The converse to rollup is the function spine, defined by

rollup の逆は spine 関数で, 次のように定義される

> spine :: Tree a -> [Tree a]
> spine (Leaf x)   = [Leaf x]
> spine (Node u v) = spine u ++ [v]

This function returns the leftmost leaf of a tree,
followed by a list of the right  subtrees along the path from the leftmost leaf of the tree to the root.
Provided the  first tree in a forest ts is a leaf, we have

この関数は根からの最左の葉のパスに沿った右の部分木が続く最左の葉を返す。
森 ts の最初の木は葉になる。
次が成立する

 spine (rollup ts) = ts

We can now define

これで次のような定義が可能だ

> mktreesF :: [a] -> [Tree a]
> mktreesF = map rollup . mkforests

{- p.181 -}

where mkforests builds the forests:

ここで mkforests は森を構築する:

> mkforests :: [a] -> [Forest a]
> mkforests = foldrn (concatMap . extendF) (wrap . wrap . Leaf)

> extendF :: a -> Forest a -> [Forest a]
> extendF x ts = [Leaf x : rollup (take k ts) : drop k ts | k <- [1 .. length ts] ]

The new version of extend is arguably simpler than the previous one.
It works by  rolling up some initial segment of the forest into a tree and adding a new leaf as the  first tree in the new forest.
For example,

新しいバージョンの extend は以前のものよりも間違いなく単純だ。
これは森の初期セグメントを木に巻き上げ新しい森の最初の木として新しいリーフを追加することで動作する。
例えば以下のようになる

extend x [t1, t2, t3] =  [Leaf x, t1, t2, t3]
                         [Leaf x, Node t1 t2, t3]
                         [Leaf x, Node (Node t1 t2) t3]

The two versions of mktrees are not the same function simply because they produce  the trees in a different order.
We will come back to spine and rollup later on.

Let us now return to the first definition of `mktrees`, the one expressed directly as  an instance of `foldrn`.
To fuse the two component functions in the definition of `mct` we can appeal to the fusion law of `foldrn`.
The context-sensitive version of this law  states that

木を生成する順番が違というだけでも, mktreesの2つのバージョンは同じ機能ではない。
spineとrollupについては後ほど説明する。

ここで, `foldrn` のインスタンスとして直接表現された, 最初の `mktrees` の定義に戻ろう。
mct の定義を構成する2つの関数を融合させるために `foldrn` の融合法則を利用することができる。
この法則の文脈依存版は次のようになる

foldrn f2 g2 xs <- M (foldrn f1 g1 xs)

for all finite, nonempty lists xs, provided g2 x <- M (g1 x) and
f2 x (M (foldrn f1 g1 xs)) <- M (f1 x (foldrn f1 g1 xs))

任意の有限の空でないリスト xs に対して g2 x <- M (g1 x) と
f2 x (M (foldrn f1 g1 xs)) <- M (f1 x (foldrn f1 g1 xs))
が与えられる。

For our problem, M = MinWith cost, f1 = concatMap extend, and g1 = wrap . leaf.
Since Leaf x = MinWith cost [Leaf x], we can take g2 = Leaf.
For the second fusion  condition we have to find a function, gstep say, so that

我々の問題では, M = MinWith cost, f1 = concatMap extend, g1 = wrap . Leaf だ。
Leaf x = MinWith cost [Leaf x] なので g2 = Leaf を得る。


  gstep x (MinWith cost (mktrees xs))
      <- MinWith cost (concatMap (extend x) (mktrees xs))

As we saw at the end of the previous chapter, this condition is satisfied if the monotonicity condition

前の章の最後で見たように, この条件は

  cost t ≤ cost t' cost (gstep x t) ≤ cost (gstep x t)

holds for all trees t and t' in mktrees xs.
However, no such function gstep exists to satisfy the monotonicity condition. Consider the two trees t1 and t2:

mktrees xs 内の任意の木 t と t' に対して単調性が成り立つときに満たされます。
しかし, 単調性を満たすそのような関数 gstep は存在しません。
木 t1 と t2 を考えてみます。


図 p.181 下

      10
     /  \
    9    9
   / \
  5   8
     / \
    6   7


       10
       / \
      8   9
     / \
    7   7
   / \
  5   6



{- p.182 -}

which, along with the three trees

3 つの木とともに


図 p.182 上

        11
       / \
      /   \
     /     \
    7       10
   / \     / \
  5   6   7   9


    11
   / \
  5   10
     / \
    8   9
   / \
  6   7


    12
   / \
  5   11
     / \
    6   10
       / \
      7   9


are the five trees that can be built with fringe [5,6,7,9].
The subtrees of each tree have been labelled with their costs, so both t1 and t2 have the minimum possible cost 10.
However, the monotonicity condition

(これらは) fringe [5,6,7,9] から作られる 5つの木だ。
それぞれの木の部分木はそのコストのラベルが付いているので, t1 と t2 はどちらも考えられる最小のコスト 10 を持つ。
しかし, 単調性の条件は

cost t1 ≤ cost t2 ==> cost (gstep x t1) ≤ cost (gstep x t2)

fails for any definition of gstep.
Take, for example, x = 8.
Adding 8 to t1 in the best possible way gives a tree with minimum cost 11, while adding 8 to t2 in the best possible way gives a tree with cost 10.
So there is no way we can define a function gstep for which the fusion condition holds.
Once again we appear to be stuck, even  with a refinement version of fusion.

どのような gstep でも果たされない。
たとえば x = 8 を取るとする。
8 を t1 に加えるのに, 考えうる最良の方法から,最小のコスト 11 の木を得ます。さらに 8 を t2 に加える最良の方法からはコスト 10 の木を得る。
よって融合条件を保持する関数 gstep を定義することができる方法はない。

The only way out of the wood is to change the cost function, and once again lexical ordering comes to the rescue.
Notice that the list of costs [10,8,7,5] reading downwards along the left spine of t2 is lexically less than the costs [10,9,5] along the left spine of t1.
The lexical cost, lcost say, is defined by

この問題をのりこえる唯一の方法はコスト関数を変えることであり, ふたたび字句順序が助けになる。
t2 の左の spine に沿って下向きに読めるコストのリスト [10,8,7,5] は t1 の左の spine に沿ったコスト [10,9,5] よりも字句的に小さいことに注意しよう。
字句順コスト( lcost と呼ぼう ) は次のように定義される

> lcost ::Tree Nat -> [Nat]
> lcost = reverse . scanl1 op . map cost . spine
>   where op x y = 1 + (x `max` y)

The costs of the trees along the left spine are accumulated from left to right by `scanl1 op` and then reversed.
For example, spine t2 has tree costs [5,6,7,9] and accumulation gives the list [5,7,8,10], which, when reversed, gives the lexical cost of t2.
Minimising lcost also minimises cost (why?), so we can revise the second fusion condition to read

左の spine に沿った木のコストは scanl1 op によって左から右に累積され, 反転される。
たとえば, spine t2 は木のコスト [5,6,7,9] を持ち累積はリスト [5,7,8,10 ] で, 反転すると t2 の 字句順コストを与える。
lcost を最小にすると cost も最小になり(なぜでしょう?), 第二の融合条件を次のように修正でる

  gstep x (MinWith lcost (mktrees xs))
    <-- MinWith lcost (concatMap (extend x) (mktrees xs))

This time we can show

今回は次を示すことができる

  lcost t1 ≤ lcost t2 ==> lcost (gstep x t1) ≤ lcost (gstep x t2)

where gstep is specified by

gstep は次で定める

  gstep x ts  <-- MinWith lcost (extend x ts)

To give a constructive definition of gstep and to prove that monotonicity holds, consider the two trees of Figure 8.1 in which t1 is a leaf.
The tree on the left is the result of rolling up the forest [t1,t2,...,tn] into a single tree.
The tree on the right is obtained by adding x as a new leaf after rolling up the first j elements of the forest.

gstep の構成的な定義を与え, 単調性が保たれることを示すために, 図8.1 の t1 が葉となっているような, 2つの木を考えよう。
左側の木は [t1,t2,...,tn] を一つの木へと巻き上げた結果だ。
{- p.183 -}
右側の木は始めの j 要素を forest へと巻き上げたあとに x を新らたな葉として加えることで得られる。


図 p.183 上 - 図 8.1

  左
         c{n}
        /   \
     c{n-1} t{n}
      /  \
     /    \
    c2   t{n-1}
   /  \
  t1  t2

  右
        c'{n}
        /  \
       /    t{n}
    c'{j+1}
     /    \
   c'{j}  t{j+1}
   /  \
  x    c{j}
      /  \
     /    t{j}
    t1

The trees are labelled with cost information, so

木にはコストの情報のラベルが付いている。なので, 2 ≤ k ≤ n に対して次が成り立つ。

  c1 = cost t1
  c{k} = 1 + (c{k-1} `max` cost t{k})

In particular, [c1, c2,...,cn] is strictly increasing.
A similar definition holds for the costs on the right:

とくに, [c1, c2,...cn] は厳密に増加します。
似たような定義が右の木のコストについて, j+1 ≤ k ≤ n 対して成り立つ。

  c'j = 1+ (x `max` c{j})
  c'k = 1+ (c k1 `max` cost t{k})

for j+1 ≤ k ≤ n.
In particular, since adding a new leaf cannot reduce costs, we have c{k} ≤ c'{k} for j ≤ k ≤ n.

とくに, 新たな葉を加えてもコストを減らすことはできないので, j ≤ k ≤ n に対して c{k} ≤ c'{k} だ。


The aim is to define gstep by choosing j to minimise [c'n, c'{n-1},...,c'j, x].
For example, consider the five trees [t1,t2,...,t5] with costs [5,2,4,9,6].
Then

意図としては,  [c'{n}, c'{n-1},...,c'{j}, x] を最小にするように j を選ぶことで gstep を定義することだ。
たとえば, 5つの木 [t1,t2,...,t5] を考えてみよう

  [c1,c2,...,c5] = [5,6,7,10,11]

Take x = 8.
There are five possible ways of adding x to the forest, namely by rolling up j trees for 1 ≤ j ≤ 5.
Here they are, with costs on the left and accumulated costs on the right:

x = 8 としよう。
x を forest に加えるのに 5通りの方法がありえる。つまりは 1 ≤ j ≤ 5 に対する j まで巻き上げだ。
ここで, コストは左, 累積したコストは右となる:


  [8,5,2,4,9,6] --> [8,9,10,11,12,13]
  [8,6,4,9,6] --> [8,9,10,11,12]
  [8,7,9,6] --> [8,9,10,11]
  [8,10,6] --> [8,11,12]
  [8,11] --> [8,12]

The forest which minimises lcost is the third one, whose lexical cost is the reverse of [8,9,10,11].

lcost を最小にする forest は 3番目のものだ。その字句順コストは [8,9,10,11] の反転である。

We claim that the best choice of j is the smallest value in the range 1 ≤ j < n, if it  exists, such that

ここで, 最も良い j の選択は範囲 1 ≤ j < n において次のような最小の値であると主張しよう

  1+(x `max` c{j}) < c{j+1}   (8.1)

{-
  (8.1) の導出

  c{j+1} = 1 + (c{j} `max` cost t{j+1})  -- x を挿入する前の木
    {- ここから cost t{j+1} < c{j+1} -}

  x 挿入後に c'{j+1} が c{j+1} 以下になる(大きくならない)ようにするには

  c'{j} = 1 + (x `max` c{j})
  c'{j+1} = 1 + (c'{j} `max` cont t{j+1}) ≤ c{j+1}

  (c'{j} `max` cont t{j+1}) < c{j+1}
  c'{j} < c{j+1} かつ cont t{j+1} < c{j+1} -- こちらは上でもともと成立
  1 + (x `max` c{j}) < c{j+1}
 -}

{- p.184 -}

If no such j exists, then choose j = n. For example, with

そのような j が無ければ, j = n を選択する。例えば

  [c1,c2,c3,c4,c5] = [5,6,7,10,11]

and x = 8, the smallest j satisfying (8.1) is j = 3, with the result

で, x = 8 なら, (8.1) を満たす最小の j は j = 3 で, 結果は次のようになる。

  [x,1+(x `max` c3),c4,c5] = [8,9,10,11]

On the other hand, with x = 9 we have j = 5, with the result

一方, x = 9 なら j = 5 で, 結果は次のようになる。

  [x,1+ (x `max` c5)] = [9,12]

To prove (8.1), suppose the claim holds for both j and k, where 1 ≤ j < k < n.
Then, setting c'{j} = 1+(x `max` c{j}) and c'{k} = 1 + (x `max` c{k}), the two sequences

(8.1) を証明するために, 1 ≤ j < k < n において j と k が両方とも主張を満たすことを仮定する。
c'{j} = 1+(x `max` c{j}) ,  c'{k} = 1 + (x `max` c{k}) とすると

  as = [x,c'{j},c{j+1},...,c{k-1},c{k},c{k+1},...,c{n}]
  bs = [x, c'{k},c{k+1},...,c{n}]

are such that `reverse as` < `reverse bs` because c{k} < c'{k}.
Hence, the smaller the value of j, the lower is the cost.

では `reverse as` < `reverse bs` だ。なぜなら c{k} < c'{k} だからである。
よって, より小さい j の値, より小さいものがコストとなる。

To show that gstep x is monotonic with respect to lcost, suppose

`lcost` については `gstep x` が単調であることを示すには, 次を仮定する

  lcost t1 = [c{n},c{n-1},...,c1]
  lcost t2 = [d{m},d{m-1},...,d1]

where lcost t1 ≤ lcost t2.
If these costs are equal, then so are the costs of adding a new leaf to either tree.
Otherwise, if lcost t1 < lcost t2 and we remove the common prefix, say one of length k, then we are left with two trees t'1 and t'2 with

ここで lcost t1 ≤ lcost t2 だ。
これらのコストが等しいなら, 新たな葉をどちらかの木に加える。
そうでなく, lcost t1 < lcost t2 なら共通の接頭辞を削除し, (この長さを k とする) 2つの木 t'1 と t'2 が残る。

  lcost t'1 = [c{p},...,c1]
  lcost t'2 = [d{q},...,d1]

where p = n-k, q = m-k and c{p} < d{q}.
It is sufficient to show that

ここで p = n-k, q = m-k かつ c{p} < d{q} である。
これは次を示すのには十分だ

  lcost (gstep x t'1) ≤ lcost (gstep x t'2)

Firstly, suppose (8.1) holds for t'1 and j < p. Then
まず t'1 と j < p について (8.1) を仮定する。すると

  lcost (gstep x t'1) = [c{p},...,c{j+1},1+(x `max` c{j}),x]

But c{p} < d{q}, and since `gstep x t'2` can only increase the cost of t'2, we have in this case that

  lcost (gstep x t'1) < lcost t'2 ≤ lcost (gstep x t'2)

In the second case, suppose (8.1) does not hold for t'1. In this case

  lcost (gstep x t'1)=[1+ (x `max` cp),x]

Now, either 1+ (x `max` cp) < dq, in which case

  lcost (gstep x t'1) < lcost t'2 ≤ lcost (gstep x t'2)

or 1+(x `max` c{p}) ≥ d{q}, in which case x ≥ d{q} - 1 and 1+(x `max` d{q-1}) ≥ d{q}.
That means that (8.1) does not hold for t'2 either, and so we have

{-
    x ≥ d{q} - 1 から 1+(x `max` d{q-1}) ≥ d{q} を示す

    {- d{q} の定義 -}
    d{q} = 1 + (d{q-1} `max` cost t{q})

    x ≥ d{q} - 1
      ⟺ {- 元の式を残す, d{q} の定義 -}
    x ≥ d{q} - 1 ⋀ x ≥ d{q-1} `max` cost t{q}
      ⟹ {- a `max` b ≥ a  |  a <- d{q-1}, b <- cost t{q} -}
    x ≥ d{q} - 1 ⋀ x ≥ d{q-1} `max` cost t{q} ≥ d{q-1}
      ⟹ {- 推移律 ≥  -}
    x ≥ d{q} - 1 ⋀ x ≥ d{q-1}
      ⟹ {- a ≥ b から a = a `max` b | a <- x, b <- d{q-1} -}
    x `max` d{q-1} ≥ d{q} - 1
      ⟺ {- 両辺 +1 -}
    1+(x `max` d{q-1}) ≥ d{q}
-}

{- p.185 -}

  lcost (gstep x t'1) = [1+ (x `max` c{p}),x]
                      ≤ [1+ (x `max` d{q}),x] = lcost (gstep x t'2)

That completes the proof of monotonicity.

The next task is to implement gtep.
We can rewrite (8.1) by arguing

       1+ (x `max` c{j}) < c{j+1}
  ⟺  1+ (x `max` c{j}) < 1+ (c{j} `max` cost t{j+1})
  ⟺  (x `max` c{j}) < cost t{j+1}

{-
   c{j} `max` cost t{j+1} = c{j} とすると
   c{j} ≤ (x `max` c{j}) < c{j} となり矛盾するので,
   c{j} `max` cost t{j+1} = cost t{j+1} の場合のみありえる
-}

Hence mct = foldrn gstep Leaf, where

  gstep :: Nat -> Tree Nat -> Tree Nat
  gstep x = rollup . add x . spine

where add is defined by

  add x ts = Leaf x : join x ts
  join x [u] = [u]
  join x (u:v:ts) = if x `max` cost u < cost v
                    then u:v:ts else join x (Node u v : ts)

{-
   join がやっていること
   x や後続の木のコストと同じくらいのコストになるまで木をまとめる
 -}

However, instead of computing spines at each step and then rolling up the spine again,
we can roll up the forest at the end of the computation.
What is wanted for  this step are functions hstep and g for which

  foldrn gstep Leaf = rollup . foldrn hstep g

We can discover hstep and g by appealing to the fusion law for foldrn.
Notice that here we are applying the fusion law for foldrn in the anti-fusion,
or fission direction, splitting a fold into two parts.

Firstly, we require rollup . g = Leaf.
Since rollup [Leaf x] = Leaf x, we can define g by g = wrap . Leaf.
Secondly, we want

  rollup (hstep x ts) = gstep x (rollup ts)

for all x and all ts of the form ts = foldrn hstep g xs. Now,

    gstep x (rollup ts)
  = {definition of gstep}
    rollup (add x (spine (rollup ts)))
  = {provided the first element of ts is a leaf}
    rollup (add x ts)

Hence we can take hstep = add, provided the first element of ts is a leaf. But  ts = foldrn add (wrap . Leaf) xs for some xs and it is immediate from the definition  of add that the first element of ts is indeed a leaf.

We now have mct = rollup  foldrn add (wrap . Leaf). As a final step, repeated  evaluations of cost can be eliminated by pairing each tree in the forest with its cost.  That leads to the final algorithm

{- p.186 -}

> type Pair = (Tree Nat,Nat)
> mct :: [Nat] ->  Tree Nat
> mct = rollup . map fst . foldrn hstep (wrap . leaf)
> hstep :: Nat ->  [Pair] ->  [Pair]
> hstep x ts = leaf x :join x ts
> join :: Nat -> [Pair] -> [Pair]
> join x [u] =[u]
> join x (u:v:ts) = if x `max` snd u < snd v
>                     then u:v:ts else join x (node u v:ts)

The functions leaf and node are the smart constructors

> leaf :: Nat -> Pair
> leaf x = (Leaf x,x)
> node :: Pair ->  Pair ->  Pair
> node (u,c) (v,d) = (Node u v, 1 + c `max` d)

For example, the greedy algorithm applied to the list [5,3,1,4,2] produces the  forests

  [Leaf 2]
  [Leaf 4,Leaf 2]
  [Leaf 1,Node (Leaf 4) (Leaf 2)]
  [Leaf 3,Leaf 1,Node (Leaf 4) (Leaf 2)]
  [Leaf 5,Node (Node (Leaf 3) (Leaf 1)) (Node (Leaf 4) (Leaf 2))]

The final forest is then rolled up into the final tree

   /\
  5  \
     /\
    /  \
   /    \
  /\    /\
 3  1  4  2

with cost 7.

It remains to estimate the running time of mct. The critical measure is the number  of calls to join. We can prove by induction that any sequence of hstep operations  applied to a list of length n and returning a forest of length m involves at most  2n - m calls to join. The base case, n = 1 and m = 1, is obvious. For the induction  step, note that join applied to a list of length m and returning a list of length m is  called m'-m times. Thus, using the induction step that hstep applied to a list of  length n - 1 and returning a forest of length m involves at most 2(n-1)-m' calls of join, we have hstep applied to a list of length n, and returning a forest of length  m involves at most

{- p.187 -}

  (2 (n - 1) - m')  + 1 + (m' - m) ≤ 2n - m

calls of join, establishing the induction. Hence the algorithm takes linear time.

Before leaving the problem of building a minimum-cost tree, we make one final  remark. Observe that, when the input is a list consisting entirely of zeros, building  a minimum-cost tree means building a minimum-height tree. It follows that the  greedy algorithm, with minor changes, also works when the cost is the height of the  tree. The changes are left as an exercise.


8.2 Huffman coding trees
-----

Our second example is Huffman coding trees. As older computer users know only  too well, it is often necessary to store files of information as compactly as possible.  Suppose the information to be stored is a text consisting of a sequence of characters.  Haskell uses Unicode internally for its Char data type, but the standard text I/O  functions assume that texts are sequences of 8-bit characters, so a text of n characters  contains 8n bits of information. Each character is represented by a fixed-length  code, so the characters of a text can be recovered by decoding each successive group  of eight bits.

One idea for reducing the total number of bits required to code a text is to abandon  the notion of fixed-length codes, and seek instead a coding scheme based on the  relative frequency of occurrence of the characters in the text. The basic idea is to  take a sample piece of text, estimate the number of times each character appears,  and choose short codes for the more frequent characters and longer codes for the  rarer ones. For example, if we take the codes

  't' -->  0
  'e' -->  10
  'x' -->  11

then text can be coded as the bit sequence 010110 of length 6. However, it is  important that codes are chosen in such a way as to ensure that the coded text can  be deciphered uniquely. To illustrate, suppose the codes had been

  't' -->  0
  'e' -->  10
  'x' -->  1

Under this scheme, text would be coded as the sequence 01010 of length 5.  However, the string tee would also be coded by 01010. Obviously this is not what is wanted.

{- p.188 -}

The simplest way to prevent the problem arising is to choose codes so  that no code is a proper prefix of any other  a prefix-free code.

As well as requiring unique decipherability, we also want the coding to be optimal.  An optimal coding scheme is one that minimises the expected length of the coded  text. More precisely, if characters c{j}, for 1 ≤ j ≤ n, have frequencies of occurrence  pj, then we want to choose codes with lengths lj such that

Σ_{j=1}^{n} pj・lj

is as small as possible.

One method for constructing an optimal code satisfying the prefix property is  called Huffman coding. Each character is stored in a leaf of a binary tree, the  structure of which is determined by the computed frequencies. The code for a  character c is the sequence of binary values describing the path in the tree to the  leaf containing c. For instance, with the tree

Node (Node (Leaf 'b') (Leaf 'e')) (Leaf 't')

the character b is coded by 00, the character e by 01, and the character t by 1.  Clearly, such a scheme yields a prefix-free code.

There are four aspects to the problem of implementing Huffman coding: (i) collecting information from a sample; (ii) building a binary tree; (iii) coding a text; and  (iv) decoding a bit sequence. We deal only with the problem of building a tree.  So, having analysed the sample, suppose we are given a list of pairs:

[(c1,w1),(c2,w2),...,(cn,wn)]

where for 1 ≤ j ≤ n the c{j} are the characters and the wj are positive integers,  called weights, indicating the frequencies of the characters in the text. The relative  frequency of character c{j} occurring is therefore wj/W, where W = wj. We will  suppose w1 w2  wn, so that the weights are given in ascending order.

In terms of trees, the cost function we want to minimise can be defined in the  following way. By definition, the depth of a leaf is the length of the path from the  root of the tree to the leaf. We can define the list of depths of the leaves in a tree by

> depths :: Tree a -> [Nat]
> depths = from 0
>          where from n (Leaf x) =[n]
>                from n (Node u v) = from (n+1) u++from (n+1) v

Now introduce the types

> type Weight = Nat
> type Elem = (Char,Weight)
> type Cost = Nat


------

> type Nat = Int

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
