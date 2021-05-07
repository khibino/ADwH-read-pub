
8.2 Huffman coding trees -- ハフマン符号化木
-----

Our second example is Huffman coding trees.
As older computer users know only too well,
it is often necessary to store files of information as compactly as possible.
Suppose the information to be stored is a text consisting of a sequence of characters.
Haskell uses Unicode internally for its Char data type,
but the standard text I/O  functions assume that texts are sequences of 8-bit characters,
so a text of n characters contains 8n bits of information.
Each character is represented by a fixed-length code,
so the characters of a text can be recovered by decoding each successive group of eight bits.

二番目の例はハフマン符号化木だ。
より古くからの計算機の利用者がだけが良く知っているように、
できるかぎり密にファイル群の情報を保存することがしばしば重要だ。
保存された情報が文字の列からなるテキストであると考えてみよう。
Haskell はその Char のデータ型に Unicode を内部的に利用しているが、
標準のテキスト I/O の関数はテキストが 8-bit の文字の列であることを想定しているので、
n 文字のテキストは 8n ビットの情報を含む。
それぞれの文字は固定長のコードで表現されるので、
テキストの文字はそれぞれの連続したの8ビットのグループをデコードすることで復元できる。

One idea for reducing the total number of bits required to code a text is to abandon the notion of fixed-length codes,
and seek instead a coding scheme based on the relative frequency of occurrence of the characters in the text.
The basic idea is to take a sample piece of text, estimate the number of times each character appears,
and choose short codes for the more frequent characters and longer codes for the rarer ones.
For example, if we take the codes

テキストを符号化するのに必要なビットの総数をおさえる一つのアイデアは固定長のコードという考えをやめることだ。
代わりにテキストに現れる文字の相対的な頻度に基づいた符号化の案をさがそう。
基本的なアイデアはテキストの標本としての一部をとり、それぞれの文字の回数を見積り、
より頻度の高い文字には短い符号を選び、より稀な文字にはより長い符号を選ぶことだ。
たとえば、次のコードを採用すると、

  't' -->  0
  'e' -->  10
  'x' -->  11

then text can be coded as the bit sequence 010110 of length 6.
However, it is important that codes are chosen in such a way as to ensure that the coded text can be deciphered uniquely.
To illustrate, suppose the codes had been

テキストは長さ 6 のビット列 010110 に符号化できる。
しかしながら、符号化されたテキストが唯一に解読できることを保証するような方法で符号が選択されることが重要だ。
たとえば、次のような符号を考えよう

  't' -->  0
  'e' -->  10
  'x' -->  1

Under this scheme, text would be coded as the sequence 01010 of length 5.
However, the string tee would also be coded by 01010.
Obviously this is not what is wanted.

この構成では、 text は長さ 5 の列 01010 として符号化されるだろう。
しかしながら、文字列 tee も 01010 と符号化されるだろう。
明らかにこれは望んだものではない。

{- p.188 -}

The simplest way to prevent the problem arising is to choose codes so that
no code is a proper prefix of any other a prefix-free code.

この問題が起こるのを防ぐ最も簡単な方法は、
どんな符号も他の符号の厳密な接頭辞とならないような prefix-free の符号を選ぶことだ。

As well as requiring unique decipherability, we also want the coding to be optimal.
An optimal coding scheme is one that minimises the expected length of the coded text.
More precisely, if characters c_{j}, for 1 ≤ j ≤ n, have frequencies of occurrence p_{j},
then we want to choose codes with lengths l_{j} such that

単一解釈可能性が必要なのと同じくらい、符号化の最適性も求められる。
最適な符号の構成は、符号化されたテキストの予想される長さを最小化するものだ。
より正確には、1 ≤ j ≤ n に対して文字 c_{j} の出現頻度が p_{j} なら、
次のような長さ l_{j} を持ち、

Σ_{j=1}^{n} p_{j}・l_{j}

is as small as possible.

を可能なかぎり小さくする符号を選択したい。

One method for constructing an optimal code satisfying the prefix property is called Huffman coding.
Each character is stored in a leaf of a binary tree, the structure of which is determined by the computed frequencies.
The code for a character c is the sequence of binary values describing the path in the tree to the leaf containing c.
For instance, with the tree

接頭辞の制約を満たす最適な符号を構成する一つの方法はハフマン符号化と呼ばれる。
それぞれの文字は二分木の葉に保存され、その木の構造は計算された頻度によって決まる。
文字 c の符号化は木の中の c を含む葉へのパスを記述するバイナリ値の列だ。
たとえば、次の木では

Node (Node (Leaf 'b') (Leaf 'e')) (Leaf 't')

the character b is coded by 00, the character e by 01, and the character t by 1.
Clearly, such a scheme yields a prefix-free code.

文字 b は 00 と符号化され、文字 e は 01 と、文字 t は 1 となる。
明らかに、このような方法は prefix-free の符号だ。

There are four aspects to the problem of implementing Huffman coding:
(i)   collecting information from a sample;
(ii)  building a binary tree;
(iii) coding a text; and
(iv)  decoding a bit sequence.
We deal only with the problem of building a tree.
So, having analysed the sample, suppose we are given a list of pairs:

ハフマン符号化を実装する問題には4つの面がある:
(i)   標本から情報を集めること;
(ii)  二分木を構築すること;
(iii) テキストを符号化すること; そして
(iv)  ビット列を複合すること;
我々は木を構築する問題だけを扱う。
よって、分析した標本があり、与えられたペアのリストがあると想定する:

[(c_1,w_1),(c_2,w_2),...,(c_n,w_n)]

where for 1 ≤ j ≤ n the c_{j} are the characters and the w_{j} are positive integers,
called weights, indicating the frequencies of the characters in the text.
The relative frequency of character c_{j} occurring is therefore w_{j}/W, where W = Σ w_{j}.
We will suppose w_1 ≤ w_2 ≤...≤ wn, so that the weights are given in ascending order.

ここで 1 ≤ j ≤ n に対して c_{j} は文字で w_{j} は重みと呼ばれる、
テキスト内の文字の頻度を表わす正の整数だ。
よって、文字 c_{j} の出現の相対的な頻度は w_{j}/W となる。ここで  W = Σ w_{j} だ。
w_1 ≤ w_2 ≤...≤ wn を想定する、つまり重みは昇順で与えられる

In terms of trees, the cost function we want to minimise can be defined in the following way.
By definition, the depth of a leaf is the length of the path from the root of the tree to the leaf.
We can define the list of depths of the leaves in a tree by

木の見方から、最小化のために欲しいコスト関数は以下ような方法になる。
定義によれば、葉の深さは木の根から葉までのパスの長さだ。
木の葉の深さのリストを次のように定義できる

> depths :: Tree a -> [Nat]
> depths = from 0
>          where from n (Leaf x) =[n]
>                from n (Node u v) = from (n+1) u++from (n+1) v

Now introduce the types

次の型を導入する

> type Weight = Nat
> type Elem = (Char,Weight)
> type Cost = Nat

{- p.189 -}

and define cost by

そして cost を次で定義する

> cost ::Tree Elem -> Cost
> cost t = sum [w * d | ((_,w),d) <- zip (fringe t) (depths t)]

It is left as an exercise to derive the following alternative definition of cost:

cost の次のような代わりの定義の導出を演習問題として残しておく:

  cost (Leaf e) = 0
  cost (Node u v) = cost u + cost v + weight u + weight v
  weight :: Tree Elem -> Nat
  weight (Leaf (c,w)) = w
  weight (Node u v) = weight u + weight v

We might now follow the previous section and specify

ここでは前節にならって、次のようにする

  huffman :: [Elem] -> Tree Elem
  huffman <- MinWith cost mktrees

where mktrees builds all the trees with a given list as fringe.
But this specification is too strong:
it is not required that the input list be the fringe, only that some permutation of it is.
(However, in Chapter 14 we will consider a version of the problem in which the input is required to be the fringe.)
One way of correcting the definition is to replace mktrees by concatMap mktrees perms.
Another way, and  the one we will pursue, is to design a new version of mktrees.
This version will construct all unordered binary trees.
In an unordered binary tree the two children of a node are regarded as a set of two trees rather than an ordered pair.
Thus Node u v is regarded as the same tree as Node v u.
For example, there are 12 ordered binary trees whose fringe is a permutation of [1,2,3],
two trees for each of the six permutations,
but only three essentially different unordered trees:

ここで mktrees は fringe として与えられたリストでのすべての木を生成する。
しかしこの仕様は強すぎる:
入力のリストが fringe である必要はなく、その順列であることが必要なだけだ。
(しかし、14章ではこの問題の、入力が fringe である必要があるものを考える)
この定義を修正する一つの方法は mktrees を concatMap mktrees perms に置き換えることだ。
このバーションは順序無しのすべての二分木を生成するだろう。
順序無しの二分木ではノードの二つの子が順序の対ではなく二つの木の組とみなされる。
よって、Node u v は Node v u と同じ木と見なされる。
たとえば、fringe が [1,2,3] の順列である順序有りの二分木は 12 あり、
それぞれの順列に対して 2つの木がある。
しかし、順序無しの木で本質的に異なるものは三つだけだ。

{- 対が入れ替わっても同じ木と考えればわかる -}

  Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
  Node (Node (Leaf 1) (Leaf 3)) (Leaf 2)
  Node (Node (Leaf 2) (Leaf 3)) (Leaf 1)

Each tree can be flipped in three ways
(flipping the children of the top tree, the children of the left subtree, or both)
to give the 12 different ordered binary trees.
For Huffman coding it is sufficient to consider unordered trees
because two sibling characters have the same codes except for the last bit and it does not matter which sibling is on the left.
To compute all the unordered Huffman trees we can start with a list of leaves in weight order,
and then repeatedly combine pairs of trees until a single tree remains.
The pairs are chosen in all possible ways and a combined pair can be placed back in the list so as to maintain weight order.
Thus, in an unordered tree Node u v we can assume cost u ≤ cost v without loss of generality.

それぞれの木は 3通りにひっくりかえすことができ、
(頂点の木の子を入れ換える、左の部分木の子を入れ換える、あるいはその両方)
12 の異なる順序有りの二分木を与える。
ハフマン符号化では順序無しの木を考えるのが重要だ。
なぜなら、二つの兄弟の文字は最後のビットを除いて同じ符号を持ち、どちらの兄弟が左にあるかは重要ではないからだ。
すべての順序無しのハフマン木を計算するには、重み順の葉のリストから始め、
一つの木が残るまで繰り返し木の対を結合する。
対は可能なすべての場合で選ばれ、結合された対は重みの順序を維持するようにリストに置くことができる。
このように、順序無しの木 Node u v では、一般性を失なうことなく cost u ≤ cost v を仮定できる。

Here is an example to see the idea at work.
Showing only the weights, consider the following list of four trees in weight order:

このアイデアを動かして見る例だ。
重みだけを見るようにして、重み順の次の四つの木のリストを考える。

{- p.190 -}

  [Leaf 3,Leaf 5,Leaf 8,Leaf 9]

As a first step we can choose to combine the first and third trees (among six possible choices) to give

最初のステップとして次に与えるように、(6つの可能な選択のうち)一番目と三番目の木を結合するのを選ぶことができる

  [Leaf 5,Leaf 9,Node (Leaf 3) (Leaf 8)]

The new tree, with weight 11, is placed last in the list to maintain weight order.
As the next step we can choose to combine the first two trees (among three possible choices),
giving

重み 11 の新たな木は、重み順を維持したリストの最後に置かれる。
次のステップとして、(三つの可能な選択肢から)最初の二つの木を結合することを選ぶ。
すると次が与えられる

  [Node (Leaf 3) (Leaf 8),Node (Leaf 5) (Leaf 9)]

The next step is forced as there are only two trees left, and we end up with a singleton tree

次のステップは、二つの木だけ残っているのでそれ以外にはなく、最後に単一の木になる。

  [Node (Node (Leaf 3) (Leaf 8)) (Node (Leaf 5) (Leaf 9))]

whose fringe is [3,8,5,9].
This bottom-up method for building trees will generate 6 × 3 = 18 trees in total,
more than the total number of unordered trees on four elements,
because some trees, such as the one above, are generated twice (see the exercises).
However, the list of trees includes all that are needed.

この木の fringe は [3,8,5,9] だ。
木を生成するこのボトムアップの方法は、総計 6 × 3 = 18 の木を生成するだろう。
これは四つの要素における順序無しの木の総数より多い。
なぜなら、上のもののような、いくつかの木が二回生成されるからだ。(演習問題を見よ)
しかしながら、必要なものは木のリストにすべて含まれている。

Now for the details. We define

それでは詳細を見ていく。次を定義する

> mktrees :: [Elem] -> [Tree Elem]
> mktrees = map unwrap . mkforests . map Leaf

where mkforests builds the list of forests, each forest consisting of a singleton tree.
On way to define this function uses until:

ここで mkforests は forest のリストを構築し、それぞれの forest は単一の木からなる。
この関数を定義する一つの方法は until を使うことだ:

> mkforests :: [Tree Elem] -> [Forest Elem]
> mkforests = until (all single) (concatMap combine) . wrap

The function mkforests takes a list of trees, turns them into a singleton list of forests by applying wrap,
and then repeatedly combines two trees in every possible way until every forest is reduced to a single tree.
Each singleton forest is then unwrapped to give the final list of trees.
The function combine is defined by

関数 mkforests は木のリストを取り、それらを wrap を適用することで forest の単一のリストへと変える。
そして、繰り返し二つの木を可能なすべての方法で結合し、すべての forest は単一の木へとまとめられる。
それぞれの単一の forest は最終的な木のリストを与えるために unwrap される。

> combine :: Forest Elem  -> [Forest Elem]
> combine ts = [insert (Node t1 t2) us | ((t1,t2),us) <- pairs ts]
> pairs :: [a] -> [((a,a),[a])]
> pairs xs = [((x,y),zs) | (x,ys) <- picks xs, (y,zs) <- picks ys]

The function picks was defined in Chapter 1.
The function insert, whose definition is left as an exercise,
inserts a tree into a list of trees so as to maintain weight order.
Hence combine selects, in all possible ways, a pair of trees from a forest,
combines them into a new tree, and inserts the new tree into the remaining trees.

関数 picks は1章で定義された。
関数 insert の定義は演習問題として残した。
この関数は重さの順を保つように木を木のリストへと挿入する。
よって combine は、可能なすべての方法で、木の対を forest から選び、
木の対を新たな木へと結合し、その新たな木を残りの木々へと挿入する。

Another way to define mkforests uses the function apply.
Recall the answer to  Question 1.13, which gives the following definition of apply:

mkforests を定義する別の方法では、関数 apply を利用する。
演習問題 1.13 の答えを思いだそう。これは apply の次の定義を与える:

{- p.191 -}

> apply :: Nat -> (a -> a) -> a -> a
> apply n f = if n == 0 then id else f . apply (n - 1) f

Thus apply n applies a function n times to a given value.
The alternative definition of mkforests is to write

このように、apply n は与えられた値に関数を n 回適用する。
mkforests の代わりの定義を書くなら次のようになる

> mkforestsA :: [Tree Elem] -> [Forest Elem]
> mkforestsA ts = apply (length ts - 1) (concatMap combine) [ts]

The two definitions give the same result because at each step the number of trees in each forest is reduced by one,
so it takes exactly n-1 steps to reduce an initial forest of n trees to a list of singleton forests.

それぞれのステップで  それぞれの forest 内の木の数が一つに畳み込まれるため、二つの定義は同じ結果を与える。
よって、n 個の木からなる初期の forest を singleton forest のリストへと畳み込むのにちょうど n-1 ステップかかる。

Our problem now takes the form

問題は今や、次の形を取る

  huffman :: [Elem] -> Tree Elem
  huffman <- MinWith cost mktrees

Since mktrees is defined in terms of until, we will aim for a constructive definition of huffman of the same form.
The task is to find a function gstep so that

mktrees は until で定義されるため、同じ形になるような huffman の構成的定義を目標にする。
やるべきことは次のような関数 gstep を、型 [Elem] のすべての有限の空でないリスト xs に対して見付けることだ。

  unwrap (until single gstep (map Leaf xs)) <- MinWith cost (mktrees xs)

for all finite nonempty lists xs of type [Elem].
More generally, we will seek a function gstep such that

より一般的には、次ような関数 gstep をすべての有限の空でない木のリスト ts に対してさがそう。

  unwrap (until single gstep ts) <- MinWith cost (map unwrap (mkforests ts))

for all finite nonempty lists of trees ts.
Problems of this form will arise in the following chapter too,
so let us pause for a little more theory on greedy algorithms.

この形の問題は以降の章にもある。
よって、貪欲アルゴリズムの理論のために小休止しよう。

 ### Another generic greedy algorithm -- もう一つの一般的な貪欲アルゴリズム

Suppose in this section that the list of candidates is given by a function

この節では、ある種の状態から候補のリストが次のような関数で与えられる状況を考えよう

  candidates :: State -> [Candidate]

for some type State.
For Huffman coding, states are lists of trees and candidates are trees:

ハフマン符号化では、状態は木のリストで候補は複数の木だ:

  candidates ts = map unwrap (mkforests ts)

For the problems in the following chapter, states are combinations of values.

以降の章でのこの問題では、状態は値の組み合わせだ。

The aim of this section is to give conditions for which the refinement

この節の目的は、次の精緻化がすべての状態 sx に対して保持される条件を与えることだ

  extract (until final gstep sx) <- MinWith cost (candidates sx)   (8.2)

holds for all states sx.
The functions on the left have the following types:

左辺の関数は次のような型を持つ:

  gstep :: State -> State
  final :: State -> Bool
  extract :: State -> Candidate

{- p.192 -}

In words, (8.2) states that repeatedly applying a greedy step to any initial state sx will result in a final state
from which a candidate x can be extracted with the property that x is a candidate in candidates sx with minimum cost.
In order for the refinement to be meaningful,
it is assumed that the left-hand side returns a well-defined value for any initial state.
Unlike the formulation of a generic greedy algorithm in Section 7.1,
nothing is known about how the candidates are constructed.

言ってみれば、 (8.2) は
任意の初期状態 sx に対して繰り返し貪欲ステップを適用することで、結果として最終状態になり、
その最終状態から、最小コストを持つ candidates sx の中にある候補であるという性質を持つ x を取り出すことができる、ということだ。
精緻化を意味のあるものにするために、
左辺は任意の初期状態に対して一意の値を返すと想定する。
7.1節の一般化された貪欲アルゴリズムの定式化と異なり、
候補がどう構成されるのかについて分かっていることはない。

For brevity in what follows, define

続く説明を簡潔にするために、次のように定義する

  MCC sx = MinWith cost (candidates sx)
  mincost sx = minimum (map cost (candidates sx))

In particular, for all x in candidates sx we have

とくに、candidates sx 内のすべての x に対して、次が成立する

  x <- MCC sx ⟺ cost x = mincost sx

There are two conditions that ensure (8.2).
The first is

(8.2) を保証する二つの条件がある。
一つ目は

  final sx ⟹ extract sx <- MCC sx   (8.3)

This condition holds for Huffman coding,
when final = single and extract = unwrap,
since map unwrap (mkforests [t]) = [t] and MinWith cost [t] = t.

この条件はハフマン符号化を保つ為のもので、
このとき final = single かつ extract = unwrap となる。
なぜなら map unwrap (mkforests [t]) = [t] かつ MinWith cost [t] = t であるからだ。

The second condition is the greedy condition.
We can state it in two ways.
The first way is

二つ目の条件は貪欲条件だ。
それを二通りの方法で述べることができる。
一通り目は

  not (final sx) ⟹ (∃x:x <- MCC (gstep sx) ⋀ x <- MCC sx)   (8.4)

In hillclimbing terms, the greedy condition asserts that,
from any starting point not already on top of the hill,
there is some path to a highest point that starts out with a greedy step.

hill-climbing の言葉で言えば、貪欲条件は次を想定する。
まだ丘のてっぺんでない任意の出発点から、貪欲ステップで始まる最も高い点へのパスがある。

The second way of stating the greedy condition appears to be stronger:

貪欲条件を述べる二通り目の方法は、より強いように見える

  not (final sx) ⟹ MCC (gstep sx) <- MCC sx   (8.5)

However, with one extra proviso, (8.4) implies (8.5).
The proviso is that applying gstep to a state may reduce the number of final candidates but will never introduce new ones.
In symbols,

しかしながら、ひとつ特別な条件があれば、(8.4) は (8.5) を導出する。
その条件とは、gstep を状態に適用することで最終的な候補の数が減り、新たなものが導入されないことだ。

  candidates (gstep sx) ⊆ candidates sx   (8.6)

Suppose x <- MCC (gstep sx) and x <- MCC sx.
Then, by definition of MCC and mincost, we have

x <- MCC (gstep sx) かつ x <- MCC sx とする。
すると、MMC と mincost の定義から、次を得る。

  mincost (gstep sx) = cost x = mincost sx

Now suppose y <- MCC (gstep sx), so y ∈ candidates sx by (8.6).
Then

今や y <- MCC (gstep sx) とするなら (8.6) から y ∈ candidates sx となる。
すると

  cost y = mincost (gstep sx) = mincost sx

and so y <- MCC sx.
Although, in general,
E1 <- E2 is a stronger statement than one that merely asserts there exists some value v such that v <- E1 ⋀ v <- E2,
that is not the case here.

なので y <- MCC sx となる。
一般的には、
単に v <- E1 ⋀ v <- E2 となるような v が存在することを仮定するよりも、E1 <- E2 がより強い主張となる、とはいえ、
ここではその場合にはあたらない。

To prove (8.2), suppose that k is the smallest integer -- assumed to exist --
for which apply k gstep sx is a final state.
That means

(8.2) を証明するには、apply k gstep sx が最終状態であるような
最小の整数 k (存在すると仮定する)を考える。

{- p.193 -}

  until final gstep sx = apply k gstep sx

It follows that apply j gstep sx is not a final state for 0 ≤ j < k, so,
by the stronger greedy condition, we have

0 ≤ j < k について apply j gstep sx は最終状態ではないので、
より強い貪欲条件によって、0 ≤ j < k について次を得る。

  MCC (apply (j+1) gstep sx) <- MCC (apply j gstep sx)

for 0 ≤ j < k.
Hence MCC (apply k gstep sx) <- MCC sx.
Furthermore, by (8.3) we have

よって、MCC (apply k gstep sx) <- MCC sx となる。
さらに、(8.3) から次を得て、

  extract (apply k gstep sx) <- MCC (apply k gstep sx)

establishing (8.2).

(8.2) が成立する。

This style of reasoning about greedy algorithms is very general.
However, unlike greedy algorithms derived by fusion,
it gives no hint as to what form gstep might take.

貪欲アルゴリズムについてのこの形式の証明はとても一般的だ。
しかしながら、融合変換から導出される貪欲アルゴリズムとは違い、
gstep がどんな形をとるのかについてのヒントはなにも無い。

 ### Huffman coding continued -- ハフマン符号化の続き

Returning to Huffman coding,
in which candidates are trees,
it remains to define gstep and to show that the greedy condition holds.
For Huffman coding we have

ハフマン符号化に戻ると、そのときの候補は木で、
gstep を定義することと貪欲条件の保存を示すことが残っている。
ハフマン符号化については次を得る

  MCC ts = MinWith cost (map unwrap (mkforests ts))

We take gstep to be the function that combines the two trees in the forest with smallest weights.
Since trees are kept in weight order, that means

最小の重みを持つ forest 内の二つの木を結合する関数を gstep としてとる。
木は重み順が保たれるので、つまり次を意味する

  gstep (t1 :t2 :ts) = insert (Node t1 t2) ts

For the greedy condition, let ts = [t1,t2,...,tn] be a list of trees in weight order,
with weights [w1,w2,...,wn].
The task is to construct a tree t for which

貪欲条件に対して、重み [w1,w2,...,wn] であるような重み順の木のリスト ts = [t1,t2,...,tn] をとる。
やるべきことは次のような木 t を構築することだ

  t <- MCC (gstep ts) ⋀ t <- MCC ts

Suppose t' <- MCC ts.
We construct t by applying tree surgery to t'.
Every tree in ts appears somewhere as a subtree of t',
so imagine that t{i} appears at depth d{i} in t' for 1 ≤ i ≤ n.
Now, among the subtrees of t',
there will be a pair of sibling trees at greatest depth.
There may be more than one such pair, but there will be at least one.
Suppose two such trees are t{i} and t{j} and let d = d{i} = d{j}.
Then d{1} ≤ d and d{2} ≤ d.
Furthermore, t{i} and t{j} could have been chosen as the first step in the construction of t'.
Without loss of generality, suppose w{1} ≤ w{i} and w{2} ≤ w{j}.
Construct t by swapping t{i} with t{1} and t{j} with t{2}.
Then t can be constructed by taking a greedy first step.
Furthermore

t' <- MCC ts を考える。
木の手法を t' に適用することで t を構成する。
ts 内のすべての木は t' の部分木としてどこかに出現するので、
1 ≤ i ≤ n に対して t{i} が t' 内の深さ d{i} に出現したと考える。
今や、t' の部分木の中では
最大の深さの兄弟の木の対があるだろう。
そのような対は複数あるかもしれないが、少なくとも一つはあるだろう。
そのような 2つの木 t{i} と t{j} を考え、 d = d{i} = d{j} とする。
すると d{1} ≤ d かる d{2} ≤ d である。
さらに、 t{i} と t{j} は t' を構成する最初のステップとして選ぶことができた。
一般性を失うことなく、 w{1} ≤ w{i} かつ w{2} ≤ w{j} とできる。
t{i} と t{1}、t{j} と t{2} を交換することで t を構成する。
すると t は貪欲の最初のステップで構成できる。
さらに次のようになる

  cost t' - cost t = d{1}w{1} + d{2}w{2} + d (w{i} + w{j}) - (d{1}w{i} + d{2}w{j} + d (w{1} + w{2}))
                   = (d - d{1})(w{i} - w{1}) + (d - d{2}) (w{j} - w{2})
                   ≥ 0

But cost t is as small as possible,
so cost t' = cost t.
Hence t <- MCC ts and t <- MCC (gstep ts).

しかし cost t はできるかぎり小さくするので cost t' = cost t である。
よって t <- MCC ts かつ t <- MCC (gstep ts) となる。

{- p.194 -}

The same tree surgery can be used to show that the stronger greedy condition holds by a direct argument.
Suppose t <- MCC (gstep ts) but t is not a value in MCC ts.
That means there exists a tree t' <- MCC ts with cost t' < cost t.
We now get a contradiction by applying the surgical procedure to t'
to produce another tree t'' <- MCC (gstep ts) with cost t = cost t'' ≤ cost t'.
Here is the greedy algorithm we have derived:

直接的により強い貪欲条件が保持されることを示すのに、同様の木の手法が利用できる。
t が MCC ts にある値ではないような t <- MCC (gstep ts) を考える。
これは cost t' < cost t となるような木 t' <- MCC ts が存在することを意味する。
今や cost t = cost t'' ≤ cost t' となるようなもう一つの別の木 tree t'' <- MCC (gstep ts) を生成するために
t' に生成手法を適用することで矛盾が得られる。
ここで導出される貪欲アルゴリズムは次のようになる:

  huffman es = unwrap (until single gstep (map Leaf es))
               where gstep (t1 :t2 :ts) = insert (Node t1 t2) ts

However, simple as it is, the algorithm is not quite ready to leave the kitchen.
There are two sources of inefficiency.
Firstly, the function insert recomputes weights at each step,
an inefficiency that can easily be brushed aside by tupling.
The more serious issue is that,
while finding two trees of smallest weights is a constant-time operation,
inserting the combined tree back into the forest can take linear time in the worst case.
That means the greedy algorithm takes quadratic time in the worst case.
The final step is to show how this can be reduced to linear time.

しかし、シンプルなだけに、このアルゴリズムはまだキッチンから出ることができない。
非効率の原因が二つある。
一つ目は、関数 insert が各ステップで重みを再計算することだが、
非効率性はタプリングによって容易に払拭される。
より重大な問題は、
最小の重みの二つの木を見付けるのは定数時間の操作とはいえ、
結合した木を forest に挿入しなおすのに最悪ケースで線形時間がかかる。
最後のステップはこれを線形時間へと減少させる方法を示す。

The key observation behind the linear-time algorithm is the fact that,
in any call of gstep, the argument to insert has a weight at least as large as any previous argument.
Suppose we combine two trees with weights w1 and w2 and, later on, two trees with weights w3 and w4.
We have w1 ≤ w2 ≤ w3 ≤ w4, and it follows that w1+w2 ≤ w3+w4.
This suggests maintaining the non-leaf trees as a simple queue,
whereby elements are added to the rear of the queue and removed only from the front.
Instead of maintaining a single list we therefore maintain two lists,
the first being a list of leaves and the second a queue of node trees.
Since elements are never added to the first list, but only removed from the front,
the first list could also be a queue.
But a simple list suffices.
We will call the first list a stack simply to distinguish it from the second one.
At each step, gstep selects two lightest trees from either the stack or the queue,
combines them, and adds the result to the end of the queue.
At the end of the algorithm the queue will contain a single tree, the greedy solution.
Figure 8.2, which shows the weights only, gives an example of how the method works out.
The method is viable only if the various queue operations take constant time.
But we have already met symmetric lists in Chapter 3, which satisfy the requirements exactly.

線形時間のアルゴリズムの背景にある観察のポイントは次の事実です、
gstep のどの呼び出しにおいても、insert への引数が少なくとも以前の引数と同じ大きさの重みを持つということだ。
重み w1 と w2 を持つ二つの木を結合し、その後で、重み w3 と w4 を持つ二つの木についても考えよう。
w1 ≤ w2 ≤ w3 ≤ w4 から w1+w2 ≤ w3+w4 が従う。a
これは、要素がキューの後方に加えられ、前方からの削除のみが行なわれるような、
葉の無い木を単純なキューとして管理することを意味する。 (???)
単一のリストを管理する代わりに、二つのリストを管理する。
一つ目は葉のリストで、二つ目はノードの木のキューだ。
要素は一つ目のリストに加えられることはないが、前方からの削除のみが行なわれるので、
一つ目のリストもキューとなる。
しかし、単純なリストで十分である。
単に二つ目のリストと区別するために、一つ目のリストをスタックと呼ぶことにする。
それぞれのステップで、gstep は二つの最右の木をスタックまたはキューから選択し、
結合し、その結果をキューの最後に加える。
アルゴリズムの最後では、キューは単一の木を含むことになり、それが貪欲アルゴリズムの解となる。
図 8.2 (重みのみを見せている) はこの手法がそのように振る舞うかの例を与える。
この手法はそれぞれのキューの操作が定数時間を取るならば成立する。
しかし、3章ですでに symmetric list を見ており、これは要求をちょうど満足する。

Here are the details.
First we set up the type SQ of s:

ここに詳細を示す。
まず、Stack-Queue の型 SQ を用意する:

> type SQ a = (Stack a,Queue a)
> type Stack a = [a]
> type Queue a = SymList a

Now we can define

今や、次が定義できる

> huffman :: [Elem] -> Tree Elem
> huffman = extractSQ . until singleSQ gstep . makeSQ . map leaf

{- p.195 -}

 Stack of weights     Queue of combined weights
------------------------------------------------
 1, 2, 4, 4, 6, 9
       4, 4, 6, 9     1 + 2
          4, 6, 9     4 + (1 + 2)
	        9     4 + (1 + 2), 4 + 6
		      4 + 6, 9 + (4 + (1 + 2))
		      (4 + 6) + (9 + (4 + (1 + 2)))

Figure 8.2 Example of the stack and queue operations

The component functions on the right-hand side are defined in terms of the type

右辺にある構成関数は木と重みの対の次の型から定義される。

> type Pair = (Tree Elem,Weight)

of pairs of trees and weights.
First of all, the functions leaf and node (needed in the definition of gstep)
are smart constructors that install weight information correctly:

まず最初に、関数 leaf と node ( gstep の定義に必要 )
は重みの情報を正しく保持するようなスマートコンストラクタだ。

> leaf :: Elem -> Pair
> leaf (c,w) = (Leaf (c,w),w)
> node :: Pair -> Pair -> Pair
> node (t1,w1) (t2,w2) = (Node t1 t2,w1+w2)

Next, the function makeSQ initialises a Stack-Queue:

次に、関数 makeSQ は Stack-Queue を初期化する:

> makeSQ :: [Pair] -> SQ Pair
> makeSQ xs = (xs,nilSL)

Recall that the function nilSL returns an empty symmetric list.

関数 nilSL は空の symmetric list を返すことを思いだそう。

Next, the function singleSQ determines whether a Stack-Queue is a singleton,
and extractSQ extracts the tree:

さらに、関数 singleSQ は Stack-Queue が singleton かどうかを判定し、
extractSQ は木を取りだす。

> singleSQ :: SQ a -> Bool
> singleSQ (xs,ys) = null xs && singleSL ys
> extractSQ :: SQ Pair -> Tree Elem
> extractSQ (xs,ys) = fst (headSL ys)

The function singleSL, whose definition is left as an exercise,
tests for whether a symmetric list is a singleton.

関数 singleSL ( この定義は演習問題として残しておく ) は
symmetric list が singleton かどうかを判定する。

Finally, we define

最後に、次を定義する

> gstep :: SQ Pair -> SQ Pair
> gstep ps = add (node p1 p2) rs
>            where (p1,qs) = extractMin ps
>                  (p2,rs) = extractMin qs
> add ::Pair -> SQ Pair -> SQ Pair
> add y (xs, ys) = (xs,snocSL y ys)

{- p.196 -}

It remains to define extractMin for extracting a tree with minimum weight from a Stack-Queue:

Stack-Queue から最小の重みの木を取り出す extractMin の定義が残っている:

> extractMin :: SQ Pair -> (Pair,SQ Pair)
> extractMin (xs,ys)
>   | nullSL ys = (head xs,(tail xs, ys))
>   | null xs = (headSL ys,(xs,tailSL ys))
>   | snd x <= snd y = (x,(tail xs,ys))
>   | otherwise = (y,(xs,tailSL ys))
>   where x = head xs; y = headSL ys

If both the stack and the queue are nonempty,
then the tree with the smallest weight from either list is selected.
If one of the stack and the queue is empty,
the selection is made from the other component.

スタックとキューが両方とも空でないなら、
最小の重みの木が両方のリストから選択される。
スタックとキューのうちの片方が空の場合、
そうでない法から選択が行なわれる。

The linear-time algorithm for Huffman coding depends on the assumption
that the input is sorted into ascending order of weight.
If this were not the case, then O(n log n) steps have to be spent sorting.
Strictly speaking, that means Huffman coding actually takes O(n log n) steps.
There is an alternative implementation of the algorithm with this running time,
and that is to use a priority queue.
Priority queues will be needed again, particularly in Part Six, so we will consider them now.

ハフマン符号化の線形時間のアルゴリズムは入力が重みの昇順に整列されている仮定によっている。
その場合でなければ、O(n log n) ステップが整列に消費される必要がある。
厳密に言えば、ハフマン符号化は O(n log n) ステップを実際には消費する。
この実行時間を持つアルゴリズムの代わりの実装があり、それは優先度付きキューを利用する。
特に第六部で、優先度付きキューが再び必要になるので、今ここでも考えることにする。

------

> data Tree a = Leaf a | Node (Tree a) (Tree a)
>             deriving (Eq, Show)

> type Forest a = [Tree a]

> fringe :: Tree a -> [a]
> fringe (Leaf x) = [x]
> fringe (Node u v) = fringe u ++ fringe v

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

> type SymList a = ([a], [a])

> nilSL :: SymList a
> nilSL = ([],[])

> nullSL :: SymList a -> Bool
> nullSL ([],[]) = True
> nullSL  _      = False

> singleSL :: SymList a -> Bool
> singleSL ([_],[]) = True
> singleSL ([],[_]) = True
> singleSL  _       = False

> consSL :: a -> SymList a -> SymList a
> consSL x (xs,ys) = if null ys then ([x],xs) else (x:xs,ys)

> headSL :: SymList a -> a
> headSL ([],ys) = case ys of
>                    []   -> undefined
>                    y:_  -> y
> headSL (x:_,_) = x

> tailSL :: SymList a -> SymList a
> tailSL (xs,ys)
>   | null xs    = if null ys then undefined else nilSL
>   | single xs  = (reverse vs, us)
>   | otherwise  = (tail xs, ys)
>   where (us,vs) = splitAt (length ys `div` 2) ys

> snocSL :: a -> SymList a -> SymList a
> snocSL x (xs,ys) = if null xs then (ys,[x]) else (xs,x:ys)

> initSL :: SymList a -> SymList a
> initSL (xs,ys)
>   | null ys    = if null xs then undefined else nilSL
>   | single ys  = (us, reverse vs)
>   | otherwise  = (xs, tail ys)
>   where (us,vs) = splitAt (length xs `div` 2) xs

> lastSL :: ([p], [p]) -> p
> lastSL (xs,ys) = if null ys
>                  then if null xs
>                       then error "lastSL of empty list"
>                       else head xs
>                  else head ys

> picks :: [a] -> [(a,[a])]
> picks []     = []
> picks (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- picks xs]

> insert :: Tree Elem -> Forest Elem -> Forest Elem
> insert = undefined
