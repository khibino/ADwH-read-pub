
8.3 Priority queues -- 優先度付きキュー
-----

A priority queue is a data structure PQ for maintaining a list of values so that
the following two operations take at most logarithmic time in the length of the list:

優先度付きキューは以下の二つの操作が最大でもリストの長さの対数時間となるよう
値のリストを管理するデータ構造 PQ です:

  insertQ :: Ord p => a -> p -> PQ a p -> PQ a p
  deleteQ :: Ord p => PQ a p -> ((a,p),PQ a p)

The function insertQ takes a value and a priority and inserts the value into the queue with the given priority.
The function deleteQ takes a nonempty queue and extracts a value whose priority is the smallest,
returning the value and its associated priority, together with the remaining queue.
In a max-priority queue the function deleteQ would extract a value with the largest priority.
As well as the two functions above, we also need some other functions on priority queues, including

関数 insertQ は値と優先度を取り、与えられた優先度で値をキューへと挿入する。
関数 deleteQ は空でないキューを取り、優先度が最小の値を取り出し、
割り当てられた優先度とその値を、残りのキューと一緒に返す。
大きい優先度を優先するキューでは関数 deleteQ は最大の優先度を持つ値を返すだろう。
上の二つの関数だけでなく、優先度付きキューには他のいくつかの関数も必要で、次のものを含む

  emptyQ :: PQ a p
  nullQ :: PQ a p -> Bool
  addListQ :: Ord p => [(a,p)] -> PQ a p -> PQ a p
  toListQ :: Ord p => PQ a p -> [(a,p)]

The constant emptyQ represents an empty queue, and nullQ tests for an empty queue.
The function addListQ adds a list of value-priority pairs in one fell swoop,

定数 emptyQ は空のキューを表現し、 nullQ は空のキューを判定する。
関数 addListQ は値と優先度の対のリストを一度に追加し、

{- p.197 -}

while toListQ returns a list of value-priority pairs in order of priority.
The function addListQ can be defined in terms of insertQ (see the exercises).

一方 toListQ は値と優先度の対のリストを優先度順に返す。
関数 addListQ は insertQ を利用して定義できる。(演習問題を見よ)

One simple implementation of priority queues is to maintain the queue as a list in ascending order of priority.
But, as we have seen with Huffmans algorithm, this means that insertion is a linear-time operation.
A better method is to use a heap, similar to the heaps described in Section 5.3.
Using a heap guarantees logarithmic time for an insertion or deletion, as we will now see.

優先度付きキューのとても単純な実装は、優先度の昇順のリストとしてキューを管理することだ。
しかし、ハフマンアルゴリズムで見てきたように、これは挿入が線形時間の操作となることを意味する。
よりよい方法はヒープを使うことで、5.3節で述べたヒープと類似している。
ヒープの挿入および削除の対数時間の保証を利用して、見てみよう。

The relevant data type for heaps is the following:

ヒープの適切なデータ型は次のようになる:

> data PQ a p = Null | Fork Rank a p (PQ a p) (PQ a p)
> type Rank = Nat

A queue is therefore a binary tree.
(We use Fork as a constructor rather than Node to avoid a name clash with Huffman trees,
 but continue to refer to nodes rather than forks.)
The heap condition is that flattening a queue returns a list of elements in ascending order of priority:

よってキューは二分木だ。
(名前の衝突を避けるために Node ではなく Fork をコンストラクタとして利用するが、
 参照するときにはフォークではなくノードとする。)
ヒープの条件は、キューを平坦化したときに要素が優先度の昇順のリストを返すことだ。

> toListQ :: Ord p => PQ a p -> [(a,p)]
> toListQ Null = []
> toListQ (Fork _ x p t1 t2) = (x,p):mergeOn snd (toListQ t1) (toListQ t2)

The definition of mergeOn is left as an exercise.
Thus a queue is a heap in which the element at a node has a priority that is no larger than the priorities in each of its subtrees.
Each node of the queue stores an additional piece of information, the rank of that node.
By definition, the rank of a tree is the length of the shortest path in the tree from the root to a Null tree.
A queue is not just a heap but a variety called a leftist heap.
A tree is leftist if the rank of the left subtree of any node is no smaller than the rank of its right subtree.
This property makes heaps taller on the left, whence its name.
A simple consequence of the leftist property is that
the length of the shortest path from the root of a tree to a Null is always along the right spine of the tree.
We leave it as an exercise to show that, for a tree of size n, this length is at most ⌊log(n+1)⌋.

mergeOn の定義は演習問題として残しておく。
このように、キューは、ノードにある要素がそれぞれの部分木内の優先度よりも大きくない優先度を持つヒープだ。
キューのそれぞれのノードは追加の情報(ノードのランク)を保存する。
定義から、木のランクはルートから Null の木までの最短パスの長さだ。
キューは単なるヒープではなく leftist ヒープと呼ばれる種別だ。
任意のノードの左の部分木のランクが右の部分木のランクより小さくないなら、木は leftist だ。
この性質はヒープの左側を高くするが、そこからこの名前が生じた。
leftiest の性質の単純な帰結として、
ルートから Null の木までの最短パスの長さは常に木の右のスパインに沿ったものになる。
サイズ n の木について、その長さが最大でも ⌊log(n+1)⌋ となることを示すのは、演習問題として残しておく。

We can maintain rank information with the help of a smart constructor fork:

スマートコンストラクタ fork の助けでランク情報を維持できる:

> fork :: a -> p -> PQ a p -> PQ a p -> PQ a p
> fork x p t1 t2
>   | r2 <= r1 = Fork (r2 + 1) x p t1 t2
>   | otherwise = Fork (r1 + 1) x p t2 t1
>   where r1 = rank t1; r2 = rank t2
> rank ::PQ a p -> Rank
> rank Null = 0
> rank (Fork r _ _ _ _) = r

{- p.198 -}

In order to maintain the leftist property,
the two subtrees are swapped if the left subtree has lower rank than the right subtree.

leftist の性質を維持するために、
左の部分木が右の部分木よりも低いランクを持つ場合、二つの部分木は交換される。

Two leftist heaps can be combined into one by the function combineQ, where

二つの leftist ヒープは、関数 combineQ によって一つに結合される。combineQ は

> combineQ :: Ord p => PQ a p -> PQ a p -> PQ a p
> combineQ Null t = t
> combineQ t Null = t
> combineQ (Fork k1 x1 p1 l1 r1) (Fork k2 x2 p2 l2 r2)
>   | p1 <= p2  = fork x1 p1 l1 (combineQ r1 (Fork k2 x2 p2 l2 r2))
>   | otherwise = fork x2 p2 l2 (combineQ (Fork k1 x1 p1 l1 r1) r2)

In the worst case, combineQ traverses the right spines of the two trees.
Hence the running time of combineQ on two leftist heaps of rank at most r is O(log r) steps.
Now we can define the insertion and deletion operations (the functions emptyQ and  nullQ are left as exercises):

最悪のケースで、combineQ は二つの木の右のスパインを辿る。
よって、最大で r のランクを持つ二つの leftist ヒープでの combineQ の実行時間は O(log r) ステップだ。
今や、挿入と削除の操作を定義できる(関数 emptyQ と nullQ は演習問題として残す):

> insertQ :: Ord p => a -> p -> PQ a p -> PQ a p
> insertQ x p t = combineQ (fork x p Null Null) t
> deleteQ :: Ord p => PQ a p -> ((a,p),PQ a p)
> deleteQ (Fork _ x p t1 t2) = ((x,p), combineQ t1 t2)

Both operations take logarithmic time in the size of the queue.
Summarising, by using a priority queue of n elements rather than an ordered list
we can reduce the time for an insertion to O(log n) steps rather than O(n) steps.
The price paid for this reduction is that the time to find a smallest value goes up from O(1) steps to O(log n) steps.

操作は両方ともキューのサイズの対数時間を取る。
まとめると、順序付きのリストではなく n 要素の優先度付きキューを利用することで、
挿入の時間を O(n) ステップから O(lon n) ステップへと減らすことができる。
この削減のために払った代償は、最小の値を見つける時間が O(1) ステップから O(log n) ステップになることだ。

Finally, here is the implementation of Huffmans algorithm using a priority queue:

最後に、優先度付きキューを使ったハフマンアルゴリズムの実装を示す。

> huffman :: [Elem] -> Tree Elem
> huffman = extract . until singleQ gstep . makeQ . map leaf
> extract :: PQ (Tree Elem) Weight -> Tree Elem
> extract = fst . fst . deleteQ
> gstep :: PQ (Tree Elem) Weight -> PQ (Tree Elem) Int
> gstep ps = insertQ t w rs
>            where (t,w) = node p1 p2
>                  (p1,qs) = deleteQ ps
>       	   (p2,rs) = deleteQ qs
> makeQ :: Ord p => [(a,p)] -> PQ a p
> makeQ xs = addListQ xs emptyQ
> singleQ :: Ord p => PQ a p -> Bool
> singleQ = nullQ . snd . deleteQ

{- p.199 -}

This algorithm runs in O(n log n) steps without making the assumption that the input is sorted by weight.

このアルゴリズムは、入力が重み順に整列しているという仮定無しに O(n log n) ステップで動作する。

8.4 Chapter notes
-----

The minimum-cost tree problem was first described in [1].
Another way to build a minimum-cost tree is to use either the Hu-Tucker [2] or the Garsia-Wachs algorithm [5].
The Hu-Tucker algorithm applies because cost is a regular cost function as defined in [2].
But the best implementation of the Hu-Tucker algorithm takes Θ(n log n) steps.
The Garsia-Wachs algorithm will be discussed in Section 14.6.

最小コスト木の問題は最初に [1] で述べられた。
最小コスト木を作るもう一つの別の方法は Hu-Tucker [2] や Garsia-Wachs のアルゴリズム [5] を利用した。
Hu-Tucker のアルゴリズムは cost が [2] で定義された正規化コスト関数であることを応用する。
しかし、Hu-Tucker のアルゴリズムの最も良い実装は Θ(n log n) ステップを取る。
Garsia-Wachs のアルゴリズムは 14.6節で議論する。

Huffmans algorithm is a firm favourite in the study of greedy algorithms.
It first appeared in [3].
The linear-time greedy algorithm based on queues is described in [4],
which also shows how the algorithm can be generalised to deal with k-ary trees rather than just binary trees.
If one insists that the fringe of the tree is exactly the given character-weight pairs in the order they are given,
then the resulting tree, called an alphabetic tree by Hu, can be built using the Garsia-Wachs algorithm.

ハフマンアルゴリズムは貪欲アルゴリズムを学ぶ際の定番だ。
最初に [3] で登場した。
キューに基づいた線形時間の貪欲アルゴリズムは [4] で述べられた。
それはこのアルゴリズムが単なる二分木ではなく k分木を使って生成できることをも示している。
木のフリンジがちょうど与えられた順の文字と重みの対であることを強調するなら、
その結果の木は (Hu は alphabetic tree と呼ぶ) Garsia-Wachs アルゴリズムで構築できる。

There are many implementations of priority queues, including leftist heaps, skew heaps, and maxiphobic heaps.
All these can be found in [6, 7].

leftist ヒープ、skew ヒープ、maxiphobic ヒープを含む、多くの優先度付きキューの実装がある。
それらはすべて、[6, 7] に見つけることができる。

 ### References

[1] Richard Bird. Pearls of Functional Algorithm Design. Cambridge University Press, Cambridge, 2010.
[2] Te Chiang Hu. Combinatorial Algorithms. Addison-Wesley, Reading, MA, 1982.
[3] David A. Huffman. A method for the construction of minimum-redundancy codes.
    Proceedings of the IRE, 40(9):10981101, 1952.
[4] Donald E. Knuth.
    The Art of Computer Programming, volume 1: Fundamental  Algorithms. Addison-Wesley, Reading, MA, third edition, 1997.
[5] Donald E. Knuth.
    The Art of Computer Programming, volume 3: Sorting and  Searching. Addison-Wesley, Reading, MA, second edition, 1998.
[6] Chris Okasaki. Purely Functional Data Structures. Cambridge University Press,  Cambridge, 1998.
[7] Chris Okasaki. Fun with binary heap trees.
    In J. Gibbons and O. de Moor, editors, The Fun of Programming, pages 116. Palgrave, Macmillan, Hampshire, 2003.

> type Nat = Int

> type Weight = Nat
> type Elem = (Char,Weight)

> type Pair = (Tree Elem,Weight)

> data Tree a = Leaf a | Node (Tree a) (Tree a)
>             deriving (Eq, Show)

> leaf :: Elem -> Pair
> leaf (c,w) = (Leaf (c,w),w)
> node :: Pair -> Pair -> Pair
> node (t1,w1) (t2,w2) = (Node t1 t2,w1+w2)


> mergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
> mergeOn = undefined

> emptyQ :: PQ a p
> emptyQ = Null

> nullQ :: PQ a p -> Bool
> nullQ Null      = True
> nullQ (Fork {}) = False

> addListQ :: Ord p => [(a,p)] -> PQ a p -> PQ a p
> addListQ ps q = foldr (uncurry insertQ) q ps
