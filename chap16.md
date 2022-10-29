
# 16章 Heuristic search

@khibino

----

* Heuristic search (発見的探索?)
	* graph(グラフ), edge(辺), path(経路) で定式化した問題を扱う.
    * 解となる経路の候補に対して推定値を計算し、後続の探索ステップでは最も良いものを採用する. 候補を、推定値を優先度とした優先度付き待ち行列 (priority queue) で管理する.

----

# 16.1 Searching with an optimistic heuristic

----

## T* 探索


* 最小コストより少ないか等しい推定をするヒューリステイックを楽観的ヒューリステイック (optimistic heuristic) または admissible heuristic と言う
    * 頂点vからゴール(複数あってもよい)までの経路の最小コストを H(v) とすると、Heuristic関数 h(v) について、 h(v) ≤ H(v)
* ここでは T* 探索と呼ぶことにする


```
type Cost = Nat
type Graph = Vertex → [(Vertex,Cost)]  {- 隣接する頂点とそれに付随する辺のコストのリストを返す -}
type Heuristic = Vertex → Cost  {- 頂点から最も近いゴールまでのコストを推定 -}
type Path = ([Vertex],Cost)  {- 頂点のリストとパスのコスト, リストは逆順で終点がリストの先頭 -}
```


```
end :: Path → Vertex
end = head · fst    {- an end state -}
cost :: Path → Cost
cost = snd          {- the cost of the moves -}
extract :: Path → Path
extract (vs, c) = (reverse vs  {- a list of moves -}, c)
```

----

8.3節の優先度待ち行列の操作を再利用する

```
insertQ :: Ord p ⇒ a → p → PQ a p → PQ a p
addListQ :: Ord p ⇒ [(a,p)] → PQ a p → PQ a p
deleteQ :: Ord p ⇒ PQ a p → ((a,p),PQ a p)
emptyQ :: PQ a p
nullQ :: PQ a p → Bool
{- 捨てる要素の優先度を利用しないので deleteQ の代わりに使う -}
removeQ :: Ord p ⇒ PQ a p → (a,PQ a p)
```

----

## tstar 関数


候補が無ければ解無し、優先度の最も高い候補があってゴールなら終わり、そうでなければ、
優先度の最も高い候補のから succs で次の候補リストを作って優先度付きキュー内で置き換えるのを繰り返す

```
tstar :: Graph → Heuristic → (Vertex → Bool) → Vertex → Maybe Path
tstar g h goal source = tsearch start
  where start = insertQ ([source],0) (h source) emptyQ
        tsearch ps | nullQ ps = Nothing
                   | goal (end p) = Just (extract p)
                   | otherwise = tsearch rs
          where (p,qs) = removeQ ps
                rs = addListQ (succs g h p) qs

succs :: Graph → Heuristic → Path → [(Path,Cost)]
succs g h (u:vs, c) = [((v:u:vs, c + d), c + d + h v) | (v,d) ← g u]
                                                 {- ゴールまでの Heuristic値を加える -}
```

----

## tstar の問題点

終了が保証されない.
AとBを無限に繰り返す.

```
    1
A <---> B     C
```

AとBで100回繰り返したあとコスト101の ABC を発見.

```
    1      100
A <---> B ----> C
```

A, B, D を約 2^50 回繰り返す.

```
    1       100
  A <---> B ----> C
  ↑
1 |
  ↓
  D
```

ゴールへの経路があれば、tstarは最小のコストで経路を見つけることができるのは良い点.
`h`が楽観的であることと、辺のコストが正であることが条件.

----

## tstar 論証: 最善でない経路は解にならない

```
v: 任意の頂点
s: 開始の頂点
p: v への最善の経路
c: 経路のコスト関数
h: ゴールまでのコストを推定する関数
H: ゴールまでの最善のコスト
```
`v` への最善の経路 `p` について、次が成り立つ.

```
c(p) + h(v) ≤ c(p) + H(v) = H(s)
```

```
u: 任意の頂点
q: 選択された u への最善でない経路
```

選択された `u` への最善でない経路 `q` について、次が成り立つ.

```
c(q) + h(u) ≤ c(p) + h(v) ≤ H(s)
```

u はゴールではありえない. u がゴールだとするとコスト推定値 0 としても次が成立して矛盾するため.

```
c(q) + h(u) = c(q) + 0 > H(s)
```

----

## tstar 論証: 解が存在すれば最善の経路が発見できる

最善でない経路が無制限に追加されないかを確認したい.


```
δ: 任意の辺の最小コスト
r: 経路
k: 経路rの長さ
```

`H(s)/δ` より大きい長さ `k` の経路 `r` を考えると、次が成立.

```
H(s)/δ < k ⇒ H(s) < kδ
```

また、それぞれの edge のコストは `δ` で下から抑えられるので、

```
kδ ≤ c(r)
```

よって

```
c(p) + h(v) ≤ H(s) < kδ ≤ c(r)
```

となり、解が存在する場合には、
この `r` のような `H(s)/δ` より長い経路が候補に追加されるよりも早く解が発見できる.

----

## tstar の動作例


グラフ 図16.1
```
    5       5
A ----> B ----> D
 \     ↗
 2\   /2
   ↘ /
    C
```

Heuristic関数
```
   | A B C D
---+---------
 h | 9 1 5 0
```

優先度付き待ち行列の状態列: `現頂点までの経路 (現頂点までのcost + 現頂点からのheuristic)`
```
A (0+9)
AB (5+1), AC (2+5)
AC (2+5), ABD (10+0)
ACB (4+1), ABD (10+0)
ACBD (9+0), ABD (10+0)
```

----

## tstar が終了しない問題

tstar が終了しない問題を解決したい

* Q. 訪れた頂点を記録する引数を持つのはどうか
* A. 2通り以上の方法で同じ頂点を通る経路がある場合にうまくいかない. 図16.1 の例だと B を追加する手順を 2通りの経路が利用する.

頂点から経路コストへのマップを保持するという方法

```
import qualified Data.Map as M
-- Data.Map の次の操作を利用
empty :: Ord k ⇒ Map k a
lookup :: Ord k ⇒ k → Map k a → Maybe a
insert :: Ord k ⇒ k → a → Map k a → Map k a
```

----

## astar 関数

T* 検索を修正したアルゴリズム、A* 検索

```
astar :: Graph → Heuristic → (Vertex → Bool) → Vertex → Maybe Path
astar g h goal source = asearch M.empty start
  where start = insertQ ([source],0) (h source) emptyQ
        asearch vcmap ps | nullQ ps = Nothing
                         | goal (end p) = Just (extract p)
                         | better p vcmap = asearch vcmap qs
                         | otherwise = asearch (add p vcmap) rs
                         where (p,qs) = removeQ ps
                               rs = addListQ (succs g h p) qs
better :: Path → M.Map Vertex Cost → Bool
better (v:vs, c) vcmap = query (M.lookup v vcmap)
                          where query Nothing = False
                                query (Just c') = c' ≤ c
                          {- vcmap にあるものの方が良い(コストが低い)場合には候補から除去して再帰 -}
add :: Path → M.Map Vertex Cost → M.Map Vertex Cost
add (v:vs, c) vcmap = M.insert v c vcmap
```

----

## 論証: astar は最適解を返す

各ステップにおいて、候補に良い経路があることを示す.

* 終点`v`を持つ経路`p`が候補にあり、有限マップが記録するエントリ `(v,c)` の `c ≦ c(p)` を満たすものが存在しないとき、`p`は開いていると言う. そうでなければ、pは閉じていると言う.
* 開いた経路はさらなる拡張の候補となり、閉じた経路はそうではない.

`P = [v0,v1,...,vn]` を開始頂点 `v0` からゴール `vn` への最適経路とし、
`Pj` を `0 ≤ j < n` の初期セグメント `[v0, v1,...,vj]` とすると、
各ステップで、ある `j` に対して終点 `vj` の開いた経路 `p` があり、`c (p) = c (Pj)` であることが示される.

初期`P = [P0]` のときもこの条件は成立している.

<!--
v0 から vi までの、c(q) = c(Pi) を満たす閉じた経路 q が候補にあるような vi の集合を D とし、
みたいな議論があるが違っていそうなので修正する
 -->

`v0` から `vi` までの閉じた経路 `q'` が候補にあるとき、そのような `vi` の集合を `D` とする.
`D` の最大の添字 `i` を取り出して `vi` と対応する経路 `q'` を取り直す.

このとき `q'` は閉じた経路なので、それに対応する開いた経路 `q` があって、`c(q) = c(Pi)`.

また、`j = i+1`、辺 `(vi,vj)` のコストを `c` 、 `q` に `(vi,vj)` を継いた経路を `p` とすると

```
c(p) = c(q) + c = c(Pi) + c = c(Pj)
```

この `p` は開いた経路となる.

以上で astar が最適解を返すとわかる.

----

Exercise 16.1

辺のコストが正であることに制約されない場合，関数Hは well-defined か？
辺のコストが正であるが整数である必要はない場合、H は well-defined か？

----

辺のコストが負になりうると、コストが減少するループが考えられ、最小コスト H が定まらない例を作ることができる.
よって well-defined でない

辺のコストが正だが、整数でなくてよい場合、は well-defined になる.
有限グラフでない場合は well-defined にならない.

----

Exercise 16.2

次のグラフを考える.

```
      0         5
 A <-----> B ------> C
```

楽観的ヒューリスティックの関数が `h(A) = h(B) = 4` かつ `h(C) = 0` とする.
`tstar` は経路 ABC を発見するか?

----

```
   |  A  B  C
---+----------
 h |  4  4  0
---+----------
 H |  5  5  0
```

条件 `h ≤ H` は満たされているが、各辺のコストが正という条件は満たされていない.

AB を繰り返す経路が、最も良い推論値 4 を持ち続け、経路 ABC の推論値 5 より小さいため、 `tstar` は ABC を発見できない.

( `c(AB) + h(B) = c(ABA) + h(A) = 4 < c(ABC) + h(C) = 5` )

----

16.2 Searching with a monotonic heuristic

----

```
empty :: Ord a ⇒ Set a
member:: Ord a ⇒ a → Set a → Bool
insert :: Ord a ⇒ a → Set a → Set a
```


----

Exercise 16.3

ダイクストラのアルゴリズムが A*探索の special case であるのはなぜか?

----

ヒューリスティック関数が h = const 0 の場合に astar はダイクストラのアルゴリズムと一致するため.
このときには mstar の special case にもなっているらしい.

----

Exercise 16.4

優先順位がゴールまでのヒューリスティックによる推定値だけの場合、tstarは必ずしも最短経路を返さないことを示す簡単なグラフを与えよ。

----

次のグラフとヒューリスティック関数の場合、
最初の再帰のステップで経路 AC が優先順位 0 で追加されてしまい、経路 ABC を発見することができない.

```
    5
A ----> C
 \     ↗
 2\   /2
   ↘ /
    B
```

```
   | A  B  C
---+---------
 h | 4  2  0
```

----

Exercise 16.5
