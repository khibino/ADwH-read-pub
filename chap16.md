
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


* 最小コストより少ないか等しい推定をするヒューリスティックを楽観的ヒューリスティック (optimistic heuristic) または admissible heuristic と言う
    * 頂点vからゴール(複数あってもよい)までの経路の最小コストを H(v) とすると、ヒューリスティック関数 `h(v)` について、 `h(v) ≤ H(v)`
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

### Exercise 16.1

辺のコストが正であることに制約されない場合，関数Hは well-defined か？
辺のコストが正であるが整数である必要はない場合、H は well-defined か？

----

辺のコストが負になりうると、コストが減少するループが考えられ、最小コスト H が定まらない例を作ることができる.
よって well-defined でない

辺のコストが正だが、整数でなくてよい場合、は well-defined になる.
有限グラフでない場合は well-defined にならない.

----

### Exercise 16.2

次のグラフを考える.

```
      0         5
 A <-----> B ------> C
```

楽観的ヒューリスティックの関数を `h(A) = h(B) = 4` かつ `h(C) = 0` とする.
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

# 16.2 Searching with a monotonic heuristic

----

## 単調ヒューリスティック

ヒューリスティック関数 `h` が
始点 `u`、終点 `v`、コスト `c` の辺 `(u,v,c)` に対し
`h(u) ≤ c + h(v)` を満たすとき、単調ヒューリスティックという.

ゴール `v` に対し、`h(v) = 0` とすると、楽観的ヒューリスティックでもあることが分かる.

```
... (u₃,u₂,c₃) (u₂,u₁,c₂) (u₁,v,c₁)
h(v) = 0 = H(v)
h(u₁) ≤ c₁ + h(v)  = c₁ + H(v)  = H(u₁)
h(u₂) ≤ c₂ + h(u₁) ≤ c₂ + H(u₁) = H(u₂)
h(u₃) ≤ c₃ + h(u₂) ≤ c₃ + H(u₂) = H(u₃)
...
```

----

## 単調ヒューリスティック/訪れた頂点集合

単調ヒューリスティックの場合、同じ頂点を複数回訪れる必要がないことがわかる(後述)ため、有限マップは必要無い.

すでに訪れた頂点集合を持つようにする.

4.4節の集合演算、あるいは Data.Set を利用する.

```
empty :: Ord a ⇒ Set a
member:: Ord a ⇒ a → Set a → Bool
insert :: Ord a ⇒ a → Set a → Set a
```

`member` と `insert` は対数時間.
名前の衝突を避けるために、qualified import を利用する.

```
import qualified Data.Set as S
```

----

## mstar アルゴリズム

`h`が単調という仮定のもとで、
単調探索アルゴリズムmstarは、ゴールへの最適経路が存在すれば、それを見つけることができる.

```
mstar :: Graph → Heuristic → (Vertex → Bool) → Vertex → Maybe Path
mstar g h goal source = msearch S.empty start
  where start = insertQ ([source],0) (h source) emptyQ
        msearch vs ps | nullQ ps = Nothing
                      | goal (end p) = Just (extract p)
                      | seen (end p) = msearch vs qs
                      | otherwise = msearch (S.insert (end p) vs) rs
          where seen v = S.member v vs
                (p,qs) = removeQ ps
                rs = addListQ (succs g h vs p) qs

succs :: Graph → Heuristic → S.Set Vertex → Path → [(Path,Cost)]
succs g h vs p = [extend p v d | (v,d) ← g (end p),not (S.member v vs)]
  where extend (vs, c) v d = ((v: vs, c+d), c+d +h v)
```

発見した経路 `p` は `source` から `v` への経路のなかで最小のコストを持つ.

同じ終点を持つ経路を一つだけ候補に追加するので `aster` より効率的.

----

## mstar の論証

頂点 `v` への経路 `p` を `v` への経路 `p'` より先に見つけたとき、`c(p) ≤ c(p')` を示す.

```
v : 頂点
p : v を終点とする経路で候補にある
p': v を終点とする経路だが候補には無い
q': p を見つけたときに候補にある p' の接頭辞
u : q' の終点
r : u から v までの p' の接尾辞
```

```
   c(p)
 ≤ { q' よりも先に p が選ばれているので c(p) + h(v) ≤ c(q') + h(u) }
   c(q') + h(u) − h(v)
 = { 経路のコストの定義から c(q') + c(r) = c(p') }
   c(p') − c(r) + h(u) − h(v)
 ≤ { h が単調かつ r は u から v への経路なので h(u) ≤ c(r) + h(v) }
   c(p')
```

`h(u) ≤ c(r) + h(v)` は単調性の一般化になっている. 証明は練習問題.

----

## 単調でないヒューリスティック関数における mstar

図16.1の例で、ヒューリスティック関数 h が楽観的だが単調でない場合、mstarは最適でない解を返すことがあることを示す.

グラフ 図16.1 (再掲)
```
    5       5
A ----> B ----> D
 \     ↗
 2\   /2
   ↘ /
    C
```

```
   | A  B  C  D
---+------------
 h | 9  1  5  0
```

`h(C) > 2 + h(B)` となり、 `h(C) ≤ 2 + h(B)` が成立しないので単調ではない.

```
 ps(候補集合)         vs      p
-------------------+-------+-----------+---------------------------------
 A(0+9)            |       | A(0+9)    |
 AB(5+1) AC(2+5)   | A     | AB(5+1)   |
 ABD(10+0) AC(2+5) | A,B   | AC(2+5)   | Bが処理済みなので ACB を候補に入れない
 ABD(10+0)         | A,B,C | ABD(10+0) |
```

----

## 辺の接続が多い場合の mstar


```
   |  A  B  C  D  E  F
---+-------------------
 h | 10 10  5  5  0  0
```

```
         A
       ／ ＼
   3／       ＼10
 ／      5      ＼
B --------------- C
 \               /
 8\             /10
   \     6     /   1
    D ------- E ------- F

      A
     / \         B                                C
    /   \          ＼                           ／
 20/     \20          ＼                     ／
  /       \            20＼               ／2
 /         \                ＼         ／
D           E                 E      D
```

```
 ps                                                                                   vs             p
-----------------------------------------------------------------------------------+--------------+---------------+
 A (0+10)                                                                          |              | A (0+10)      |
 AB (3+10), AC (10+5), AE (20+0), AD (20+5)                                        | A            | AB (3+10)     |
 ABC (8+5), AC (10+5), ABD (11+5), AE (20+0), ABE (23+0), AD (20+5)                | A,B          | ABC (8+5)     |
 AC (10+5), ABCD (10+5), ABD (11+5), ABCE (18+0), AE (20+0), ABE (23+0), AD (20+5) | A,B,C        | ABCD (10+5)   |
 ABD (11+5), ABCDE (16+0), ABCE (18+0), AE (20+0), ABE (23+0), AD (20+5)           | A,B,C,D      | ABCDE (16+0)  |
 ABCDEF (17+0), ABCE (18+0), AE (20+0),  ABE (23+0), AD (20+5)                     | A,B,C,D,E    | ABCDEF (17+0) |
```

接続している辺が多いために、削除されるだけの候補が多数追加される.

----

## 辺の接続が多い場合対策としての優先度付き探索待ち行列

優先度付き探索待ち行列(Priority Search Queue)

```
insertQ :: (Ord k,Ord p) ⇒ (a → k) → a → p → PSQ a k p → PSQ a k p
addListQ :: (Ord k,Ord p) ⇒ (a → k) → [(a,p)] → PSQ a k p → PSQ a k p
{- deleteQ :: (Ord k,Ord p) ⇒ (a → k) → PSQ a k p → ((a,p),PSQ a k p) -- まちがい? -}
deleteQ :: (Ord k,Ord p) ⇒ PSQ a k p → ((a,p),PSQ a k p)
emptyQ :: PSQ a k p
nullQ :: PSQ a k p → Bool
```
deleteQ は最小優先度のエントリを消すだけなので key 関数 `(a -> k)` は要らないはず.

```
 ps                                                                                   vs             p
-----------------------------------------------------------------------------------+--------------+---------------+
 A (0+10)                                                                          |              | A (0+10)      |
 AB (3+10), AC (10+5), AE (20+0), AD (20+5)                                        | A            | AB (3+10)     |
 ABC (8+5), ABD (11+5), AE (20+0)                                                  | A,B          | ABC (8+5)     |
 ABCD (10+5), ABCE (18+0)                                                          | A,B,C        | ABCD (10+5)   |
 ABCDE (16+0)                                                                      | A,B,C,D      | ABCDE (16+0)  |
 ABCDEF (17+0)                                                                     | A,B,C,D,E    | ABCDEF (17+0) |
```

優先度が高いもののみ候補に入れるようにするので待ち行列の長さが小さくなる.


```
 ps                                                                                   vs             p
-----------------------------------------------------------------------------------+--------------+---------------+
 A (0+10)                                                                          |              | A (0+10)      |
 AB (3+10), AC (10+5), AE (20+0), AD (20+5)                                        | A            | AB (3+10)     |
 ABC (8+5), AC (10+5), ABD (11+5), AE (20+0), ABE (23+0), AD (20+5)                | A,B          | ABC (8+5)     |
 AC (10+5), ABCD (10+5), ABD (11+5), ABCE (18+0), AE (20+0), ABE (23+0), AD (20+5) | A,B,C        | ABCD (10+5)   |
 ABD (11+5), ABCDE (16+0), ABCE (18+0), AE (20+0), ABE (23+0), AD (20+5)           | A,B,C,D      | ABCDE (16+0)  |
 ABCDEF (17+0), ABCE (18+0), AE (20+0),  ABE (23+0), AD (20+5)                     | A,B,C,D,E    | ABCDEF (17+0) |
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

astar や mstar で Data.Map の関数 insert を利用するのに、なにを仮定をしているか.

insert :: Ord k ⇒ k → a → Map k a → Map k a

----

mstar の方は Data.Set の insert の間違いだと思う.

Vertex が Ord であることを仮定している.

----

Exercise 16.6

町から一番近いゴールまでの直線距離を返すヒューリスティックは、単調なヒューリスティックか.

----

単調なヒューリスティックになる.
三角不等式から単調性が成り立つ.

```
           v
         ／ ＼
     c／       ＼h(v)
   ／             ＼
  u -------------- goal
         h(u)
```

`h(u) ≤ c + h(v)`

----

Exercise 16.7

最小の辺のコストを `c` としたとき、定数関数 `h(v) = c` は楽観的なヒューリスティックか.

----

楽観的なヒューリスティックではない.
ゴールについて考えると `H = 0` なので `h ≤ H` が成立しない.

----

Exercise 16.8

すべてのゴール `v` について `h(v) = 0` ならば、`h` が単調のときは `h` は楽観的であることを示せ.

----

<!-- ₀ ₁ ₂ ₙ ₖ ₊ ₌ ₋ ⁿ⁻¹ ᵏ⁼¹  ≤ ⇒ Σ -->

`v` からゴール `vₙ` 最短経路 `[v,v₁,v₂,...,vₙ]` を考え、 その辺のコストを `[c₁,c₂,...,cₙ]` とする.
すると、単調な `h` と  `1 ≤ k ≤ n-1` について、

```
{- hの単調性 -}
h(v)  ≤ c₁ + h(v₁) ⇒ h(v) - h(v₁)  ≤ c₁
h(vₖ) ≤ cₖ₊₁ + h(vₖ₊₁) ⇒ h(vₖ) - h(vₖ₊₁) ≤ cₖ₊₁
```
よって

```
               ₙ₋₁                         ₙ₋₁
h(v) - h(v₁) + Σ ( h(vₖ) - h(vₖ₊₁) ) ≤ c₁ + Σ cₖ₊₁
               ¹                           ¹
⇒
h(v) - h(vₙ) ≤ c₁+c₂+...+cₙ
⇒ {- ゴール vₙ について h(vₙ) = 0 -}
h(v) ≤ c₁+c₂+...+cₙ = H(v)
```

<!-- 2022-11-06 ここまで -->

----

Exercise 16.9

`v` が経路 `p` の終点のとき `f(p) = c(p) + h(v)` と定義する.
`p` が `q` の接頭辞のとき、`f(p) ≤ f(q)` を示せ.

書いてないけど `h` は単調と仮定しているはず.

----

```
w: q の終点
r: v から w までの q の接尾辞

補題 h(v) ≤ c(r) + h(w) を示す.

辺の数が n の r を rₙ とする.
rₙ を構成する辺を e₁,e₂,...,eₙ、それぞれのコストを c₁,c₂,...,cₙ、終点を wₙ とする.

n についての帰納法で示す.

n = 0 のとき、v と w₀ は一致し、c(r₀) = 0 なので
h(v) ≤ c(r₀) + h(w₀) が成立.

n = k+1 のとき

  h(v)
  {- 帰納法の仮定 -}
≤ c(rₖ) + h(wₖ)
  {- hの単調性-}
≤ c(rₖ) + cₖ₊₁ + h(wₖ₊₁)
  {- コスト c(rₖ₊₁) の定義 -}
= c(rₖ₊₁) + h(wₖ₊₁)

で成立.

よって、任意の辺の数の r で成立.

---

w: q の終点
r: v から w までの q の接尾辞

主定理 f(p) ≤ f(q) を示す.

  f(p)
  {- f の定義 -}
= c(p) + h(v)
  {- 補題 h(v) ≤ c(r) + h(w) -}
≤ c(p) + c(r) + h(w)
  {- コスト c(q) の定義, q は p と r の連結 -}
= c(q) + h(u)
  {- f の定義 -}
= f(q)
```

----

# 16.3 Navigating a warehouse

----

## 倉庫内の誘導

A*の例

* 格子点間を移動
* 障害物(格子点を結んだ単位正方形の箱)を避ける

格子点間の移動(線分)の制約がいくつか考えられる

* 水平および垂直のみ
* 水平、垂直、45度 (以下で議論)
* 任意の角度 (以下で議論)

教科書の例: 図 16.2 および 図 16.3

----

## 定式化

* m×n の格子上の点. 座標（x,y）ただし 1≦x≦m、1≦y≦n
* x＝0、x＝m＋1、y＝0、y＝n＋1 が枠
* 障害物の箱を左上の点で識別する

```
type Coord = Nat
type Vertex = (Coord,Coord)
type Box = Vertex
type Grid = (Nat,Nat,[Box])
boxes :: Grid → [Box]
boxes (_,_,bs) = bs

-- 箱の四隅
corners :: Box → [Vertex]
corners (x,y) = [(x, y),(x+1, y),(x+1,y−1),(x, y−1)]
```

-----

## 定式化 / 線分を45度まで許す場合

* m×n の格子上の点. 座標（x,y）ただし 1≦x≦m、1≦y≦n
* x＝0、x＝m＋1、y＝0、y＝n＋1 が枠
* 障害物の箱を左上の点で識別する

45度まで許す場合、隣接する格子点は 8つ

```
type Graph = Vertex → [Vertex]
neighbours::Grid → Graph
neighbours grid = filter (free grid) · adjacents
adjacents::Vertex → [Vertex]
adjacents (x,y) = [(x−1, y−1),(x−1, y),(x−1, y+1),
                   (x,   y−1),         (x,   y+1),
                   (x+1, y−1),(x+1, y),(x+1, y+1)]
free ::Grid → Vertex → Bool
free (m,n,bs)=(a!)
  where a = listArray ((0,0),(m+1,n+1)) (repeat True)
            // [((x, y),False) | x ← [0..m+1],y ← [0,n+1]]
            // [((x, y),False) | x ← [0,m+1],y ← [1..n]]
            // [((x, y),False) | b ← bs,(x,y) ← corners b]
-- 演算子 // は配列の更新
```

----

## 線分を45度まで許す場合 / 経路の計算


```
type Dist = Float
type Path = ([Vertex],Dist)
end :: Path → Vertex
end = head · fst

extract ::Path → Path
extract (vs,d)=(reverse vs,d)
```

```
-- 経路を計算する関数
fpath :: Grid → Vertex → Vertex → Maybe Path
fpath grid source target = mstar (neighbours grid) source target

-- グラフ、ソース頂点、ターゲット頂点を取る
mstar :: Graph → Vertex → Vertex → Maybe Path

-- ヒューリスティック関数の代わりにターゲット頂点を引数に取る. ヒューリスティック関数はユークリッド距離
succs :: Graph → Vertex → S.Set Vertex → Path → [(Path,Dist)]
succs g target visited p = [extend p v | v ← g (end p),not (S.member v visited)]
  where extend (u:vs, d) v = ((v:u:vs, dv), dv + dist v target)
                             where dv = d + dist u v
```

-----

## 線分の角度を任意に取る場合 / 経路の計算

```
-- 線分の開始地点から行き先が見えるか
visible :: Grid → Segment → Bool

type Segment = (Vertex,Vertex)

-- 経路を計算する関数
vpath ::Grid → Vertex → Vertex → Maybe Path
vpath grid source target = mstar (neighbours grid) (visible grid) source target

-- 引数に、visible grid が追加される
mstar :: Graph → (Segment → Bool) → Vertex → Vertex → Maybe Path

-- vtest に visible grid が渡される
succs g vtest target vs p = [extend p w | w ← g (end p),not (S.member w vs)]
  where extend (v: vs,d) w = if not (null vs) ∧ vtest (u,w)
                             then ((w: vs,du),du+dist w target)
                             else ((w: v: vs,dw),dw+dist w target)
                             where u = head vs
                                   du = d - dist u v + dist u w
                                   dw = d + dist v w

neighbours (m,n,bs) (x1, y1) =
	[ (x2,y2)
	| x2 ← [1..m], y2 ← [1..n]
	, visible (m,n,bs) ((x1, y1),(x2,y2)) ]
```

----

# 16.4 The 8-puzzle

----
