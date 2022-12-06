
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

ゴールへの経路があれば、tstarは最小のコストの経路を見つけることができるのは良い点.
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

## tstar の繰り返しが長い問題

tstar の繰り返しが長い問題を解決したい

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

T* 探索を修正したアルゴリズム、A* 探索

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
= c(q) + h(w)
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

* 実線: 45度刻みでの最適  : 18 + 5√2                  ≒ 25.07 (25.071...)
* 破線: 任意の角度の例    : 3 + 2√10 + √205          ≒ 23.64 (23.642...)
* 点線: 任意の角度での最適: 3√2 + √17 + √34 + √53  ≒ 21.48 (21.476...)

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

## 定式化 / 線分の角度を45度刻みで取る場合

* m×n の格子上の点. 座標（x,y）ただし 1≦x≦m、1≦y≦n
* x＝0、x＝m＋1、y＝0、y＝n＋1 が枠
* 障害物の箱を左上の点で識別する

45度まで許す場合、隣接する格子点は 8つ

```
type Graph = Vertex → [Vertex]
neighbours :: Grid → Graph
neighbours grid = filter (free grid) · adjacents
adjacents :: Vertex → [Vertex]
adjacents (x,y) = [(x−1, y−1),(x−1, y),(x−1, y+1),
                   (x,   y−1),         (x,   y+1),
                   (x+1, y−1),(x+1, y),(x+1, y+1)]
free :: Grid → Vertex → Bool
free (m,n,bs)=(a!)
  where a = listArray ((0,0),(m+1,n+1)) (repeat True)
            // [((x, y),False) | x ← [0..m+1],y ← [0,n+1]]
            // [((x, y),False) | x ← [0,m+1],y ← [1..n]]
            // [((x, y),False) | b ← bs,(x,y) ← corners b]
-- 演算子 // は配列の更新
```

----

## 線分の角度を45度刻みで取る場合 / 経路の計算


```
type Dist = Float
type Path = ([Vertex],Dist)
end :: Path → Vertex
end = head · fst

extract :: Path → Path
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

## 線分の角度を任意に取る場合1 / 経路の計算

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
```

`neighbours` は 45度刻みの方を流用すると、図16.3の破線のような経路が導出される.

-----

## 線分の角度を任意に取る場合2 / 経路の計算

`neighbours` で任意の角度を取るようにする.

```
neighbours (m,n,bs) (x1, y1) =
	[ (x2,y2)
	| x2 ← [1..m], y2 ← [1..n]
	, visible (m,n,bs) ((x1, y1),(x2,y2)) ]
```

----

### Exercise 16.10

図16.3の格子には、直線18手、斜め5手の固定角の経路が他にいくつあるか？

----

異なる選択肢があるのは、
45度で下がる、1行目から2行目、2行目から3行目、3行目から4行目.
2行目から3行目は 2つの選択肢があるが、依存があり、それぞれで可能な 1行目から2行目の選択肢数は 7つと 8つ.
3行目から4行目の選択肢は 2つで、2行目から3行目での選択とは依存が無く選択できる.

よって

(8 + 7) * 2 = 30

教科書の答えが違う?

----

### Exercise 16.11

倉庫の問題で利用される関数 dist を定義せよ.

----

```
dist :: Vertex -> Vertex -> Dist
dist (x1, y1) (x2, y2) = sqrt $ fromIntegral $ square (x1 - x2) + square (y1 - y2)
  where square x = x * x
```

----

### Exercise 16.12

2本の任意の線分が交差するかどうかを判定することは、計算幾何学の基本的な課題である.
完全なアルゴリズムは、多くの様々なケースを考慮しなければならないので、少し複雑だ.
しかし、倉庫の問題の状況では、まだ多くのケースを区別しなければならないにもかかわらず、この課題はいくらか単純化することができる.
まず、構成中の経路の線分が水平、垂直、または45度の角度の傾斜である場合、何を示さなければならないか？

----

端点が障害物の角と一致しないことを示せばよい.

----

### Exercise 16.13

前問に引き続き、残りのケースでは、線分の終点に障害物がないこと、どの箱の境界も線分を横断していないことを確認する必要がある.
格子内の箱の境界線は、次のように定義できる.

```
borders :: Grid → [Segment]
borders = concatMap (edges · corners) · boxes
  where edges [u,v,w,x] = [ (u,v), (w,v), (x,w), (x,u) ]
```

しかし、すべての境界線が与えられた線分 `s` を横断しているかどうかをテストすることは、`s` から遠く離れた境界線を含むことになる.
何らかの適切な意味での `near s` で、境界線をフィルタリングする方がよい. `near` の適切な定義とは何か？

```
 u +--+ v
   |  |
 x +--+ w
```

----

線分が対角線となる長方形の重なりについてだけ考えれば良い.

```
near ((x1,y1), (x2,y2)) ((x3,y3), (x4,y4)) =
    min x1 x2 <= x3 && x4 <= max x1 x2 &&
    min y1 y2 <= y3 && y4 <= max y1 y2
```

教科書の解答.
障害物が単位正方形のみなので、これでもよさそう.

一般的には次の方がよさそう.

```
near ((x1,y1), (x2,y2)) ((x3,y3), (x4,y4)) =
    (min x1 x2 <= x3 || x4 <= max x1 x2) &&
    (min y1 y2 <= y3 || y4 <= max y1 y2)
```

----

### Exercise 16.14

続いて、visibleの定義は次のような形になる。

```
visible :: Grid → Segment → Bool
visible g s | hseg s = all (free g) (ypoints s)
            | vseg s = all (free g) (xpoints s)
            | dseg s = all (free g) (dpoints s)
            | eseg s = all (free g) (epoints s)
            | otherwise = free g (snd s) ∧ all (not · crosses s) es
  where es = filter (near s) (borders g)
```

線分は，水平なら hseg，垂直なら vseg，端点の2つの座標の和が同じなら dseg，対角線は左右対象なので，座標の差が同じなら eseg を満足する.
crosses 以外の残りの関数の妥当な定義を書け.

----

```
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
```

----

### Exercise 16.15

あとは、crosses の定義である. そのためには、三角形の向きを決定する必要がある. このとき、関数

```
orientation :: Segment → Vertex → Int
orientation ((x1,y1),(x2,y2)) (x, y) = signum ((x-x1)×(y2-y1)-(x2-x1)×(y-y1))
```

は、A = (x1,y1), B = (x2,y2), C = (x, y) の三角形 ABC の向きが反時計回りなら -1、時計回りなら +1、点 A, B, および C が平行なら 0 を返す.
たとえば、図16.5では、ABCの向きは反時計回り、ABDの向きは時計回りである.
したがって、CD がある箱の境界であれば、線分はそれを横切ることになる.
一方、ABE と ABF の向きが反対であっても、線分は EF を横切らない.
EFには交差テストが適用されないのはなぜか？ それを考慮して crosses を定義せよ.

----

`near` で線分から決まる長方形の重なりをテストしているので、EF のようなケースは除外される.

```
crosses :: Segment -> Segment -> Bool
crosses p (q1, q2) =
  orientation p q1 * orientation p q2 <= 0
```

<!-- 2022-11-27 ここまで -->

----

# 16.4 The 8-puzzle

----

## 8-puzzle

図16.4

```
   | 8 | 3
---+---+---
 2 | 5 | 6
---+---+---
 1 | 4 | 7


 1 | 2 | 3
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 |
```

8枚のタイルを配置、一箇所は空ける.
手順は空きスペースへ隣接するタイルをスライドさせるだけ.

初期状態から終了状態へ到達できるか?

解けるのは全状態のうち半分だけ.

----

## 8-puzzle の論証 / 反転カウント

反転カウント - inversion count

順列 `p` について、列 `p(0),p(1),p(2),...,p(n)` を考えたとき、
`i < j` かつ `p(i) > p(j)` が成立する `(i,j)` の組の数.

空きスペースを `0` として図16.4 を考えると、

`123456780` の 反転カウントは 8

`083256147` の 反転カウントは 14

```
(8,3) (8,2) (8,5) (8,6) (8,1) (8,4) (8,7)
(3,2) (3,1)
(2,1)
(5,1) (5,4)
(6,1) (6,4)
```

----

## 8-puzzle の論証 / 反転カウントの偶奇

```
s: 順列 p(1) から p(n) までのうち i から j までの列(i,j を除く)
m: s の長さ
Lᵢ: s のうち i より小さいものの数
Bᵢ: s のうち i より大きいものの数
Lⱼ: s のうち j より小さいものの数
Bⱼ: s のうち j より大きいものの数

Lᵢ + Bᵢ = Lⱼ + Bⱼ = m
```

```
        i(--    s    --)j
p(0),..,p(x),  ...   ,p(y),..,p(n)

Lᵢ: 反転カウントのうち片方のみが i のもの
Bⱼ: 反転カウントのうち片方のみが j のもの
c₀: s内とs外、s内同士, iとj の 反転カウント

c = c₀ + Lᵢ + Bⱼ
```


`i` と `j` を交換後の 反転カウント

```
        j(--    s    --)i
p(0),..,p(x),  ...   ,p(y),..,p(n)

Lⱼ: 反転カウントのうち片方のみが j のもの
Bᵢ: 反転カウントのうち片方のみが i のもの
c₀: s内とs外、s内同士, iとj の 反転カウント (教科書だと s の外側とあったけど間違いだと思う)

c' = c0 + Lⱼ + Bᵢ + 1 (i < j)
c' = c0 + Lⱼ + Bᵢ - 1 (i > j)
```

----

## 8-puzzle の論証 / 反転カウントの偶奇


```
c = c₀ + Lᵢ + Bⱼ

c' = c0 + Lⱼ + Bᵢ + 1 (i < j)
c' = c0 + Lⱼ + Bᵢ - 1 (i > j)
```

```
c' = c0 + Lⱼ + Bᵢ ± 1
  { c = c₀ + Lᵢ + Bⱼ  }
   = (c − Lᵢ − Bⱼ) + Lⱼ + Bᵢ ±1
  { Lᵢ + Bᵢ = Lⱼ + Bⱼ ⇒ Bᵢ - Bⱼ = - Lᵢ + Lⱼ }
   = c − 2Lᵢ + 2Lⱼ ± 1
```

よって `c` と `c'` の偶奇が一致する.

初期状態の 反転カウントが偶数で、終了状態の 反転カウントが偶数なら、偶数回の移動でのみ到達できる.

----

## 8-puzzle の論証 / マンハッタン距離

タイルの垂直水平移動の合計数

図16.4

```
   | 8 | 3
---+---+---
 2 | 5 | 6
---+---+---
 1 | 4 | 7


 1 | 2 | 3
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 |
```

終了状態までのマンハッタン距離を考える.

`1, 2, 4, 7, 8` は 2

`3, 5, 6` は 0

空きスペースは 4

----

## 8-puzzle の論証

```
1文字目 {E|O}: 順列の反転カウントの偶奇
2文字目 {E|O}: 空きスペースの終了状態までのマンハッタン距離の偶奇
```

`EE` から任意の移動を考えると `OO` になり、`OO` から任意の移動を考えると `EE` になる.

`EO` から任意の移動を考えると `OE` になり、`OE` から任意の移動を考えると `EO` になる.

もし空きスペースの行の 2枚のタイルを入れ替えると、
空きスペースのマンハッタン距離を変えずに、反転カウントの偶奇のみを変える.

つまり、`OE` は `EE` になり、 `EO` は `OO` になり、かつ、これらは全単射なので、
それぞれの並べ替えの状態集合の大きさは同じになる.

終了状態は空きスペースのマンハッタン距離が 0 なので、 `EE` または `OE` になる.

終了状態が `EE` なら、到達可能な初期状態は `EE` または `OO`.

終了状態が `OE` なら、到達可能な初期状態は `EO` または `OE`.

となり、どちらの場合も半分の場合のみが到達可能となる.

----

## 8-puzzle を楽観ヒューリスティックで解く

図16.4

```
   | 8 | 3
---+---+---
 2 | 5 | 6
---+---+---
 1 | 4 | 7


 1 | 2 | 3
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 |
```

どんなヒューリスティック関数 `h` を選ぶか.

格子 `g` に対して `h(g)` を考える

* 位置違いのヒューリスティック / out-of-place heuristic
    * 終了状態とは異なるタイルの数 - 図16.4 だと `h(g) = 5`
* マンハッタンヒューリスティック / Manhattan heuristic
    * 終了状態までのタイルのマンハッタン距離の和(空きスペースについては含めず) - 図16.4 だと `h(g) = 10`

マンハッタンヒューリスティックは位置違いのヒューリスティックの改良版になっている.

マンハッタンヒューリスティックは単調.

----

## 8-puzzle を解くプログラム


```
import qualified Data.Text as T

type Position = Nat
type State = (T.Text,Position)
-- 格子の状態
perm :: State → String
perm (xs,j) = T.unpack xs
-- 空きスペースの位置
posn0 :: State → Position
posn0 (xs,j) = j
```

```
-- 初期状態と終了状態
istate,fstate:: State
istate = (T.pack "083256147",0)
fstate = (T.pack "123456780",8)

type Move = Nat
-- 移動できる位置のリストを返す
moves:: State → [Move]
moves st = moveTable !(posn0 st)
moveTable ::Array Nat [Nat]
moveTable = listArray (0,8) [[1,3], [0,2,4], [1,5],
                             [0,4,6],[1,3,5,7],[2,4,8],
                             [3,7], [4,6,8], [5,7]]
{- 格子内の位置の添字:
  0 1 2
  3 4 5
  6 7 8 -}
```

----

## 8-puzzle を解くプログラム

```
move :: State → Move → State
move (xs,i) j = (T.replace ty t0 (T.replace t0 tx (T.replace tx ty xs)),j)
                where t0 = T.singleton '0'
                      ty = T.singleton '?'
                      tx = T.singleton (T.index xs j)
-- T.replace x y s は s 内の x をすべて y に置き換える
-- x -> ?, 0 -> x, ? -> 0
-- x <---> 0
```

```
-- 反転カウントが偶数なら真
icparity:: State → Bool
-- 空きスペースの初期状態から終了状態までのマンハッタン距離が偶数なら真
mhparity:: State → State → Bool

--
possible:: State → State → Bool
possible is fs = (mhparity is fs == (icparity is == icparity fs))
--                2                  1              終了状態
-- EE ==> E       E                  E              E
-- OO ==> E       O                  O              E
-- EO ==> O       O                  E              O
-- OE ==> O       E                  O              O
```

----

## 8-puzzle を解くプログラム / 位置違いのヒューリスティック

```
-- 位置違いのヒューリスティック
type Heuristic = State → State → Nat
h1 :: Heuristic
h1 is fs = length (filter p (zip (perm is) (perm fs)))
           where p (c,d) = c ≠ '0' ∧ c ≠ d
```

----

## 8-puzzle を解くプログラム / マンハッタン・ヒューリスティック

マンハッタン・ヒューリスティックを定義するための座標
```
(0,0) (0,1) (0,2)
(1,0) (1,1) (1,2)
(2,0) (2,1) (2,2)
```

順列 `083256147` を添字順の座標リストにする (0 は抜かす)
```
(0,0) (0,1) (0,2)
0     8     3

(1,0) (1,1) (1,2)
2     5     6

(2,0) (2,1) (2,2)
1     4     7
```

```
[(2,0), (1,0), (0,2), (2,1), (1,1), (1,2), (2,2), (0,1)]
 1      2      3      4      5      6      7      8
```

----

## 8-puzzle を解くプログラム / マンハッタン・ヒューリスティック

```
coords :: State → [Coord]
coords = tail · map snd · sort · addCoords
         where addCoords st = zip (perm st) gridpoints
               gridpoints = map (`divMod` 3) [0..8]

h2 :: Heuristic
h2 is fs = sum (zipWith d (coords is) (coords fs))
           where d (x0, y0) (x1, y1) = abs (x0 − x1) + abs (y0 − y1)
```


----

## 8-puzzle を解くプログラム / mstar

```
type Path = ([Move],Nat,State)
key :: Path → State
key (ms,k,st) = st

mstar :: Heuristic → State → State → Maybe [Move]
mstar h istate fstate =
  if possible istate fstate then msearch S.empty start else Nothing
  where start = insertQ key ([],0,istate) (h istate fstate) emptyQ
        msearch vs ps | st == fstate = Just (reverse ms)
                      | S.member st vs = msearch vs qs
                      | otherwise = msearch (S.insert st vs) rs
          where ((ms,k,st), qs) = removeQ ps {- 先に説明したように、最小優先度のエントリを消すだけなので key 引数は不要 -}
                rs = addListQ key (succs h fstate (ms, k,st) vs) qs

succs :: Heuristic → State → Path → S.Set State → [(Path,Nat)]
succs h fstate (ms,k,st) vs =
  [((m:ms, k+1, st'), k+1 + h st' fstate)
  | m ← moves st,let st' = move st m,not (S.member st' vs)]
```

----

### Exercise 16.16

2×2のマス目に3つのタイルと1つの空白がある3パズルについて考える．最終的なタイルの並べ方が1230であるとき，24通りの初期状態のうち，どれが解けるか？

----

半分の12通りが解ける

TBD

----

### Exercise 16.17

8-puzzle の位置違いヒューリスティック h が単調であることを示せ． また、空きスペースのタイルが位置違いかどうかを数えると、そうならないことを示せ。

----

任意の手順 `m : s1 → s2` について場合分けを行なう.
手順 `m` のコスト `c(m) = 1` から
手順 `m` で動かすパネルの位置が合っている状態を T、 合っていない状態を F で表現する.
また `h(s1) = p` としたとき、以下の 3通りとなり、それぞれ単調ヒューリスティックの条件が成立する.

```
 s1  | s2  |h(s1)|h(s2)| 単調ヒューリスティックの条件
-----+-----+-----+-----+--------------------------------------
  F  |  F  |  p  |  p  | h(s1) = p <= 1 +  p    = c(m) + h(s2)
     +     +     +     +
  T  |  F  |  p  | p+1 | h(s1) = p <= 1 + (p+1) = c(m) + h(h2)
     +     +     +     +
  F  |  T  |  p  | p-1 | h(s1) = p <= 1 + (p-1) = c(m) + h(h2)
```

空きスペースのタイルを位置違いと数えると、
例えば終了状態まで後 1手のときに、 h = 2 となり、これは最短手数より大きいため、楽観ヒューリスティックにならない.
楽観ヒューリスティックであることは、単調ヒューリスティックであることの必要条件なので、単調ヒューリスティックではない.

----

### Exercise 16.18

空きスペースのタイルの距離が合計に含まれないからこそ、マンハッタン・ヒューリスティックが単調になることを示せ.

----

TBD

----

### Exercise 16.19

関数 icparity と mhparity を定義せよ.
ここで、icparity は反転数のパリティが偶数の場合に真を返し、mhparity は初期状態の空きスペースのタイルから終了状態の終わりの地までのマンハッタン距離が偶数の場合に真を返す．

----

```
icparity :: State -> Bool
icparity st = even $ sum [ length $ filter (h >) cs | h:cs  <- tails (perm st) ]

mhparity :: State -> State -> Bool
mhparity istate fstate = even $ d (zpoint istate) (zpoint fstate)
  where d (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)
```
