
# 12章 Partitions

@khibino

---

* 12.1 Ways of generating partitions
    - partition の生成方法
* 12.2 Managing two bank accounts
    - スケジューリングの最適化
* 12.3 The paragraph problem
    - 段落のテキストの整形

空でないリストの partition: リストを空でない複数のリストに分割すること

12.1 は partition 生成方法について、12.2 と 12.3 は partition の応用例

---

# 12.1 Ways of generating partitions

---

## partition: 定義

```haskell
type Partition a = [Segment a]
type Segment a = [a] -- 分割された後のリスト
```

空でないリストの partition: リストを空でない複数のリストに分割すること

```haskell
-- xss が xs の partition である
concat xss = xs ∧ all (not · null) xss
```

---

## partition: 生成 - splits


```haskell
-- 空のリストは空のリストの唯一の partition と考える
parts :: [a] → [Partition a]
parts [] = [[]]
parts xs = [ys: yss | (ys,zs) ← splits xs, yss ← parts zs]

-- 空でないリスト xs を， ys が空でなく， ys++zs = xs となるようなリストのペア (ys,zs) に分割
splits :: [a] → [([a],[a])]
splits [] = []
splits (x:xs) = ([x],xs): [(x:ys,zs) | (ys,zs) ← splits xs]
```

---

## partition: 生成 - foldr, extendl

```haskell
parts ::[a] → [Partition a]
parts = foldr (concatMap · extendl) [[]]

-- partition を左に拡張
extendl :: a → Partition a → [Partition a]
extendl x [] =[cons x []]
extendl x p = [cons x p,glue x p]

cons,glue:: a → Partition a → Partition a
cons x p = [x]: p  -- 先頭に新しいセグメントを作る
glue x (s:p) = (x:s):p  -- 先頭のセグメントを拡張
```

## partition: 生成 - foldl, extendr

```haskell
parts :: [a] → [Partition a]
parts = foldl (flip (concatMap · extendr)) [[]]

-- partition を右に拡張
extendr :: a → Partition a → [Partition a]
extendr x [] = [snoc x []]
extendr x p  = [snoc x p,bind x p]

snoc,bind:: a → Partition a → Partition a
snoc x p = p ++ [[x]]  -- 末尾に新しいセグメントを作る
bind x p = init p ++ [last p ++ [x]]  -- 末尾のセグメントを拡張
```

## partition: 定義の選択

parts の定義を foldr にするか foldl にするか

問題によってはどちらを選ぶかが重要になる

次を証明するような場合を考える
```haskell
filter (all ok)· parts = foldr (concatMap · okextendl) [[]]

okextendl x = filter (ok · head)· extendl x
```

要求される融合条件

```haskell
-- 文脈依存の融合条件(context sensitive fusion condition)
filter (all ok) (concatMap (extendl x) ps) =
  concatMap (okextendl x) (filter (all ok) ps)
```

## partition: 定義の選択 - 融合条件の確認

```haskell
filter (all ok)· parts = foldr (concatMap · okextendl) [[]]
filter (all ok) · foldr (concatMap · extendl) [[]]  =  foldr (concatMap · okextendl) [[]]

h (foldr f e xs) = foldr g (h e) xs
h (f x (foldr f e xs))  =  g x (h (foldr f e xs)) -- 文脈依存の融合条件

e = [[]]
f = concatMap · extendl
g = concatMap · okextendl
h = filter (all ok)
h e = filter (all ok) [[]] = [[]]
```

```haskell
-- 文脈依存の融合条件(context sensitive fusion condition)
filter (all ok) (concatMap (extendl x) ps) =
  concatMap (okextendl x) (filter (all ok) ps)
```

## partition: 定義の選択 - 融合条件を示す条件

```haskell
-- 文脈依存の融合条件(context sensitive fusion condition)
filter (all ok) (concatMap (extendl x) ps) =
  concatMap (okextendl x) (filter (all ok) ps)
```

okが接尾辞で閉じている「ok (xs++ys) が成り立つなら ok ys も成り立つ」という仮定が必要。

同様に foldl による定義から考える場合、
ok が接頭辞で閉じている
つまり、 「ok (xs++ys) が成り立つなら ok xs も成り立つ」という仮定が必要。

-----

# 12.1 関連の Exercise

-----

Exercise 12.1

How many partitions of a list of length n > 0 are there?
長さ n > 0 のリストには partition がいくつあるか?

リストの要素と要素の間を「分割する」あるいは「分割しない」の2通りが考えられて、要素と要素の間は n-1。
よって 2^(n-1)


-----

Exercise 12.2

Why is the clause parts [] = [[]] necessary in the first definition of parts?
parts の最初の定義には parts [] = [[]] の節がなぜ必要か?

```
parts :: [a] -> [Partition a]
parts [] = [[]]
parts xs = [ys:yss|(ys,zs) <- splits xs,yss <- parts zs]

splits :: [a] -> [([a],[a])]
splits [] = []
splits (x:xs) = ([x],xs):[(x:ys,zs)|(ys,zs) <- splits xs]
```

parts [] = [[]] の節が無い場合、 parts [] = [] となってしまって、すべての parts xs が [] になってしまう。

-----

Exercise 12.3

Give another definition of parts in terms of foldr, one that at each step does all the cons operations before the glue operations.
foldr のやり方で parts のもう一つの定義を与えよ。
その定義ではそれぞれのステップですべての cons が glue より先に来るものとする。

```
parts :: [a] -> [Partition a]
parts = foldr step [[]]
  where
    step :: a -> [Partition a] -> [Partition a]
    step x ps = map (cons x) ps ++ concatMap cglue ps
      where cglue [] = []
            cglue p  = [glue x p]
```

Answer

```
parts :: [a] -> [Partition a]
parts = foldr step [[]]
  where step x [[]] = [[[x]]]
        step x ps = map (cons x) ps ++ map (glue x) ps
```

foldl 版の parts と同じ結果になるらしい

----

Exercise 12.4

Give the details of the proof that
filter (all ok)· parts = foldr (concatMap · okextendl) [[]]
provided ok is suffix-closed.
(Hint: it is probably best to express the fusion condition in terms of list comprehensions.)

ok が接尾辞で閉じているもとでの
filter (all ok)· parts = foldr (concatMap · okextendl) [[]]
の証明の詳細を与えよ(ヒント: おそらくは融合条件をリスト内包表記で表現するのがベスト)

```haskell
-- 融合条件が成立することを示す
filter (all ok) (concatMap (extendl x) ps) =
  concatMap (okextendl x) (filter (all ok) ps)

okextendl x = filter (ok · head)· extendl x
extendl x [] =[cons x []]
extendl x p = [cons x p,glue x p]
cons x p = [x]:p
glue x (s:p) = (x:s):p
-- 定義にしたがって融合条件の左辺、右辺をそれぞれ展開する

lhs = [ ep
      | p  <- ps
      , ep <- if p == [] then [ [[x]] ] else [ [x]:p, (x:head p):tail p ]  -- extendl
      , all ok ep
      ]

rhs = [ ep
      | p <- ps
      , all ok p
      , ep <- if p == [] then [ [[x]] ] else [ [x]:p, (x:head p):tail p ]  -- extendl  -- okextendl
      , ok (head ep)                                                                   -- okextendl
      ]

p == [] の場合

lhs = [ ep
      | p  <- ps
      , ep <- [ [[x]] ]  -- extendl
      , ok [x]
      ]

rhs = [ ep
      | p <- ps
      , True             -- 無くても同じなので lhs と同じ式となる
      , ep <- [ [[x]] ]  -- extendl  -- okextendl
      , ok [x]                       -- okextendl
      ]

p /= [] の場合

lhs = [ ep
      | p  <- ps
      , ep <- [ [x]:p, (x:head p):tail p ]
      , ep <- [ [x]:p             | ok [x] && all ok p ] ++            -- rhs にも all ok p があるので同条件
              [ (x:head p):tail p | ok (x:head p) && all ok (tail p) ]
                                    -- ok が接尾辞で閉じているので
                                    -- ok (x:head p) == ok ([x] ++ head p) ⇒ ok (head p)
                                    -- ok (head p) && all ok (tail p) ⇔ all ok p となり rhs と同条件
      ]

rhs = [ ep
      | p  <- ps
      , all ok p
      , ep <- [ [x]:p, (x:head p):tail p ]
      , ep <- [ [x]:p | ok [x] ] ++
              [ (x:head p):tail p | ok (x:head p) ]
      ]

//
```

-----

Exercise 12.5

Which of the following predicates on nonempty sequences of positive numbers are prefix-closed and which are suffix-closed?
正の数の空でない列に対する次の述語のうち、接頭辞において閉じているものはどれか、接尾辞において閉じているものはどれか?

```haskell
leftmin xs = all (head xs ≤) xs
rightmax xs = all (≤ last xs) xs
ordered xs = and (zipWith (≤) xs (tail xs))
nomatch xs = and (zipWith (≠) xs [0..])
```

Do each of these predicates hold for singleton lists?
これらの述語は、それぞれシングルトンリストでも成立するか?



接頭辞において閉じているもの
leftmin, ordered, nomatch

接尾辞において閉じているもの
rightmax, ordered

どの述語もシングルトンリストで成立する

-----

# 12.2 Managing two bank accounts

-----

## 問題の定義1

入出金を行なう当座預金口座の金額上限を $C$ に保つ

正負の整数のリストを最短の安全な partition(セグメントのリスト) に分割する問題

安全なセグメント $[x_1, x_2,...,x_k]$
$r, r+x_1, r+x_1+x_2,..., r+x_1+x_2+···+x_k$ がすべて $0$ と $C$ の間にある

$C = 100$ のもとで
$r = 20$ を取れば $[−20,40,60,−30]$ は安全であることがわかるが、
$[40,−50,10,80,20]$ は安全ではない。
少なくとも $r=10$ としなければならないが、$10+40−50+10+80+20 = 110$ で $C$ より大きくなってしまう。

-----

## 問題の定義2

安全の条件を簡単にする

$x_1, x_1+x_2,..., x_1+x_2+···+x_k$ のうちの最小値を $n$ 最大値を $m$ としたとき、$n ≤ 0 ≤ m$ となっている。

ここで $0 ≤ r+n ≤ C ⋀ 0 ≤ r+m ≤ C$ となる $r$ が存在する必要がある。
この条件は $m ≤ C+n$ と等価となる(演習問題)。

```haskell
-- 安全なセグメント
-- C をグローバル値 c で与える
safe :: Segment Int → Bool
safe xs = maximum sums ≤ c +minimum sums
          where sums = scanl (+) 0 xs
```

```haskell
-- 最小の安全な partition
msp:: [Int] → Partition Int
msp ← MinWith length · filter (all safe)· parts
```

-----

## 問題の定義3

```haskell
-- foldr での定義
msp ← MinWith length · safeParts

safeParts     = foldr (concatMap ·safeExtendl) [[]]
safeExtendl x = filter (safe · head)· extendl x
-- cons で追加されたシングルトンのセグメントは safe
-- glue で拡張されたセグメントが safe の場合に結果が残る

extendl :: a → Partition a → [Partition a]
extendl x [] =[cons x []]
extendl x p = [cons x p,glue x p]
```

-----

## 貪欲アルゴリズム

セグメント $[4,4,3,-3,5]$ を考える

$C = 10$ とすると
$[[4],[4,3,-3,5]]$ と $[[4,4],[3,-3,5]]$ の2つが最短の安全な partition

$[[4],[4,3,-3,5]]$ は $5$ を加えることで安全な partition $[[5,4],[4,3,-3,5]]$ に拡張できる

$[[4,4],[3,-3,5]]$ は $[5,4,4]$ が安全なセグメントではないので拡張できない

そのままでは最短の安全な partition を維持することができない

しかし、先頭のセグメントを最短にするようにコスト関数を修正するとうまくいく

```
cost p = (length p,length (head p))
```

-----

## 貪欲アルゴリズム2

先頭のセグメントを最短にするようにコスト関数を修正するとうまくいく

コストが最小なら長さも最小になる性質も保たれている

```
cost p = (length p,length (head p))
```

貪欲アルゴリズムの導出

```
  MinWith cost · concatMap (safeExtendl x)
= {distributing MinWith cost}
  MinWith cost · map (MinWith cost ·safeExtendl x)
→ {with add x ← MinWith cost · safeExtendl x}
  MinWith cost · map (add x)
→ {貪欲条件(後述)}
  add x · MinWith cost
```

```haskell
-- 安全な場合は先頭のセグメントを拡張する
add x [] = [[x]]
add x (s:p) = if safe (x:s) then (x:s):p else [x]:s:p
```

-----

## 貪欲条件

同じリストを分割した partition、
p1 および p2 が安全であるときに次の貪欲条件が成立することを確かめる

```haskell
cost p1 ≤ cost p2 ⇒ cost (add x p1) ≤ cost (add x p2)
```

次の 4つの場合に分けて考える。 `p1`、`p2` それぞれに `add` を適用し、`add` の分岐が 2通りなので全部で 4通り。

```haskell
q1 = cons x p1 ; q2 = cons x p2 (12.1)
q1 = cons x p1 ; q2 = glue x p2 (12.2)
q1 = glue x p1 ; q2 = cons x p2 (12.3)
q1 = glue x p1 ; q2 = glue x p2 (12.4)
```

```
add x (s:p) = if safe (x:s) then (x:s):p else [x]:s:p

add x p@(s:_) = if safe (x:s) then glue x p else cons x p
```

-----

## 貪欲条件2

ここからは `length p` を `|p|` と書く

`cost p = (|p|,|head p|)`

```haskell
q1 = cons x p1 ; q2 = cons x p2 (12.1)
q1 = cons x p1 ; q2 = glue x p2 (12.2)
q1 = glue x p1 ; q2 = cons x p2 (12.3)
q1 = glue x p1 ; q2 = glue x p2 (12.4)
```

まず `|p1| < |p2|` とする。

(12.2) 以外の場合は `|q1| < |q2|`。
(12.2) の場合も `|q1| ≤ |q2|` (cons は長さを 1つ伸ばす) かつ `|head q1| ≤ |head q2|` (cons の先頭より glue の先頭の方が長い)。

よって `|p1| < |p2|` のとき `cost q1 ≤ cost q2`。

-----

## 貪欲条件3

```haskell
q1 = cons x p1 ; q2 = cons x p2 (12.1)
q1 = cons x p1 ; q2 = glue x p2 (12.2)
q1 = glue x p1 ; q2 = cons x p2 (12.3)
q1 = glue x p1 ; q2 = glue x p2 (12.4)
```

次に `|p1| = |p2|` かつ `|s1| ≤ |s2|` (`s1 = head p1`, `s2 = head p2`) とする。
`p1` と `p2` は同じリストから生成したものなので `s1` は `s2` の接頭辞になっている。

```
add x (s:p) = if safe (x:s) then (x:s):p else [x]:s:p

add x p@(s:_) = if safe (x:s) then glue x p else cons x p
```

add の定義から考えると (12.2) の場合が発生するには
`not (safe (x:s1)) ⋀ safe (x:s2)` となる必要があるが、`x:s1` は `x:s2` の接頭辞なのでそうはならず、(12.2) の場合は起こらない。

残りの 3つの場合は、自明だが一応確認すると、

(12.1) `|q1| = |q2| ⋀ |head q1| = |head q2| = 1`
(12.3) `|q1| < |q2|`
(12.4) `|q1| = |q2| ⋀ |head q1| = |s1| + 1 ≤ |s2| + 1 = |head q2|`

となり、どの場合も `cont q1 ≤ cost q2` が成立し、貪欲条件が成立することがわかる

-----

## 貪欲アルゴリズム3

結論として次の貪欲アルゴリズムが得られる

```
msp :: [Int] → Partition Int
msp = foldr add []
      where add x [] = [[x]]
            add x (s: p) = if safe (x:s) then (x:s):p else [x]:s:p
```

`safe` の計算コストを定数時間にすれば、これは線形時間アルゴリズムになる

タプリングによって `safe` の計算コストを定数時間にするのは演習問題

-----

## Thining

Thining を考える前に貪欲アルゴリズムが成立するかを確認するのは良い方法

それはともかく Thining もやってみる

```
msp ← MinWith length · ThinBy (≼) · safeParts
```

次を満たすように ≼ を選ぶ必要がある

```
p1 ≼ p2 ⇒ length p1 ≤ length p2
```

次の半順序を ≼ として選ぶ

```
p1 ≼ p2 = length p1 ≤ length p2 ∧ length (head p1) ≤ length (head p2)
```

このとき、次の融合条件が成立する

```
ThinBy (≼) · step x → ThinBy (≼) · step x · ThinBy (≼)  where step x = concatMap (safeExtendl x)
```

次のアルゴリズムは各ステップで thining を行なう。

```
msp = minWith length · foldr tstep [[]]
      where tstep x = thinBy (≼) · concatMap (safeExtendl x)
```

<!--- TODO: thining の融合条件と thining アルゴリズムの関係を調べる -->

thinByの定義から、各段階で高々 2つのパーティションが保持されることを帰納法で証明できる

≼ にもとづく thining アルゴリズムは貪欲アルゴリズムとほぼ同じ効率となる

-----

## 興味深い例

`C = 100` の場合

```
msp [50,20,30,−10,40,−90,−20,60,70,−40,80]
  = [[50],[20,30,−10,40,−90],[−20,60],[70],[−40,80]]
```

次の partition も長さは 5 で解としてもよいはず

```
[[50,20,30,−10],[40,−90],[−20,60],[70,−40],[80]]
```

この例は演習問題12.12で扱う

-----

# 12.3 The paragraph problem

-----

## 段落問題

段落を、すべての行を特定の幅に収めるように整形する

テキストは、空ではない単語の列で構成され、各単語は空ではない空白でない文字の列であると仮定

```
type Text = [Word]
type Word = [Char]
type Para = [Line]
type Line = [Word]
```

```
-- 関数fitsは、ある行が必要な幅に収まるかどうかを判断
para :: Text → Para
para ← MinWith cost · filter (all fits)· parts
```

-----

<!---
 Local Variables:
 indent-tabs-mode: nil
 End:
 -->