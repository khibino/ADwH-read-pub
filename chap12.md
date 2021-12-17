
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

---

Answer

リストの要素と要素の間を「分割する」あるいは「分割しない」の2通りが考えられて、要素と要素の間は $n-1$。
よって $2^(n-1)$


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

---

Answer

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

---

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

---

Answer

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

---

Answer

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

安全なセグメント $[x_1, x_2,...,x_k]$ とは
$r, r+x_1, r+x_1+x_2,..., r+x_1+x_2+···+x_k$ がすべて $0$ と $C$ の間にあること

$C = 100$ のもとで
$r = 20$ を取れば $[−20,40,60,−30]$ は安全であることがわかるが、
$[40,−50,10,80,20]$ は安全ではない。
少なくとも $r=10$ としなければならないが、$10+40−50+10+80+20 = 110$ で $C$ より大きくなってしまう。

-----

## 問題の定義2

安全の条件を簡単にする

$0, x_1, x_1+x_2,..., x_1+x_2+···+x_k$ のうちの最小値を $n$ 最大値を $m$ としたとき、$n ≤ 0 ≤ m$ となっている。

ここで $0 ≤ r+n ≤ C ⋀ 0 ≤ r+m ≤ C$ となる $r$ が存在する必要がある。
この条件は $m ≤ C+n$ と等価となる(演習問題)。

```haskell
-- 安全なセグメント
-- C をグローバル値 c で与える
safe :: Segment Int → Bool
safe xs = maximum sums ≤ c + minimum sums
          where sums = scanl (+) 0 xs
```

```haskell
-- 最小の安全な partition
msp:: [Int] → Partition Int
msp ← MinWith length · filter (all safe) · parts
```

-----

## 問題の定義3

```haskell
-- foldr での定義
msp ← MinWith length · safeParts

safeParts     = foldr (concatMap · safeExtendl) [[]]
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
`p₁` および `p₂` が安全であるときに次の貪欲条件が成立することを確かめる

```haskell
cost p₁ ≤ cost p₂ ⇒ cost (add x p₁) ≤ cost (add x p₂)
```

次の 4つの場合に分けて考える。 `p₁`、`p₂` それぞれに `add` を適用し、`add` の分岐が 2通りなので全部で 4通り。

```haskell
q₁ = cons x p₁ ; q₂ = cons x p₂ (12.1)
q₁ = cons x p₁ ; q₂ = glue x p₂ (12.2)
q₁ = glue x p₁ ; q₂ = cons x p₂ (12.3)
q₁ = glue x p₁ ; q₂ = glue x p₂ (12.4)
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
q₁ = cons x p₁ ; q₂ = cons x p₂ (12.1)
q₁ = cons x p₁ ; q₂ = glue x p₂ (12.2)
q₁ = glue x p₁ ; q₂ = cons x p₂ (12.3)
q₁ = glue x p₁ ; q₂ = glue x p₂ (12.4)
```

まず `|p₁| < |p₂|` とする。

(12.2) 以外の場合は `|q₁| < |q₂|`。
(12.2) の場合も `|q₁| ≤ |q₂|` (cons は長さを 1つ伸ばす) かつ `|head q₁| ≤ |head q₂|` (cons の先頭より glue の先頭の方が長い)。

よって `|p₁| < |p₂|` のとき `cost q₁ ≤ cost q₂`。

-----

## 貪欲条件3

```haskell
q₁ = cons x p₁ ; q₂ = cons x p₂ (12.1)
q₁ = cons x p₁ ; q₂ = glue x p₂ (12.2)
q₁ = glue x p₁ ; q₂ = cons x p₂ (12.3)
q₁ = glue x p₁ ; q₂ = glue x p₂ (12.4)
```

次に `|p₁| = |p₂|` かつ `|s1| ≤ |s2|` (`s1 = head p₁`, `s2 = head p₂`) とする。
`p₁` と `p₂` は同じリストから生成したものなので `s1` は `s2` の接頭辞になっている。

```
add x (s:p) = if safe (x:s) then (x:s):p else [x]:s:p

add x p@(s:_) = if safe (x:s) then glue x p else cons x p
```

add の定義から考えると (12.2) の場合が発生するには
`not (safe (x:s1)) ⋀ safe (x:s2)` となる必要があるが、`x:s1` は `x:s2` の接頭辞なのでそうはならず、(12.2) の場合は起こらない。

残りの 3つの場合は、自明だが一応確認すると、

(12.1) `|q₁| = |q₂| ⋀ |head q₁| = |head q₂| = 1`

(12.3) `|q₁| < |q₂|`

(12.4) `|q₁| = |q₂| ⋀ |head q₁| = |s1| + 1 ≤ |s2| + 1 = |head q₂|`

となり、どの場合も `cost q₁ ≤ cost q₂` が成立し、貪欲条件が成立することがわかる

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
p₁ ≼ p₂ ⇒ length p₁ ≤ length p₂
```

次の半順序を ≼ として選ぶ

```
p₁ ≼ p₂ = length p₁ ≤ length p₂ ∧ length (head p₁) ≤ length (head p₂)
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

# 12.2 関連の Exercise

-----

Exercise 12.6

Suppose that n ≤ 0 ≤ m.
Show that  (∃ r : 0 ≤ r + n ≤ C ∧ 0 ≤ r + m ≤ C) ⇔ m ≤ C + n

n ≤ 0 ≤ m とする.
(∃ r : 0 ≤ r + n ≤ C ∧ 0 ≤ r + m ≤ C) ⇔ m ≤ C + n を示せ.

---

Answer

(∃ r : 0 ≤ r + n ≤ C ∧ 0 ≤ r + m ≤ C) ⟹ m ≤ C + n

∃ r : 0 ≤ r + n ≤ C ∧ 0 ≤ r + m ≤ C

⟹ {- 0 ≤ r + n の r を左辺へ -}

∃ r :-r ≤ n ∧ r + m ≤ C

⟹ {- 不等式の両辺を加算する -}

m ≤ C + n  //


(∃ r : 0 ≤ r + n ≤ C ∧ 0 ≤ r + m ≤ C) ⟸ m ≤ C + n

m ≤ C + n

⟹ {- -n ≤ r ≤ C - m となる r を導入 -}

E r : 0 ≤ r + n ≤ r ≤ r + m ≤ C

⟹ {- a ≤ b ≤ c ⟹ a ≤ c -}

E r : 0 ≤ r + n ≤ C ∧ 0 ≤ r + m ≤ C  //


-----

Exercise 12.7

Show that the predicate safe in the bank accounts problem is both prefix-closed and suffix-closed.

銀行口座の問題での述語 `safe` が接頭辞で閉じているかつ接尾辞で閉じているをこと示せ.

---

Answer


```haskell
safe :: Segment Int → Bool
safe xs = maximum sums ≤ c + minimum sums
          where sums = scanl (+) 0 xs
```

sums' = scanl (+) 0 とする


接頭辞で閉じている

safe (xs++ys) ⟹ safe xs

safe (xs++ys)

⟹ {- safe の定義 -}

(maximum . sums') (xs++ys) ≤ c + (minimum . sums') (xs++ys)

⟹ {- (maximum . sums') xs ≤ (maximum . sums') (xs++ys) -}

(maximum . sums') xs ≤ c + minimum (xs++ys)

⟹ {- (minimum . sums') (xs++ys) ≤ (minimum . sums') xs -}

(maximum . sums') xs ≤ c + (minimum . sums') xs

⟹ {- safe の定義 -}

safe xs


接尾辞で閉じている

safe (xs++ys) ⟹ safe ys

safe (xs++ys)

⟹ {- safe の定義 -}

(maximum . sums') (xs++ys) ≤ c + (minimum . sums') (xs++ys)

⟹ {- (maximum . sums') ys ≤ (maximum . sums') (xs++ys) -}

(maximum . sums') ys ≤ c + minimum (xs++ys)

⟹ {- (minimum . sums') (xs++ys) ≤ (minimum . sums') ys -}

(maximum . sums') ys ≤ c + (minimum . sums') ys

⟹ {- safe の定義 -}

safe ys

-----

Exercise 12.8

Suppose C = 10.
What is the value of msp [2,4,50,3] when msp is the greedy algorithm for the bank accounts problem and when msp is defined by the original specification?

C = 10 とする.
msp が銀行口座の問題の貪欲アルゴリズムで定義されているときの msp [2,4,50,3] の値は何か? また元の仕様で定義されているときの値は何か?

---

Answer

貪欲アルゴリズムの場合

```
safe128 :: Segment Int -> Bool
safe128 xs = maximum sums <= 10  + minimum sums
  where sums = scanl (+) 0 xs


msp128 :: [Int] -> Partition Int
msp128 = foldr add []
  where
    add :: Int -> Partition Int -> Partition Int
    add x [] = [[x]]
    add x (s:p) = if safe128 (x:s) then (x:s):p else [x]:s:p
```

[[2,4],[50],[3]]


元の仕様の場合

```haskell
msp:: [Int] → Partition Int
msp ← MinWith length · filter (all safe) · parts
```

`50` を含むセグメントが `C = 10` を越えてしまうため、解無し

-----

Exercise 12.9

The function `add` in the bank accounts problem does not take constant time because the safety test can take linear time.
But we can represent a partition p by a triple

```
(p,minimum (sums (head p)),maximum (sums (head p)))
```

where `sums = scanl (+) 0.`

Write down a new definition of msp that does take linear time.


安全かどうかの検証が線形時間をとるため、銀行口座の問題における `add` 関数は定数時間をとらない.

しかし
`sums = scanl (+) 0.` のもとで

三つ組によってパーティション p を表現することができる.

```
(p,minimum (sums (head p)),maximum (sums (head p)))
```

線形時間をとる msp の新たな定義を書き下せ.


---

Answer

```
type TrPartition a = (Partition a, a, a)

msp129 :: [Int] -> Partition Int
msp129 = fst3 . foldr add ([], 0, 0)
  where
    fst3 (x, _, _) = x
    c129 :: Int
    c129 = 100
    add :: Int -> TrPartition Int -> TrPartition Int
    add x ([], _, _) = ([[x]], min 0 x, max 0 x)
    add x (s:p, mn, mx) =  if mx2 <= c129 + mn2 then ((x:s):p, mn2, mx2) else ([x]:s:p, min 0 x, max 0 x)
      where
        mn2 = min 0 (mn + x)
        mx2 = max 0 (mx + x)
```

```
scanl (+) 0 (x:s) =
0 : map (x +) (scanl (+) 0 s)

minimum $ scanl (+) 0 (x:s) =
minimum $ 0 : map (x +) (scanl (+) 0 s) =
min 0 ( x + minimum (scanl (+) 0 s) )

maximum $ scanl (+) 0 (x:s) =
maximum $ 0 : map (x +) (scanl (+) 0 s) =
max 0 ( x + maximum (scanl (+) 0 s) )
```

-----

Exercise 12.10

The function msp returns a partition, not the transfers that have to be made to keep the current account in balance.
Show how to define

```
transfers :: Partition Int → [Int]
```

by computing a pair `(n,r)` of nonnegative numbers for each segment, where n is the minimum that has to be in the current account to ensure the segment is safe and r is the residue after the transactions in the segment.

関数 `msp` はパーティションを返すが当座預金の収支を保つための資金移動の情報を返さない.


それぞれのセグメントについての負でない数の対 `(n,r)` を計算することで

```
transfers :: Partition Int → [Int]
```

を定義する方法を示せ.
ここで
`n` はセグメントを安全にするための当座預金の最小値で、
`r` はセグメントの取引後の残りである.

---

Answer

```
transfers :: Partition Int -> [Int]
transfers p = zipWith transfer (ns ++ [0]) (0 : rs) -- 最初は残りが 0, 最後は残しておく必要が 0
  where
    transfer n pr = n - pr
    (ns, rs) = unzip $ map transaction p
    -- (n, r) を計算
    transaction :: Segment Int -> (Int, Int)
    transaction xs = (n, last sums + n)
      where
        n = - minimum sums
        -- min 0 sums と書くところだが、scanl の結果の先頭は 0 なので省いている
        sums = scanl (+) 0 xs
```

-----

Exercise 12.11

Consider the thinning algorithm for the bank accounts problem.
Suppose that at some point in the computation there are two partitions of the form `[y]:ys:p` and `(y:ys):p`.
This could happen as early as the second step, producing the  partitions `[[y],[z]]` and `[[y,z]]`.
Show that adding in a new element `x` and thinning  the result will produce either a single partition, or two partitions of the above form.

銀行口座の問題の thinning アルゴリズムを考えてみよう.
計算のある時点で、`[y]:ys:p`と`(y:ys):p`という形の2つのパーティションがあったとする.
これは早ければ第2段階で起こる可能性があり、パーティション `[[y],[z]]` と `[[y,z]]` が生成される.
新しい要素 `x` を追加してその結果を間引くと、1つのパーティションか、上の形式の2つのパーティションのどちらかになることを示せ.

---

Answer

```
p₁ ≼ p₂ = length p₁ ≤ length p₂ ∧ length (head p₁) ≤ length (head p₂)
```


```
[[x]:[y]:ys:p, [x,y]:ys:p, [x]:(y: ys):p, (x: y: ys):p]
[[x]:[y]:ys:p, [x,y]:ys:p, [x]:(y: ys):p]
[[x]:[y]:ys:p, [x]:(y: ys):p]
```



```
[x]:(y: ys):p ≼ [x] :[y]:ys:p
[x]:(y: ys):p ≼ [x,y]:ys:p
```



```
[[x]:(y:ys):p, (x:y:ys):p]
[[x]:(y:ys):p]
[[x]:(y:ys):p]
```

-----

Exercise 12.12

How can Zakia address the suspicious feature of the given solution to the bank accounts problem, namely that transfers can occur before they are absolutely necessary?

銀行口座の問題に対する提示された解決策では「必要になるまえに送金が行なわれる」という疑わしい性質についてどのようにザキアは対処できるか?

---

Answer

```
mspLR :: [Int] -> Partition Int
mspLR = foldl add []
  where
    add :: Partition Int -> Int -> Partition Int
    add [] x = [[x]]
    add p  x = head (filter (safe . last) [bind x p, snoc x p])
```

-----

Exercise 12.13

The function runs used in Mergesort is specified by

```
runs::Ord a ⇒ [a] → Partition a
runs ← MinWith length · filter (all ordered) · parts
```

Without looking back to the section on Mergesort, write down a greedy algorithm for computing runs.
Why does the greedy algorithm work?


マージソートで使用される関数 `runs` は次で記述される.

```
runs::Ord a ⇒ [a] → Partition a
runs ← MinWith length · filter (all ordered) · parts
```

マージソートの節を振り返らず、`runs` を計算する貪欲アルゴリズムを書き下せ.
この貪欲アルゴリズムはなぜ動作するか?

---

Answer


```
ordered :: Ord a => [a] -> Bool
ordered xs = all asc $ zip xs (tail xs)  where asc (x, y) = x <= y

runs :: Ord a => [a] -> Partition a
runs = foldr add []
  where
    add x [] = [[x]]
    add x (s:p) = if x <= head s then (x:s):p else [x]:s:p
```

ordered が保たれているセグメント s に対して x が先頭の要素以下のときだけ x を先頭に加えれば x:s も orderd が保たれる。
そうでないときは s を伸ばすことはできないため。

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
para :: Text → Para
para ← MinWith cost · filter (all fits)· parts

-- 関数fitsは、ある行が必要な幅に収まるかどうかを判断
fits :: Line → Bool
fits line = width line ≤ maxWidth  -- maxWidthはグローバル値
width :: Line → Nat
width = foldrn add length  where add w n = length w+1+n
```

関数foldrn（空ではないリストに対する一般的な畳み込み）の定義は第8章

-----

## 段落問題2

foldr,  foldl どちらを採用するか?

fits は接頭辞においても接尾辞においても閉じているので、その点ではどちらでもよい

foldr で考えると、先頭の行を短かくして後続の行を長くするように cost が定義されるが、それは不自然なので foldl を採用する


各ステップでは、行が最大幅に収まる partition のみが生成される

```
para ← MinWith cost · fitParts

fitParts = foldl (flip (concatMap · fitExtend)) [[]]
                 where fitExtend x = filter (fits · last) · extendr x
```

-----

## コスト関数

5種類のコスト関数が考えられる

段落の行数が最も少なくなる

```
cost₁ = length
```

最終行以外の行の、最大幅に満たない数の合計

```
cost₂ = sum · map waste · init  where waste line = maxWidth - width line
```

最終行以外の各行の optWidth との差の2乗の合計

```
cost₃ = sum·map waste · init  where waste line = (optWidth - width line)²
```

最終行以外の行の、最大幅に満たない数のうちの最大

```
cost₄ = foldr max 0 · map waste · init  where waste line = maxWidth - width line
```

最終行以外の各行の optWidth との差の2乗のうちの最大

```
cost₅ = foldr max 0 · map waste · init  where waste line = (optWidth - width line)²
```

-----

## 貪欲アルゴリズム

自明な貪欲アルゴリズム

段落の最終行の末尾に単語が入らなくなるまで追加していき、入らなくなったら新しい行を開始する

```
greedy = foldl add []
  where add [] w = snoc w []
        add p w = head (filter (fits · last) [bind w p,snoc w p])
```

cost がどのような定義だったら貪欲アルゴリズムが有効か?

次の2つの条件が必要

```
-- fits が結果を通すなら、単語を行末に追加する方は新しい行を開始するより悪くはならない
fits (last (bind w p)) ⇒ cost (bind w p) ≤ cost (snoc w p)
-- 貪欲条件
cost p₁ ≤ cost p₂ ⇒ cost (add p₁ w) ≤ cost (add p₂ w)
```

cost が単に行数の場合は貪欲条件は成立しない(演習問題)

cost₁ を次のように再定義すると貪欲条件が成立する

```
-- 行数が最小で最後の行が最も短い
cost₁ p = (length p, width (last p))
```

-----

## 貪欲条件の証明

bank account の問題と似た流れで証明する

`q₁ = add p₁ w, q₂ = add p₂ w` とすると次の4つの場合が考えられる

```
q₁ = bind w p₁ ; q₂ = bind w p₂ (12.5)
q₁ = bind w p₁ ; q₂ = snoc w p₂ (12.6)
q₁ = snoc w p₁ ; q₂ = bind w p₂ (12.7)
q₁ = snoc w p₁ ; q₂ = snoc w p₂ (12.8)
```

`cost₁ p₁ ≤ cost₁ p₂` を前提とし、ここでも `length p` を `|p|` と書くことにする.

まず `|p₁| < |p₂|` のとき

(12.7) の場合を除いて `|q₁| < |q₂|` となる.

(1.27) の場合は

```
|q₁| ≤ |q₂| ∧ width (last q1) < width (last q2)
```

となる.

よってどの場合も `cost₁ q₁ < cost₁ q₂`.

## 貪欲条件の証明2

```
q₁ = bind w p₁ ; q₂ = bind w p₂ (12.5)
q₁ = bind w p₁ ; q₂ = snoc w p₂ (12.6)
q₁ = snoc w p₁ ; q₂ = bind w p₂ (12.7)
q₁ = snoc w p₁ ; q₂ = snoc w p₂ (12.8)
```

```
add [] w = snoc w []
add p w = head (filter (fits · last) [bind w p,snoc w p])
```

次に `|p₁| = |p₂| ∧ width (last p₁) ≤ width (last p₂)` のとき

add の定義と `width (last p₁) ≤ width (last p₂)` から

```
width (last (bind w p₁)) = width (last p₁ ++ [w]) =
width (last p₁) + 1 + width ([w]) ≤ width (last p₂) + 1 + width ([w]) =
width (last p₂ ++ [w]) = width (last (bind w p₂))
```

つまり `width (last (bind w p₁)) ≤ width (last (bind w p₂))` となって、
`not (fits (last (bind w p₁))) ∧ fits (last (bind w p₂))` は成立せず、(1.27) の場合は起こらないことがわかる。

(12.5) と (12.8) では `|q₁| = |q₂| ∧ width (last q₁) = width (last q₂)`

(12.6) では `|q₁| < |q₂|`

なのですべての場合で `cost q₁ ≤ cost q₂` となる.

よって、貪欲アルゴリズムは、1つの段落の行数を最小にすることがわかる.

-----

## 貪欲アルゴリズム2

貪欲アルゴリズムは、cost₂ に対しても有効

次を主張する

```
cost₁ p₁ ≤ cost₁ p₂ ⇒ cost₂ p₁ ≤ cost₂ p₂
```

`p₁` が `[l₁₁, l₁₂,...,l₁ₖ]` で構成され、 `w₁ⱼ` は `l₁ⱼ` の幅、 `M` は `maxWidth` として証明をすすめる

cost₂ p₁ は次のようになる

```
cost₂ p₁ = (M − w₁₁)+(M − w₁₂) +···+ (M − w₁₍ₖ₋₁₎)
         = (k − 1) M − (T − (w₁ₖ + k − 1))
```

`T` はテキストの幅の合計で `(k - 1)` 個の空白または改行を考えれば `T − (w₁ₖ + k − 1)` は最後の行を除くすべての行の幅の合計

`p₂` が `[l₂₁, l₂₂,...,l₂ₘ]` で構成されているとすると

```
cost₂ p₂ = (m − 1) M − (T − (w₂ₘ + m − 1))
```

前提 `cost₁ p₁ ≤ cost₁ p₂` から `(k, w₁ₖ) ≤ (m, w₂ₘ)`

さらに `k < m` のとき

```
cost₂ p₂ = (m − 1) M − (T − (w₂ₘ + m − 1))
         ≥ ((k + 1) − 1) M − (T − (w₂ₘ + (k + 1) − 1))
         = (k − 1) M − (T − (w₂ₘ + k − 1)) + M + 1
         = (k − 1) M − (T − (w₁ₘ + k − 1)) + M + 1 + w₂ₘ − w₁ₖ
         = cost₂ p₁ + M + 1 + w₂ₘ − w₁ₖ
         > cost₂ p₁ + M + w₂ₘ − w₁ₖ
         > cost₂ p₁    -- w₁ₖ < M なので

cost₂ p₂ ≥ cost₂ p₁ + M + w₂ₘ − w₁ₖ > cost₂ p₁
```

一方 `k = m ∧ w₁ₖ ≤ w₂ₘ` のとき

```
cost₂ p₂ = cost₂ p₁ + w₂ₘ − w₁ₖ ≥ cost₂ p₁
```

となり、いずれの場合も `cost₂ p₁ ≤ cost₂ p₂` が成立.

-----

## 貪欲アルゴリズム3

`cost₃`, `cost₄`, `cost₅` の定義では貪欲アルゴリズムは機能しない

`maxWidth = 10`, `optWidth = 8` として次の2つの partition を考える

```
-- length wᵢ = i
-- p₁ は貪欲アルゴリズムで得られるもの
p₁ = [[w₆,w₁],[w₅,w₃],[w₄],[w₇]]
p₂ = [[w₆],[w₁,w₅],[w₃,w₄],[w₇]]
```

これらのすべての cost で `p₂` は `p₁` より良い partition となっている

```
cost₃ p₁ = sum [(8−8)²,(8−9)²,(8−4)²] = 17
cost₃ p₂ = sum [(8−6)²,(8−7)²,(8−8)²] = 5

cost₄ p₁ = maximum [10−8,10−9,10−4] = 6
cost₄ p₂ = maximum [10−6,10−7,10−8] = 4

cost₅ p₁ = maximum [8−8,8−9,8−4] = 4
cost₅ p₂ = maximum [8−6,8−7,8−8] = 2
```

貪欲アルゴリズムでは最良の解を得ることができない

特定のコスト関数に対する thinning アルゴリズムが必要

-----

## Thinning

admissible cost function

```
cost p₁ ≤ cost p₂ ∧ width (last p₁) = width (last p₂)
```

が成立するとき次が成立する

```
cost (bind w p₁) ≤ cost (bind w p₂) ∧ cost (snoc w p₁) ≤ cost (snoc w p₂)
```

このような cost を admissible cost function と言う

部分段落 `p₁` および `p₂` についてこの条件が満たされているとする

このとき段落の残り `l₀, l₁, ··· ,lₖ` に対して

```
q₂ = init p₂ ++ [last p₂ ++ l₀] ++ [l₁] ++···++ [lₖ]

q₁ = init p₁ ++ [last p₁ ++ l₀] ++ [l₁] ++···++ [lₖ]
```

(admissible の前提が満たされているいことに注意しつつ単語の追加を繰り返し適用する. `l₀` は最初の `bind` の繰り返し、`l₁, ··· ,lₖ` はそれ以降の単語追加の繰り返しに対応する.)

を考えると `cost q₁ ≤ cost q₂` となり、
部分段落 `p₂` は `p₁` よりもよい解を導くことはない

(admissible の条件を考慮しつつ単語の追加を繰り返し適用する)

よって thinning のための ≼ として次を考える

```
p1 ≼ p2 = cost p1 ≤ cost p2 ∧ width (last p1) == width (last p2)
```

-----

## Thinning 2

```
p1 ≼ p2 = cost p1 ≤ cost p2 ∧ width (last p1) == width (last p2)
```

さらにパーティションのリスト `ps` を最後の行の幅が増加する順に保つようにすることを考えると

`map (bind w) ps` も最後の行が増加する順となる

`map (snoc w) ps` はすべて同じ最終行を持ち、その最終行が可能な限り短いものとなる.
このリストを thinning すると `minWith cost (map (snoc w) ps)` だけが残る

```
para = minWith cost · foldl tstep [[]]
  where tstep [[]] w = [[[w]]]
        tstep ps w = minWith cost (map (snoc w) ps) :
                     filter (fits · last) (map (bind w) ps)
```

各ステップ最大でもで M = maxWidth のパーティションが維持される

tstepが Ο(`M`)ステップで済むように cost と幅をメモ化する、snoc と bind を効率化するのは練習問題

結果として `n` 個の単語の段落問題は Ο(`M n`) ステップかかる

-----

# 12.3 関連の Exercise

-----

Exercise 12.14

Show that the greedy condition fails when the cost of a paragraph is simply the number of lines.

---

Answer

```
-- 貪欲条件
cost p₁ ≤ cost p₂ ⇒ cost (add p₁ w) ≤ cost (add p₂ w)

add [] w = snoc w []
add p w = head (filter (fits · last) [bind w p,snoc w p])
```

```
maxWidth = 7
p₁ = [["This"], ["is", "a"]]
p₂ = [["This", "is"], ["a"]]
```

のもとで `cost p₁ ≤ cost p₂` が成立。

`w = "pen"` とすると

```
add p₁ w = [["This"], ["is", "a"], ["pen"]]
add p₂ w = [["This", "is"], ["a", "pen"]]

cost (add p₁ w) = 3
cost (add p₂ w) = 2
```

なので `cost (add p₁ w) ≤ cost (add p₂ w)` が成立しない。

-----

Exercise 12.15

The greedy algorithm for the paragraph problem can be made more efficient in two steps.
This exercise deals with the first step and the following exercise with the second step.
Consider the function help specified by

```
p ++ help l ws = foldl add (p ++ [l]) ws
```

Prove that

```
greedy (w:ws) = help [w] ws
  where help l [] = [l]
        help l (w:ws) = if width l' ≤ maxWidth
                        then help l' ws  else l : help [w] ws
                        where l' = l ++ [w]
```

---

Answer

```
greedy = foldl add []
  where add [] w = snoc w []
        add p w = head (filter (fits · last) [bind w p,snoc w p])

fits :: Line → Bool
fits line = width line ≤ maxWidth  -- maxWidthはグローバル値
width :: Line → Nat
width = foldrn add length  where add w n = length w+1+n
```

`p ++ help l ws = foldl add (p ++ [l]) ws ⇒ help [w] ws == foldl add [] (w:ws)` と
`p ++ help l ws = foldl add (p ++ [l]) ws` を証明する。


```
p ++ help l ws = foldl add (p ++ [l]) ws ⇒ help [w] ws == foldl add [] (w:ws)
```

の証明

```
p ++ help l ws = foldl add (p ++ [l]) ws ⇒
  {- p = [], l = [w] -}
help [w] ws = foldl add ([[w]]) ws ⇒
  {- add [] w = snoc w [] = [[w]] -}
help [w] ws = foldl add (add [] w) ws ⇒
  {- foldl add [] (w:ws) = foldl add (add [] w) ws -}
help [w] ws = foldl add [] (w:ws)
```

```
p ++ help l ws = foldl add (p ++ [l]) ws
```

の証明

`ws` についての帰納法で証明する

`[]` のとき

```
p ++ help l [] =
  {- help の定義 -}
p ++ [l] =
  {- foldl の定義 -}
foldl add (p ++ [l]) []
```

で成立。


`w:ws` かつ `width (l ++ [w]) ≤ maxWidth` のとき

```
p ++ help l (w:ws) =
  {- help の定義 -}
p ++ help (l ++ [w]) ws =
  {- 帰納法の仮定 -}
foldl add (p ++ [l ++ [w]]) ws =
  {- width (l ++ [w]) ≤ maxWidth なので add (p ++ [l]) w = p ++ [l ++ [w]] -}
foldl add (add (p ++ [l]) w) ws =
  {- foldl の定義 -}
foldl add (p ++ [l]) (w:ws)
```

で成立。


`w:ws` かつ `width (l ++ [w]) > maxWidth` のとき

```
p ++ help l (w:ws) =
  {- help の定義 -}
p ++ (l : help [w] ws) =
  {- ++ 定義, 結合則 -}
(p ++ [l]) ++ help [w] ws =
  {- 帰納法の仮定 -}
foldl add ((p ++ [l]) ++ [[w]]) ws =
  {- width (l ++ [w]) > maxWidth なので add (p ++ [l]) w = (p ++ [l]) ++ [[w]] -}
foldl add (add (p ++ [l]) w) ws =
  {- foldl の定義 -}
foldl add (p ++ [l]) (w:ws)
```

で成立。

証明終わり //

-----

Exercise 12.16

For the second step, memoise width and eliminate the concatenation with the help of an accumulating function parameter.

---

Answer

```
help l [] = [l]
help l (w:ws) = if width l' ≤ maxWidth
                then help l' ws  else l : help [w] ws
                where l' = l ++ [w]
```

```
help ( _, l) [] = [l []]
help (sz, l) (w:ws) = if sz2 <= maxWidth
                      then help (sz2, l . (w:)) ws  else l [] : help (wlen, (w:)) ws
                      where wlen = length w
                            sz2 = sz + 1 + wlen
```

-----

Exercise 12.17

In the thinning version of the paragraph problem, can we replace filter by takeWhile?

---

Answer

```
para = minWith cost · foldl tstep [[]]
  where tstep [[]] w = [[[w]]]
        tstep ps w = minWith cost (map (snoc w) ps) :
                     filter (fits · last) (map (bind w) ps)
```

`map (bind w) ps` は最後の行が増加する順となっているので filter を takeWhile に置き換えても良い

-----

Exercise 12.18

Show that the cost functions described in the text for the paragraph problem are all admissible.

---

Answer

```haskell
-- admissible
cost p₁ ≤ cost p₂ ∧ width (last p₁) = width (last p₂)
⇒
cost (bind w p₁) ≤ cost (bind w p₂) ∧ cost (snoc w p₁) ≤ cost (snoc w p₂)
```

```haskell
cost₁ = length なので
length p₁ ≤ length p₂ ∧ width (last p₁) = width (last p₂) のもとで

cost₁ (bind w p₁) =
  {- bind は行数を変えない -}
length p₁ ≤ length p₂ =
  {- bind は行数を変えない -}
cost₁ (bind w p₂)

cost₁ (snoc w p₁) =
  {- snoc は行数を1増やす -}
length p₁ + 1 ≤ length p₂ + 1 =
  {- snoc は行数を1増やす -}
cost₁ (snoc w p₂)
```

```haskell
-- 最終行以外の行の、最大幅に満たない数の合計
cost₂ = sum · map waste · init  where waste line = maxWidth - width line

cost₂ p₁ ≤ cost₂ p₂ ∧ width (last p₁) = width (last p₂) のもとで

  {- 最終行が変化しても cost₂ は保存される -}
cost₂ (bind w p₁) = cost₂ p₁ ≤ cost₂ p₂ = cost₂ (bind w p₂)

cost₂ (snoc w p₁) =
  {- cost₂ の差分は p₁ の最終行の分 -}
cost₂ p₁ + maxWidth - width (last p₁) ≤
  {- cost₂ p₁ ≤ cost₂ p₂ -}
cost₂ p₂ + maxWidth - width (last p₁) =
  {- width (last p₁) = width (last p₂) -}
cost₂ p₂ + maxWidth - width (last p₂) =
  {- cost₂ の差分は p₂ の最終行の分 -}
cost₂ (snoc w p₂)
```

```haskell
-- 最終行以外の各行の optWidth との差の2乗の合計
cost₃ = sum·map waste · init  where waste line = (optWidth - width line)²

cost₃ p₁ ≤ cost₃ p₂ ∧ width (last p₁) = width (last p₂) のもとで

  {- 最終行が変化しても cost₃ は保存される -}
cost₃ (bind w p₁) = cost₃ p₁ ≤ cost₃ p₂ = cost₃ (bind w p₂)

cost₃ (snoc w p₁) =
  {- cost₃ の差分は p₁ の最終行の分 -}
cost₃ p₁ + (optWidth - width (last p₁))² ≤
  {- cost₃ p₁ ≤ cost₃ p₂ -}
cost₃ p₂ + (optWidth - width (last p₁))² =
  {- width (last p₁) = width (last p₂) -}
cost₃ p₂ + (optWidth - width (last p₂))² =
  {- cost₃ の差分は p₂ の最終行の分 -}
cost₃ (snoc w p₂)
```

```haskell
-- 最終行以外の行の、最大幅に満たない数のうちの最大
cost₄ = foldr max 0 · map waste · init  where waste line = maxWidth - width line

cost₄ p₁ ≤ cost₄ p₂ ∧ width (last p₁) = width (last p₂) のもとで

  {- 最終行が変化しても cost₄ は保存される -}
cost₄ (bind w p₁) = cost₄ p₁ ≤ cost₄ p₂ = cost₄ (bind w p₂)

cost₄ (snoc w p₁) =
  {- p₁ の最終行以外のうちの最大と p₁ の最終行との大きい方 -}
max (cost₄ p₁) (maxWidth - width (last p₁)) ≤
  {- cost₄ p₁ ≤ cost₄ p₂ -}
max (cost₄ p₂) (maxWidth - width (last p₁)) =
  {- width (last p₁) = width (last p₂) -}
max (cost₄ p₂) (maxWidth - width (last p₂)) =
  {- p₂ の最終行以外のうちの最大と p₂ の最終行との大きい方 -}
cost₄ (snoc w p₂)
```

```haskell
-- 最終行以外の各行の optWidth との差の2乗のうちの最大
cost₅ = foldr max 0 · map waste · init  where waste line = (optWidth - width line)²

  {- 最終行が変化しても cost₅ は保存される -}
cost₅ (bind w p₁) = cost₅ p₁ ≤ cost₅ p₂ = cost₅ (bind w p₂)

cost₅ (snoc w p₁) =
  {- p₁ の最終行以外のうちの最大と p₁ の最終行との大きい方 -}
max (cost₅ p₁) (optWidth - width (last p₁))² ≤
  {- cost₅ p₁ ≤ cost₅ p₂ -}
max (cost₅ p₂) (optWidth - width (last p₁))² =
  {- width (last p₁) = width (last p₂) -}
max (cost₅ p₂) (optWidth - width (last p₂))² =
  {- p₂ の最終行以外のうちの最大と p₂ の最終行との大きい方 -}
cost₅ (snoc w p₂)
```

-----

Exercise 12.19

With some admissible cost functions, the thinning algorithm may select a paragraph with minimum cost but whose length is not as short as possible.
How can this deficiency be overcome?

---

Answer

-----

Exercise 12.20

The refinement

```
snoc w · MinWith cost ← MinWith cost · map (snoc w)
```

follows from the condition

```
cost p₁ ≤ cost p₂ ⇒ cost (snoc w p₁) ≤ cost (snoc w p₂)
```

Does this condition hold for `cost₃`?

---

Answer

```
optWidth = 6
p₁ = [["This", "is"], ["a"]]
p₂ = [["This"], ["is", "a"]]
w = "pen"

cost₃ p₁ = 1
cost₃ p₂ = 4

cost₃ (snoc w p₁) = 26
cost₃ (snoc w p₂) = 8
```

で成立しない。


-----

Exercise 12.21

Suppose we had gone for a right-to-left thinning algorithm for the  paragraph problem, using a definition of parts based on foldr.
This time a cost  function is admissible if

```
cost (glue w p₁) ≤ cost (glue w p₂) ∧ cost (cons w p₁) ≤ cost (cons w p₂)
```

provided that

```
cost p₁ ≤ cost p₂ ∧ width (head p₁) = width (head p₂)
```

As can be checked, all five cost functions introduced in the text are admissible in  this sense.
Write down the associated thinning algorithm.
Give an example to show  that the two different thinning algorithms produce different results for `cost₃`.

---

Answer

-----

Exercise 12.22

The final exercise is to make the thinning algorithm for the para graph problem more efficient.
Setting `rmr = reverse · map reverse`, we can represent  a paragraph p by a triple

```
(rmr p, cost p,width (last p))
```

The last two components memoise cost and width, while the first component means  that snoc and bind can be implemented in terms of cons and glue.
More precisely, we have

```
snoc w · rmr = rmr · cons w
bind w · rmr = rmr · glue w
```

Write down the resulting algorithm, assuming the cost function is `cost₃`.

---

Answer


<!---
 Local Variables:
 indent-tabs-mode: nil
 End:
 -->
