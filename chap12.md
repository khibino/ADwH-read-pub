
# 12章 Partitions

@khibino

---

* 12.1 Ways of generating partitions
    - partition の生成方法
* 12.2 Managing two bank accounts
    - 口座の転送回数の最適化
* 12.3 The paragraph problem
    - 段落のテキストの整形

空でないリストの partition: リストを空でない複数のリストに分割すること

12.1 は partition 生成方法について、12.2 と 12.3 はその応用例

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
filter (all ok) (concatMap (extendl x) ps) =
  concatMap (okextendl x) (filter (all ok) ps)

okextendl x = filter (ok · head)· extendl x
extendl x [] =[cons x []]
extendl x p = [cons x p,glue x p]
cons x p = [x]:p
glue x (s:p) = (x:s):p

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

p == []

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

p /= []

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
      | p <- ps
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

<!---
 Local Variables:
 indent-tabs-mode: nil
 End:
 -->
