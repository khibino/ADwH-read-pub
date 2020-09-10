---
marp: true
---
<!-- theme: gaia -->
<!-- template: invert -->
<!-- size: 4:3 -->
<!-- page_number: true -->
<!-- paginate: true -->
<!-- headingDivider: 3 -->
<!--
style: |
    h1, h2, h3, h4, h5, header, footer {
	    color: white;
	}
	section {
	    background-color: black;
        color: white;
	}
 -->

# 3章 Useful data structures

@khibino

---

-普通のリストで効率的に実装できるものも多いが
-ときどき別の構造が必要

* symmetric lists
* random-access lists
* arrays

リストの基本的な操作の実行時間における欠陥を
それぞれの方法で克服


## 3.1 Symmetric lists


### Symmetric lists/導入

```haskell
type SymList a = ([a], [a])

-- symmetric list `(xs,ys)` は
-- 普通のリスト `xs ++ reverse ys` を表現
```

```haskell
fromSL :: SymList a -> [a]
fromSL (xs, ys) = xs ++ reverse ys

-- fromSL は symmetric list を抽象的な表現へと戻す

-- abstract function というらしい
```


### Symmetric lists/operations

```
consSL, snocSL, headSL, lastSL, tailSL, initSL
```

```
cons x . fromSL = fromSL . consSL x
snoc x . fromSL = fromSL . snocSL x
tail . fromSL = fromSL . tailSL
init . fromSL = fromSL . initSL
head . fromSL = headSL
last . fromSL = lastSL
```


### Symmetric lists/不変条件

```
null xs ⇒ null ys \/ single ys
null ys ⇒ null xs \/ single xs

どちらかが空のリストならば
もう一方は空のリストか singleton リスト
```

```haskell
snocSL :: a -> SymList a -> SymList a
snocSL x (xs,ys) = if null xs then (ys,[x]) else (xs,x:ys)
```

```
[] ++ reverse [] ++ [x] = [] ++ reverse [x]
[] ++ reverse [y] ++ [x] = [y] ++ reverse [x]
```


### Symmetric lists/不変条件II

```
null xs ⇒ null ys \/ single ys
null ys ⇒ null xs \/ single xs
```

```
lastSL :: SymList a -> a
lastSL (xs,ys) = if null ys then head xs else head ys
```

```haskell
-- わかりにくいエラーを直す
-- error の代わりに ⊥ を利用して簡単にしてもよい

lastSL :: ([p], [p]) -> p
lastSL (xs,ys) = if null ys
                 then if null xs
                      then error "lastSL of empty list"
                      else head xs
                 else head ys
```

`consSL` と `headSL` は演習問題


### Symmetric lists/不変条件III

```
null xs ⇒ null ys \/ single ys
null ys ⇒ null xs \/ single xs
```

```haskell
tailSL :: SymList a -> SymList a
tailSL (xs,ys)
  | null xs    = if null ys then undefined else nilSL
  | single xs  = (reverse vs, us)
  | otherwise  = (tail xs, ys)
  where (us,vs) = splitAt (length ys `div` 2) ys
```

```
[] ++ reverse (us ++ vs) = reverse vs ++ reverse us
```

`initSL` は演習問題


### Symmetric lists/償却計算量

* consSL, snocSL, headSL, lastSL は定数時間
* tailSL, initSL は最悪のケースで線形時間
    - 償却定数時間

### Symmetric lists/償却計算量II

```
C(x_i) <= S(x_i) - S(x_{i+1}) + A(x_i)

S(x_i) = abs (length xs_i - length ys_i)    (3.1)
A(x_i) = 2
```

```haskell
abs = if n >= 0 then n else -n
```

```
headSL,lastSL -- S は増減無し
consSL,snocSL -- S の増減は 1
tailSL,initSL -- 特殊ケース以外で S の増減は 1
```


### Symmetric lists/償却計算量III

```
C(x_i) <= S(x_i) - S(x_{i+1}) + A(x_i)

S(x_i) = abs (length xs_i - length ys_i)    (3.1)
A(x_i) = 2
```

```
tailSL,initSL -- 特殊ケース
xs あるいは ys のどちらかが singleton でもう一方が長さ k
操作前に S は k-1 の値を持ち、後ではたかだか 1
k <= k - 1 - 1 + 2
```

symmetric list の追加の操作は演習問題

### Symmetric lists/応用

```
inits :: [a] -> [[a]]
inits []      = [[]]
inits (x:xs)  = [] : map (x:) (inits xs)

-- length . inits の計算量は長さの 2乗
```

```
inits = map reverse . reverse . tails . reverse

-- length . inits の計算量は線形時間になる
-- inits が無限リストに対して使用できない
```

### Symmetric lists/応用II

```
inits :: [a] -> [[a]]
inits = map fromSL . scanl (flip snocSL) nilSL

-- length . inits の計算量は線形時間になる
```

symmetric リストを利用しない `inits` の別の定義は演習問題

`Data.Sequence` の Finger-Tree が名前だけ紹介されていた


### Symmetric lists/Exercise 3.3

`consSL`, `headSL`

### Symmetric lists/Exercise 3.4

`initSL`

### Symmetric lists/Exercise 3.5

`dropWhileSL`

```
dropWhile f . fromSL = fromSL . dropWhileSL f
```

### Symmetric lists/Exercise 3.7

オンラインアルゴリズムの `inits`
`length . inits` が線形時間


## 3.2 Random-access lists

### Random-access lists/導入

```
fetch :: Nat -> [a] -> a
fetch k xs = if k == 0 then head xs else fetch (k - 1) (tail xs)

-- 通常のリストの k 番目の取り出し
-- θ(k) ステップ
```

```
cons, head, tail, fetch

-- 長さ n の Random-access list に対して O(log n) ステップ
```

### Random-access lists/導入II

```
data Tree a = Leaf a | Node (Tree a) (Tree a)

size :: Num p => Tree a -> p
size (Leaf x)     = 1
size (Node t1 t2) = size t1 + size t2
```

Random-access list の操作では木のサイズを利用するものがある


### Random-access lists/導入III

```
data Tree a = Leaf a | Node Nat (Tree a) (Tree a)

size :: Tree a -> Int
size (Leaf x)     = 1
size (Node n _ _) = n

node :: Tree a -> Tree a -> Tree a
node t1 t2 = Node (size t1 + size t2) t1 t2
-- 正しいサイズ情報を設定
-- smart constructor
```

### Random-access lists/導入IV

Random-access list は完全二分木の列

```
 [Zero,                                       -- 0
  One (Node 2 (Leaf 'a') (Leaf 'b')),         -- 1
  One (Node 4 (Node 2 (Leaf 'c') (Leaf 'd'))  -- 1
              (Node 2 (Leaf 'e') (Leaf 'f')))]
6の 2進bitの逆順は 011, "abcdef" から構成
```

```
 [One (Leaf 'a'),                             -- 1
  Zero,                                       -- 0
  One (Node 4 (Node 2 (Leaf 'b') (Leaf 'c'))  -- 1
              (Node 2 (Leaf 'd') (Leaf 'e')))]
5の 2進bitの逆順は 101, "abcde" から構成
```

### Random-access lists/導入V

Random-access list は完全二分木の列

```
空のリストは []
末尾の Zero は付けない
```

### Random-access lists/導入VI

```
data Digit a = Zero | One (Tree a)
type RAList a = [Digit a]
```

```
fromRA :: RAList a -> [a]
fromRA = concatMap from
         where from Zero    = []
               from (One t) = fromT t

fromT :: Tree a -> [a]
fromT (Leaf x) = [x]
fromT (Node _ t1 t2) = fromT t1 ++ fromT t2
```

`fromT` の効率化は演習問題


### Random-access lists/fetchRA


```
fetchRA :: Nat -> RAList a -> a
fetchRA k (Zero:xs)  = fetchRA k xs
fetchRA k (One t:xs) = if k < size t
                       then fetchT k t else fetchRA (k - size t) xs

fetchT :: Nat -> Tree a -> a
fetchT 0 (Leaf x)       = x
fetchT k (Node n t1 t2) = if k < m
                          then fetchT k t1 else fetchT (k - m) t2
                          where m = n `div` 2
```

```
fetch k . fromRA = fetchRA k
```

### Random-access lists/Other operations

```
nullRA   :: RAList a -> Bool
nilRA    :: RAList a
consRA   :: a -> RAList a -> RAList a
unconsRA :: RAList a -> (a, RAList a)
updateRA :: Nat -> a -> RAList a -> RAList a
```

`updateRA` は演習問題

### Random-access lists/consRA

```
inc []     = [1]
inc (0:bs) = 1:bs
inc (1:bs) = 0:inc bs

consRA x xs = consT (Leaf x) xs
consT t1 []          = [One t1]
consT t1 (Zero:xs)   = One t1:xs
consT t1 (One t2:xs) = Zero:consT (node t1 t2) xs
```

### Random-access lists/unconsRA

```
dec [1]    = []
dec (1:ds) = 0:ds
dec (0:ds) = 1:dec ds

unconsRA xs = (x,ys) where (Leaf x,ys) = unconsT xs
unconsT :: RAList a -> (Tree a, RAList a)
unconsT (One t:xs) = if null xs then (t,[]) else (t,Zero:xs)
unconsT (Zero:xs)  = (t1,One t2:ys) where (Node _ t1 t2,ys) = unconsT xs
```

### Random-access lists/unconsRA example

```
unconsRA xs = (x,ys) where (Leaf x,ys) = unconsT xs
unconsT :: RAList a -> (Tree a, RAList a)
unconsT (One t:xs) = if null xs then (t,[]) else (t,Zero:xs)
unconsT (Zero:xs)  = (t1,One t2:ys) where (Node _ t1 t2,ys) = unconsT xs
```

```
unconsRA [Zero,Zero,One t]

  (t1,One t2:ys) where (Node _ t1 t2,ys) = unconsT [Zero,One (tree "abcd")]
     t1 = Leaf 'a', t2 = Leaf 'b', ys = [One (tree "cd")]
  (t3,One t4:zs) where (Node _ t3 t4,zs) = unconsT [One (tree "abcd")]
     t3 = tree "ab", t4 = tree "cd", zs = []
  (tree "abcd",[])
     Node 4 (tree "ab") (tree "cd")
```

### Random-access lists/Exercise 3.8

`fromT` の計算時間の見積り
`fromT` の効率化

### Random-access lists/Exercise 3.9

適切なエラーメッセージを出す `fetchRA`


### Random-access lists/Exercise 3.11

`updateRA`


### Random-access lists/Exercise 3.12

```
(//) :: RAList a -> [(Nat,a)] -> RAList a
```


### Random-access lists/Exercise 3.13

`headRA`, `tailRA`
