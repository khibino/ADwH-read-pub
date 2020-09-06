---
marp: true
---
<!-- theme: gaia -->
<!-- template: invert -->
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

多くは普通のリストで効率的に実装できるが
ときどき別の構造が必要

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

symmetric リストを利用しない inits の別の定義は演習問題

Data.Sequence のフィンガーツリーが名前だけ紹介されていた


## 3.2 Random-access lists
