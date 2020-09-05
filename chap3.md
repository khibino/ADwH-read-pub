---
marp: true
---
<!-- theme: gaia -->
<!-- template: invert -->
<!-- page_number: true -->
<!-- paginate: true -->
<!-- headingDivider: 3 -->

# 3章 Useful data structures

@khibino

---

<div style="text-align:left">
多くは普通のリストで効率的に実装できるが<br>
ときどき別の構造が必要
</div>

<br/>

* symmetric lists
* random-access lists
* arrays

<br/>
<br/>

<div style="text-align:left">
リストの基本的な操作の実行時間における欠陥を
それぞれの方法で克服
</div>

---

## 3.1 Symmetric lists

---

### Symmetric lists/型定義

```haskell
type SymList a = ([a], [a])
```

<div style="text-align:left">
symmetric list `(xs,ys)` は<br>
普通のリスト `xs ++ reverse ys` を表現
</div>

<!-- --- -->

<!-- ### Symmetric lists/abstract function -->

```haskell
fromSL :: SymList a -> [a]
fromSL (xs, ys) = xs ++ reverse ys
```

<div style="text-align:left">
fromSL は symmetric list を抽象的な表現へと戻す

このような関数を abstract function
</div>

---

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

---

### Symmetric lists/不変条件

```
null xs ⇒ null ys \/ single ys
null ys ⇒ null xs \/ single xs
```

<div style="text-align:left">
どちらかが空のリストならば<br/>
もう一方は空のリストか singleton リスト
</div>

```haskell
snocSL :: a -> SymList a -> SymList a
snocSL x (xs,ys) = if null xs then (ys,[x]) else (xs,x:ys)
```

```
[] ++ reverse [] ++ [x] = [] ++ reverse [x]
[] ++ reverse [y] ++ [x] = [y] ++ reverse [x]
```

---
