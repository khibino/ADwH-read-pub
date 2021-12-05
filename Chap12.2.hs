
import Partition

{- 12.2  Managing two bank accounts -}

{-
Zakiaと呼ぶある個人は、当座預金と貯蓄預金の2つのオンライン銀行口座を持っています。Zakiaは、当座預金口座を、給与、定期預金、公共料金の支払いなど、決まった既知の一連の取引（入出金）にのみ使用しています。セキュリティ上の理由から、Zakiaは当座預金に一定の金額C以上の預金を望んでいません。ここで、Cは少なくとも1回の取引に相当すると想定される一定の金額です。このセキュリティ条件を維持するために、Zakiaは、当座預金口座と預金口座の間の自動振替を設定し、各取引の開始時に、次の取引に対応するために当座預金口座にお金を出し入れできるようにしたいと考えています。トラフィックを最小限に抑えるために、Zakiaさんはこのような送金の回数をできるだけ少なくしたいと考えています。
 -}


{-
あるセグメントが安全であれば、そのセグメントのすべての接頭辞と接尾辞も安全であることを証明することは課題として残されている。
安全条件を簡単にするために、和0,x1,x1 +x2,...,x1 +x2 +---+xkの最大値と最小値をmとnとすると、n≦0≦mである。
次に、0≦r＋n≦Cかつ0≦r＋m≦Cとなるようなrが存在することが求められる。
この2つの条件はm≦C +nと等価である（練習問題参照）。
 -}

{-
n ≦ 0≦ m

0 ≦ r+n ≦C かつ 0 ≦ r+m ≦C

0 ≦ r+n ≦ r ≦ r+m ≦ C

-n ≦ r
0 ≦ -n

r-n ≦ r+m ≦ C
r ≦ C+n
 -}

c :: Int
c = 100

safe :: Segment Int -> Bool
safe xs = maximum sums <= c + minimum sums
  where sums = scanl (+) 0 xs

{-
msp :: [Int] -> Partition Int
msp <- MinWith length . filter (all safe) . parts
 -}

safeParts :: [Int] -> [Partition Int]
safeParts = foldr (concatMap . safeExtendl) [[]]
safeExtendl :: Int -> Partition Int -> [Partition Int]
safeExtendl x = filter (safe . head) . extendl x

cost :: Partition a -> (Int, Int)
cost p = (length p, length (head p))

-- add :: Int -> Partition Int -> Partition Int
-- add x [] = [[x]]
-- add x (s:p) = if safe (x:s) then (x:s):p else [x]:s:p

msp :: [Int] -> Partition Int
msp = foldr add []
  where
    add :: Int -> Partition Int -> Partition Int
    add x [] = [[x]]
    add x (s:p) = if safe (x:s) then (x:s):p else [x]:s:p

exampleBank :: [Int]
exampleBank = [50,20,30,-10,40,-90,-20,60,70,-40,80]

exampleBankMsp :: Partition Int
exampleBankMsp = msp exampleBank

---

-- Exercise 12.8

{- C = 10 on exercise 12.8 -}
safe128 :: Segment Int -> Bool
safe128 xs = maximum sums <= 10 + minimum sums
  where sums = scanl (+) 0 xs


msp128 :: [Int] -> Partition Int
msp128 = foldr add []
  where
    add :: Int -> Partition Int -> Partition Int
    add x [] = [[x]]
    add x (s:p) = if safe128 (x:s) then (x:s):p else [x]:s:p


-- Exercise 12.9

-- triplePart :: Partition Int -> (Partition Int, Int, Int)
-- triplePart p = (p, minimum (sums (head p)), maximum (sums (head p)))  where sums = scanl (+) 0

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


-- Exercise 12.10

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

exercise1210 :: IO ()
exercise1210 = do
    print   input
    print $ msp input
    print $ transfers $ msp input
  where
    input = [40,-85,55,-32,79,80,-21,80]
