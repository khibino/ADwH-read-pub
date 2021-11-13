
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

exampleBank :: Partition Int
exampleBank = msp [50,20,30,-10,40,-90,-20,60,70,-40,80]
