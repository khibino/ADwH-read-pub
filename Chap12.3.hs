
-- 12.3

import Prelude hiding (Word)
import Partition

type Text = [Word]
type Word = [Char]
type Para = [Line]
type Line = [Word]

-- para =

fits :: Line -> Bool
fits line = width line <= maxWidth

foldrn :: (a -> b -> b) -> (a -> b) -> [a] -> b
foldrn _ _ []     = error "foldrn: type error: empty"
foldrn _ g [x]    = g x
foldrn f g (x:xs) = f x (foldrn f g xs)

width :: Line -> Nat
width = foldrn add length where add w n = length w + 1 + n

type Nat = Int

cost1 :: [a] -> Nat
cost1 = length

cost2 :: [Line] -> Nat
cost2 = sum . map waste . init
  where waste line = maxWidth - width line

cost3 :: [Line] -> Nat
cost3 = sum . map waste . init
  where waste line = (optWidth - width line)^(2 :: Int)

cost4 :: [Line] -> Nat
cost4 = foldr max 0 . map waste . init
  where waste line = maxWidth - width line

cost5 :: [Line] -> Nat
cost5 = foldr max 0 . map waste . init
  where waste line = (optWidth - width line)^(2 :: Int)

greedy :: [Word] -> [Line]
greedy = foldl add []
  where add [] w = snoc w []
        add p  w = head (filter (fits . last) [bind w p, snoc w p])

maxWidth :: Nat
maxWidth = 10

optWidth :: Nat
optWidth = 8

example :: String
example = "666666 1 55555 333 4444 7777777"

exampleL :: String
exampleL = "The algorithm works by adding each word to the end of the last line of the current  paragraph until no more words will fit, in which case a new line is started. A more  efficient version is discussed in the exercises. This algorithm is essentially the one  used by Microsoft Word and many other word processors.  So, for which definition of cost does the greedy algorithm work?"

greedyH :: Text -> Para
greedyH (w:ws) = help (length w, (w:)) ws
greedyH _ = error "greedyH"

help :: (Int, Line -> Line) -> Text -> Para
help ( _, l) [] = [l []]
help (sz, l) (w:ws) =
  if sz2 <= maxWidthH
  then help (sz2, l . (w:)) ws  else l [] : help (wlen, (w:)) ws
  where wlen = length w
        sz2 = sz + 1 + wlen
        maxWidthH = 80
