module Chap16PSQ where

import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as OrdPSQ

type PSQ a k p = OrdPSQ k p a

insertQ :: (Ord k, Ord p) => (a -> k) -> a -> p -> PSQ a k p -> PSQ a k p
insertQ kf x px q = case OrdPSQ.lookup k q of
  Just (py, _) | px < py    ->  inserted
               | otherwise  ->  q
  Nothing                   ->  inserted
  where
    k = kf x
    inserted = OrdPSQ.insert k px x q

addListQ :: (Ord k, Ord p) => (a -> k) -> [(a,p)] -> PSQ a k p -> PSQ a k p
addListQ kf ts q = foldr (uncurry $ insertQ kf) q ts

{- 書籍のまちがい? -}
-- deleteQ :: (Ord k, Ord p) => (a -> k) -> PSQ a k p -> ((a,p),PSQ a k p)
deleteQ :: (Ord k, Ord p) => PSQ a k p -> ((a,p), PSQ a k p)
deleteQ q = case OrdPSQ.minView q of
  Just (_k, p, x, q1) -> ((x, p), q1)
  Nothing             -> error "deleteQ: empty queue."

emptyQ :: PSQ a k p
emptyQ = OrdPSQ.empty

nullQ :: PSQ a k p -> Bool
nullQ = OrdPSQ.null

removeQ :: (Ord k, Ord p) => PSQ a k p -> (a, PSQ a k p)
removeQ q1 = (x,q2) where ((x,_),q2) = deleteQ q1
