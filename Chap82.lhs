
8.2 Huffman coding trees
-----

Our second example is Huffman coding trees.
As older computer users know only too well, it is often necessary to store files of information as compactly as possible.
Suppose the information to be stored is a text consisting of a sequence of characters.
Haskell uses Unicode internally for its Char data type, but the standard text I/O  functions assume that texts are sequences of 8-bit characters,
so a text of n characters  contains 8n bits of information.
Each character is represented by a fixed-length  code, so the characters of a text can be recovered by decoding each successive group  of eight bits.

One idea for reducing the total number of bits required to code a text is to abandon  the notion of fixed-length codes,
and seek instead a coding scheme based on the  relative frequency of occurrence of the characters in the text.
The basic idea is to  take a sample piece of text, estimate the number of times each character appears,
and choose short codes for the more frequent characters and longer codes for the  rarer ones.
For example, if we take the codes

  't' -->  0
  'e' -->  10
  'x' -->  11

then text can be coded as the bit sequence 010110 of length 6.
However, it is important that codes are chosen in such a way as to ensure that the coded text can  be deciphered uniquely.
To illustrate, suppose the codes had been

  't' -->  0
  'e' -->  10
  'x' -->  1

Under this scheme, text would be coded as the sequence 01010 of length 5.
However, the string tee would also be coded by 01010.
Obviously this is not what is wanted.

{- p.188 -}

The simplest way to prevent the problem arising is to choose codes so that no code is a proper prefix of any other  a prefix-free code.

As well as requiring unique decipherability, we also want the coding to be optimal.
An optimal coding scheme is one that minimises the expected length of the coded  text.
More precisely, if characters c{j}, for 1 ≤ j ≤ n, have frequencies of occurrence  pj,
then we want to choose codes with lengths lj such that

Σ_{j=1}^{n} pj・lj

is as small as possible.

One method for constructing an optimal code satisfying the prefix property is  called Huffman coding.
Each character is stored in a leaf of a binary tree, the  structure of which is determined by the computed frequencies.
The code for a  character c is the sequence of binary values describing the path in the tree to the  leaf containing c.
For instance, with the tree

Node (Node (Leaf 'b') (Leaf 'e')) (Leaf 't')

the character b is coded by 00, the character e by 01, and the character t by 1.
Clearly, such a scheme yields a prefix-free code.

There are four aspects to the problem of implementing Huffman coding:
(i) collecting information from a sample;
(ii) building a binary tree;
(iii) coding a text; and
(iv) decoding a bit sequence.
We deal only with the problem of building a tree.
So, having analysed the sample, suppose we are given a list of pairs:

[(c1,w1),(c2,w2),...,(cn,wn)]

where for 1 ≤ j ≤ n the c{j} are the characters and the wj are positive integers,
called weights, indicating the frequencies of the characters in the text.
The relative  frequency of character c{j} occurring is therefore wj/W, where W = wj.
We will  suppose w1 w2  wn, so that the weights are given in ascending order.

In terms of trees, the cost function we want to minimise can be defined in the  following way.
By definition, the depth of a leaf is the length of the path from the  root of the tree to the leaf.
We can define the list of depths of the leaves in a tree by

> depths :: Tree a -> [Nat]
> depths = from 0
>          where from n (Leaf x) =[n]
>                from n (Node u v) = from (n+1) u++from (n+1) v

Now introduce the types

> type Weight = Nat
> type Elem = (Char,Weight)
> type Cost = Nat

{- p.189 -}

and define cost by

  cost ::Tree Elem -> Cost
  cost t = sum [wd | ((,w),d)  zip (fringe t) (depths t)]

It is left as an exercise to derive the following alternative definition of cost:

  cost (Leaf e) = 0
  cost (Node u v) = cost u + cost v + weight u + weight v
  weight :: Tree Elem -> Nat
  weight (Leaf (c,w)) = w
  weight (Node u v) = weight u + weight v

We might now follow the previous section and specify

  huffman:: [Elem] -> Tree Elem
  huffman <- MinWith cost mktrees

where mktrees builds all the trees with a given list as fringe.
But this specification is too strong:
it is not required that the input list be the fringe, only that some  permutation of it is.
(However, in Chapter 14 we will consider a version of the  problem in which the input is required to be the fringe.)
One way of correcting the definition is to replace mktrees by concatMap mktrees perms.
Another way, and  the one we will pursue, is to design a new version of mktrees.
This version will construct all unordered binary trees.
In an unordered binary tree the two children of a node are regarded as a set of two trees rather than an ordered pair.
Thus Node u v is regarded as the same tree as Node v u.
For example, there are 12 ordered binary trees whose fringe is a permutation of [1,2,3], two trees for each of the six permutations,
but only three essentially different unordered trees:

  Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
  Node (Node (Leaf 1) (Leaf 3)) (Leaf 2)
  Node (Node (Leaf 2) (Leaf 3)) (Leaf 1)

Each tree can be flipped in three ways
(flipping the children of the top tree, the  children of the left subtree, or both)
to give the 12 different ordered binary trees.
For Huffman coding it is sufficient to consider unordered trees
because two sibling characters have the same codes except for the last bit and it does not matter which  sibling is on the left.
To compute all the unordered Huffman trees we can start with a list of leaves in weight order,
and then repeatedly combine pairs of trees until a  single tree remains.
The pairs are chosen in all possible ways and a combined pair can be placed back in the list so as to maintain weight order.
Thus, in an unordered tree Node u v we can assume cost u  cost v without loss of generality.

Here is an example to see the idea at work. Showing only the weights,
consider the following list of four trees in weight order:

{- p.190 -}

  [Leaf 3,Leaf 5,Leaf 8,Leaf 9]

As a first step we can choose to combine the first and third trees (among six possible  choices) to give

  [Leaf 5,Leaf 9,Node (Leaf 3) (Leaf 8)]

The new tree, with weight 11, is placed last in the list to maintain weight order.
As the next step we can choose to combine the first two trees (among three possible  choices),
giving

  [Node (Leaf 3) (Leaf 8),Node (Leaf 5) (Leaf 9)]

The next step is forced as there are only two trees left, and we end up with a singleton tree

  [Node (Node (Leaf 3) (Leaf 8)) (Node (Leaf 5) (Leaf 9))]

whose fringe is [3,8,5,9].
This bottom-up method for building trees will generate  6 × 3 = 18 trees in total,
more than the total number of unordered trees on four  elements,
because some trees, such as the one above, are generated twice (see the  exercises).
However, the list of trees includes all that are needed.

Now for the details. We define

  mktrees :: [Elem] -> [Tree Elem]
  mktrees = map unwrap . mkforests . map Leaf

where mkforests builds the list of forests, each forest consisting of a singleton tree.
On way to define this function uses until:

  mkforests :: [Tree Elem] -> [Forest Elem]
  mkforests = until (all single) (concatMap combine) . wrap

The function mkforests takes a list of trees, turns them into a singleton list of forests  by applying wrap,
and then repeatedly combines two trees in every possible way  until every forest is reduced to a single tree.
Each singleton forest is then unwrapped  to give the final list of trees.
The function combine is defined by

  combine :: Forest Elem  -> [Forest Elem]
  combine ts = [insert (Node t1 t2) us | ((t1,t2),us) <- pairs ts]
  pairs :: [a] -> [((a,a),[a])]
  pairs xs = [((x,y),zs) | (x,ys) <- picks xs, (y,zs) <- picks ys]

The function picks was defined in Chapter 1.
The function insert, whose definition is left as an exercise,
inserts a tree into a list of trees so as to maintain weight order.
Hence combine selects, in all possible ways, a pair of trees from a forest,
combines them into a new tree, and inserts the new tree into the remaining trees.

Another way to define mkforests uses the function apply.
Recall the answer to  Question 1.13, which gives the following definition of apply:

{- p.191 -}

8.2 Huffman coding trees 191

  apply :: Nat -> (a -> a) -> a -> a
  apply n f = if n == 0 then id else f . apply (n - 1) f

Thus apply n applies a function n times to a given value.
The alternative definition  of mkforests is to write

  mkforests::[Tree Elem]  [Forest Elem]
  mkforests ts = apply (length ts1) (concatMap combine) [ts]

The two definitions give the same result because at each step the number of trees in each forest is reduced by one,
so it takes exactly n-1 steps to reduce an initial forest of n trees to a list of singleton forests.

Our problem now takes the form

  huffman :: [Elem] -> Tree Elem
  huffman  MinWith cost mktrees

Since mktrees is defined in terms of until, we will aim for a constructive definition of huffman of the same form.
The task is to find a function gstep so that

  unwrap (until single gstep (map Leaf xs)) <- MinWith cost (mktrees xs)

for all finite nonempty lists xs of type [Elem].
More generally, we will seek a  function gstep such that

  unwrap (until single gstep ts) <- MinWith cost (map unwrap (mkforests ts))

for all finite nonempty lists of trees ts.
Problems of this form will arise in the  following chapter too,
so let us pause for a little more theory on greedy algorithms.

 ### Another generic greedy algorithm

Suppose in this section that the list of candidates is given by a function

  candidates:: State -> [Candidate]

for some type State.
For Huffman coding, states are lists of trees and candidates are trees:

  candidates ts = map unwrap (mkforests ts)

For the problems in the following chapter, states are combinations of values.

The aim of this section is to give conditions for which the refinement

  extract (until final gstep sx) <- MinWith cost (candidates sx) (8.2)

holds for all states sx.
The functions on the left have the following types:

  gstep :: State -> State
  final :: State -> Bool
  extract :: State -> Candidate

{- p.192 -}

In words, (8.2) states that repeatedly applying a greedy step to any initial state sx will result in a final state
from which a candidate x can be extracted with the property that x is a candidate in candidates sx with minimum cost.
In order for the refinement to be meaningful,
it is assumed that the left-hand side returns a well-defined value for any initial state.
Unlike the formulation of a generic greedy algorithm in Section 7.1,
nothing is known about how the candidates are constructed.

For brevity in what follows, define

  MCC sx = MinWith cost (candidates sx)
  mincost sx = minimum (map cost (candidates sx))

In particular, for all x in candidates sx we have

  x <- MCC sx ⟺ cost x = mincost sx

There are two conditions that ensure (8.2).
The first is

  final sx ⟹ extract sx <- MCC sx   (8.3)

This condition holds for Huffman coding,
when final = single and extract = unwrap,
since map unwrap (mkforests [t]) = [t] and MinWith cost [t] = t.

The second condition is the greedy condition.
We can state it in two ways.
The  first way is

  not (final sx) ⟹ (∃x:x <- MCC (gstep sx) ⋀ x <- MCC sx)   (8.4)

In hillclimbing terms, the greedy condition asserts that,
from any starting point not  already on top of the hill,
there is some path to a highest point that starts out with a greedy step.

The second way of stating the greedy condition appears to be stronger:

  not (final sx) ⟹ MCC (gstep sx) <- MCC sx   (8.5)

However, with one extra proviso, (8.4) implies (8.5).
The proviso is that applying gstep to a state may reduce the number of final candidates but will never introduce new ones.
In symbols,
  candidates (gstep sx) ⊆ candidates sx   (8.6)

Suppose x <- MCC (gstep sx) and x <- MCC sx.
Then, by definition of MCC and mincost, we have

  mincost (gstep sx) = cost x = mincost sx

Now suppose y <- MCC (gstep sx), so y ∈ candidates sx by (8.6).
Then

  cost y = mincost (gstep sx) = mincost sx

and so y <- MCC sx.
Although, in general,
E1 <- E2 is a stronger statement than one that merely asserts there exists some value v such that v <- E1 ⋀ v <- E2,
that is not the case here.

To prove (8.2), suppose that k is the smallest integer -- assumed to exist --
for which apply k gstep sx is a final state.
That means

{- p.193 -}

  until final gstep sx = apply k gstep sx

It follows that apply j gstep sx is not a final state for 0 ≤ j<k, so,
by the stronger greedy condition, we have

  MCC (apply (j+1) gstep sx) <- MCC (apply j gstep sx)

for 0 ≤ j<k.
Hence MCC (apply k gstep sx) <- MCC sx.
Furthermore, by (8.3) we have

  extract (apply k gstep sx) <- MCC (apply k gstep sx)

establishing (8.2).

This style of reasoning about greedy algorithms is very general.
However, unlike  greedy algorithms derived by fusion,
it gives no hint as to what form gstep might take.

 ### Huffman coding continued

Returning to Huffman coding,
in which candidates are trees,
it remains to define  gstep and to show that the greedy condition holds.
For Huffman coding we have

  MCC ts = MinWith cost (map unwrap(mkforests ts))

We take gstep to be the function that combines the two trees in the forest with smallest weights.
Since trees are kept in weight order, that means

  gstep (t1 :t2 :ts) = insert (Node t1 t2) ts

For the greedy condition, let ts = [t1,t2,...,tn] be a list of trees in weight order,
with weights [w1,w2,...,wn].
The task is to construct a tree t for which

  t <- MCC (gstep ts) ⋀ t <- MCC ts

Suppose t' <- MCC ts.
We construct t by applying tree surgery to t'.
Every tree in ts appears somewhere as a subtree of t',
so imagine that t{i} appears at depth d{i} in t' for 1 ≤ i ≤ n.
Now, among the subtrees of t',
there will be a pair of sibling trees at greatest depth.
There may be more than one such pair, but there will be at least one.
Suppose two such trees are t{i} and t{j} and let d = d{i} = d{j}.
Then d{1} ≤ d and d{2} ≤ d.
Furthermore, t{i} and t{j} could have been chosen as the first step in the construction of t'.
Without loss of generality, suppose w{1} ≤ w{i} and w{2} ≤ w{j}.
Construct t by swapping t{i} with t{1} and t{j} with t{2}.
Then t can be constructed by taking a greedy first step.
Furthermore

  cost t' - cost t = d{1}w{1} + d{2}w{2} + d (w{i} + w{j}) - (d{1}w{i} + d{2}w{j} + d (w{1} + w{2}))
                   = (d - d{1})(w{i} - w{1}) + (d - d{2}) (w{j} - w{2})
                   ≥ 0

But cost t is as small as possible,
so cost t' = cost t.
Hence t <- MCC ts and t <- MCC (gstep ts).

{- p.194 -}

The same tree surgery can be used to show that the stronger greedy condition  holds by a direct argument.
Suppose t <- MCC (gstep ts) but t is not a value in MCC ts.
That means there exists a tree t' <- MCC ts with cost t' < cost t.
We now get a contradiction by applying the surgical procedure to t'
to produce another tree  t   MCC (gstep ts) with cost t = cost t'' ≤ cost t'.
Here is the greedy algorithm we have derived:

  huffman es = unwrap (until single gstep (map Leaf es))
               where gstep (t1 :t2 :ts) = insert (Node t1 t2) ts

However, simple as it is, the algorithm is not quite ready to leave the kitchen.
There are two sources of inefficiency.
Firstly, the function insert recomputes weights at  each step,
an inefficiency that can easily be brushed aside by tupling.
The more serious issue is that,
while finding two trees of smallest weights is a constant-time operation,
inserting the combined tree back into the forest can take linear time in  the worst case.
That means the greedy algorithm takes quadratic time in the worst case.
The final step is to show how this can be reduced to linear time.

The key observation behind the linear-time algorithm is the fact that,
in any  call of gstep, the argument to insert has a weight at least as large as any previous  argument.
Suppose we combine two trees with weights w1 and w2 and, later on, two trees with weights w3 and w4.
We have w1 ≤ w2 ≤ w3 ≤ w4, and it follows that w1+w2 ≤ w3+w4.
This suggests maintaining the non-leaf trees as a simple  queue,
whereby elements are added to the rear of the queue and removed only from  the front.
Instead of maintaining a single list we therefore maintain two lists,
the  first being a list of leaves and the second a queue of node trees.
Since elements are  never added to the first list, but only removed from the front,
the first list could  also be a queue.
But a simple list suffices. We will call the first list a stack simply  to distinguish it from the second one.
At each step, gstep selects two lightest trees from either the stack or the queue,
combines them, and adds the result to the end of the queue.
At the end of the algorithm the queue will contain a single tree, the greedy solution.
Figure 8.2, which shows the weights only, gives an example of how the method works out.
The method is viable only if the various queue operations take constant time.
But we have already met symmetric lists in Chapter 3, which satisfy the requirements exactly.

Here are the details. First we set up the type SQ of Stack-Queues:

> type SQ a = (Stack a,Queue a)
> type Stack a = [a]
> type Queue a = SymList a

Now we can define

> huffman :: [Elem] -> Tree Elem
> huffman = extractSQ . until singleSQ gstep . makeSQ . map leaf

{- p.195 -}

 Stack of weights     Queue of combined weights
------------------------------------------------
 1, 2, 4, 4, 6, 9
       4, 4, 6, 9     1 + 2
          4, 6, 9     4 + (1 + 2)
	        9     4 + (1 + 2), 4 + 6
		      4 + 6, 9 + (4 + (1 + 2))
		      (4 + 6) + (9 + (4 + (1 + 2)))

Figure 8.2 Example of the stack and queue operations

The component functions on the right-hand side are defined in terms of the type

> type Pair = (Tree Elem,Weight)

of pairs of trees and weights.
First of all, the functions leaf and node (needed in the  definition of gstep)
are smart constructors that install weight information correctly:

> leaf :: Elem -> Pair
> leaf (c,w) = (Leaf (c,w),w)
> node :: Pair -> Pair -> Pair
> node (t1,w1) (t2,w2) = (Node t1 t2,w1+w2)

Next, the function makeSQ initialises a Stack-Queue:

> makeSQ :: [Pair] -> SQ Pair
> makeSQ xs = (xs,nilSL)

Recall that the function nilSL returns an empty symmetric list.

Next, the function singleSQ determines whether a Stack-Queue is a singleton,
and extractSQ extracts the tree:

> singleSQ :: SQ a -> Bool
> singleSQ (xs,ys) = null xs && singleSL ys
> extractSQ :: SQ Pair -> Tree Elem
> extractSQ (xs,ys) = fst (headSL ys)

The function singleSL, whose definition is left as an exercise,
tests for whether a  symmetric list is a singleton.

Finally, we define

> gstep :: SQ Pair -> SQ Pair
> gstep ps = add (node p1 p2) rs
>            where (p1,qs) = extractMin ps
>                  (p2,rs) = extractMin qs
> add ::Pair -> SQ Pair -> SQ Pair
> add y (xs, ys) = (xs,snocSL y ys)

{- p.196 -}

It remains to define extractMin for extracting a tree with minimum weight from a Stack-Queue:

> extractMin :: SQ Pair -> (Pair,SQ Pair)
> extractMin (xs,ys)
>   | nullSL ys = (head xs,(tail xs, ys))
>   | null xs = (headSL ys,(xs,tailSL ys))
>   | snd x <= snd y = (x,(tail xs,ys))
>   | otherwise = (y,(xs,tailSL ys))
>   where x = head xs; y = headSL ys

If both the stack and the queue are nonempty,
then the tree with the smallest weight from either list is selected.
If one of the stack and the queue is empty,
the selection is made from the other component.

The linear-time algorithm for Huffman coding depends on the assumption
that the input is sorted into ascending order of weight.
If this were not the case, then  O(n log n) steps have to be spent sorting.
Strictly speaking, that means Huffman coding actually takes O(n log n) steps.
There is an alternative implementation of the algorithm with this running time,
and that is to use a priority queue.
Priority queues  will be needed again, particularly in Part Six, so we will consider them now.


------

> data Tree a = Leaf a | Node (Tree a) (Tree a)
>             deriving (Eq, Show)

> type Nat = Int

> unwrap :: [a] -> a
> unwrap [x] = x

> wrap :: a -> [a]
> wrap x = [x]

> single :: [a] -> Bool
> single [_] = True
> single  _  = False

> pairWith :: (a -> a -> a) -> [a] -> [a]
> pairWith f [] = []
> pairWith f [x] = [x]
> pairWith f (x:y:xs) = f x y : pairWith f xs

> type SymList a = ([a], [a])

> nilSL :: SymList a
> nilSL = ([],[])

> nullSL :: SymList a -> Bool
> nullSL ([],[]) = True
> nullSL  _      = False

> singleSL :: SymList a -> Bool
> singleSL ([_],[]) = True
> singleSL ([],[_]) = True
> singleSL  _       = False

> consSL :: a -> SymList a -> SymList a
> consSL x (xs,ys) = if null ys then ([x],xs) else (x:xs,ys)

> headSL :: SymList a -> a
> headSL ([],ys) = case ys of
>                    []   -> undefined
>                    y:_  -> y
> headSL (x:_,_) = x

> tailSL :: SymList a -> SymList a
> tailSL (xs,ys)
>   | null xs    = if null ys then undefined else nilSL
>   | single xs  = (reverse vs, us)
>   | otherwise  = (tail xs, ys)
>   where (us,vs) = splitAt (length ys `div` 2) ys

> snocSL :: a -> SymList a -> SymList a
> snocSL x (xs,ys) = if null xs then (ys,[x]) else (xs,x:ys)

> initSL :: SymList a -> SymList a
> initSL (xs,ys)
>   | null ys    = if null xs then undefined else nilSL
>   | single ys  = (us, reverse vs)
>   | otherwise  = (xs, tail ys)
>   where (us,vs) = splitAt (length xs `div` 2) xs

> lastSL :: ([p], [p]) -> p
> lastSL (xs,ys) = if null ys
>                  then if null xs
>                       then error "lastSL of empty list"
>                       else head xs
>                  else head ys
