
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

  candidates:: State  [Candidate]

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
