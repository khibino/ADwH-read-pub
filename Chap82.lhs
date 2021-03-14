
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


------

> data Tree a = Leaf a | Node (Tree a) (Tree a)
>             deriving Show

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
