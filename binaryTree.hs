{-

In computing, it is often useful to store data in a
two-way branching structure or binary tree.

             5
           /   \
          3     7
        /  \   / \
       1   4  6   9

-}


{-

Using recursion, a suitable new type to represent
such binary trees can be declared by:

-}

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)

{-

For example, the tree on the previous slide would
be represented as follows:

-}

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))



{-

We can now define a function that decides if a given
value occurs in a binary tree:

But… in the worst case, when the value does not
occur, this function traverses the entire tree.

-}

occurs1 :: Ord a => a -> Tree a -> Bool
occurs1 x (Leaf y) = x == y
occurs1 x (Node l y r) = x == y
                         || occurs1 x l
                         || occurs1 x r

{-

Now consider the function flatten that returns the
list of all the values contained in a tree:

A tree is a search tree if it flattens to a list that is
ordered. Our example tree is a search tree, as it
flattens to the ordered list [1,3,4,5,6,7,9].

-}

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l
                       ++ [x]
                       ++ flatten r

{-

Search trees have the important property that when
trying to find a value in a tree we can always decide
which of the two sub-trees it may occur in.

This new definition is more efficient, because it only
traverses one path down the tree.

-}

occurs2 :: Ord a => a -> Tree a -> Bool
occurs2 x (Leaf y)             = x == y
occurs2 x (Node l y r) | x == y = True
                       | x < y = occurs2 x l
                       | x > y = occurs2 x r