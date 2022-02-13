-- Binary Tree

data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving (Show)

addInTree :: Ord a => a -> Tree a -> Tree a
addInTree val Empty = Node Empty val Empty
addInTree val (Node leftBranch cmpVal rightBranch) =
    if val > cmpVal then Node leftBranch cmpVal (addInTree val rightBranch)
    else Node (addInTree val leftBranch) cmpVal rightBranch

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node left val right) = (Node (fmap f left) (f val) (fmap f right))

listToTree :: Ord a => [a] -> Tree a
listToTree xs = foldr addInTree Empty xs

treeToList :: Ord a => Tree a -> [a]
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right

treeSort :: Ord a => [a] -> [a]
treeSort list = treeToList (listToTree list)

instance Foldable Tree where
    foldr f acc Empty = acc
    foldr f acc (Node left val right) =
        foldr f (f val (foldr f acc right)) left