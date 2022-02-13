module PushswapFunctions where

-- [1,2,3] -> [2,1,3]
swapList :: [Maybe Int] -> [Maybe Int]
swapList [] = []
swapList [x] = [x]
swapList (x:y:zs) = y:x:zs

-- Puts first element of one list into the other
-- Bool is telling if it's from the first or second list
putInList :: ([Maybe Int], [Maybe Int]) -> Bool -> ([Maybe Int], [Maybe Int])
putInList ([], x) True = ([], x)
putInList (x, []) False = (x, [])
putInList (x:xs, ys) True = (xs, x:ys)
putInList (xs, y:ys) False = (y:xs, ys)

-- [1,2,3] -> [2,3,1]
rotateList :: [Maybe Int] -> [Maybe Int]
rotateList [] = []
rotateList (x:xs) = xs ++ [x]

-- [1,2,3] -> [3,1,2]
reverseRotateList :: [Maybe Int] -> [Maybe Int]
reverseRotateList [] = []
reverseRotateList list = last list : init list