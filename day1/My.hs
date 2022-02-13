mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int -> Bool
myIsNeg x = if x < 0 then True else False

myAbs :: Int -> Int
myAbs x = if x < 0 then -x else x

myMin :: Int -> Int -> Int
myMin x y = if x < y then x else y

myMax :: Int -> Int -> Int
myMax x y = if x > y then x else y

myTuple :: a -> b -> (a, b)
myTuple x y = (x, y)

myTruple :: a -> b -> c -> (a, b, c)
myTruple x y z = (x, y, z)

myFst :: (a, b) -> a
myFst (x, y) = x

mySnd :: (a, b) -> b
mySnd (x, y) = y

mySwap :: (a, b) -> (b, a)
mySwap (x, y) = (y, x)

myHead :: [a] -> a
myHead [] = error "Empty List"
myHead (x:xs) = x

myTail :: [a] -> [a]
myTail [] = error "Empty list"
myTail (x:xs) = xs

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myNth :: [a] -> Int -> a
myNth [] i = error "Index too large or negative"
myNth (x:xs) i = if i == 0 then x else myNth xs (i - 1)

myTake :: Int -> [a] -> [a]
myTake i [] = []
myTake i (x:xs) = if i == 1 then [x] else x:myTake (i - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop i [] = []
myDrop i (x:xs) = if i == 1 then xs else myDrop (i - 1) xs

myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x : myAppend xs ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myAppend (myReverse xs) [x]

myInit :: [a] -> [a]
myInit [] = error "Empty list"
myInit (x:xs) = if myLength (x:xs) == 1 then [] else x : myInit xs

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast (x:xs) = if myLength (x:xs) == 1 then x else myLast xs

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((x, y):zs) = if myLength zs == 0 then ([x], [y])
    else (x : myFst (myUnzip zs), y : mySnd (myUnzip zs))

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs) = if f x then x : myFilter f xs else myFilter f xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f value (x:xs) =
    if myLength (x:xs) == 1 then f value x else myFoldl f (f value x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f value (x:xs) =
    if myLength (x:xs) == 1 then f (myLast (x:xs)) value
    else myFoldr f (f (myLast (x:xs)) value) (myInit (x:xs))

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition _ [] = ([], [])
myPartition f (x:xs) = if f x
    then (x : myFst (myPartition f xs), mySnd (myPartition f xs))
    else (myFst (myPartition f xs), x : mySnd (myPartition f xs))

myNotFilter :: (a -> a -> Bool) -> a -> [a] -> [a]
myNotFilter _ _ [] = []
myNotFilter f x (y:ys) = --if f x y == False then False else isSmallest f x ys
    if f x y then myNotFilter f x ys else y : myNotFilter f x ys

myQuickSort :: (a -> a -> Bool) -> [a] -> [a]
myQuickSort _ [] = []
myQuickSort f (x:xs) =
    myAppend (myAppend (myQuickSort f (myNotFilter f x xs)) [x])
    (myQuickSort f (myFilter (f x) xs))