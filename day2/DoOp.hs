module Main where

import Data.Char (ord)
import Control.Monad (replicateM)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)

-- Prerequisite

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem elem (x:xs) = if elem == x then True else myElem elem xs

-- Maybe

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (div x y)

safeNth :: [a] -> Int -> Maybe a
safeNth [] i = Nothing
safeNth (x:xs) i = if i == 0 then Just x else safeNth xs (i - 1)

safeSucc :: Maybe Int -> Maybe Int
safeSucc x = fmap (+ 1) x

safeAdd :: Maybe Int -> Maybe Int -> Maybe Int
safeAdd Nothing _ = Nothing
safeAdd _ Nothing = Nothing
safeAdd (Just x) (Just y) = fmap (+ x) (Just y)

safeMod :: Maybe Int -> Maybe Int -> Maybe Int
safeMod Nothing _ = Nothing
safeMod _ Nothing = Nothing
safeMod _ (Just 0) = Nothing
safeMod (Just x) (Just y) = fmap (`mod` y) (Just x)

safeMinus :: Maybe Int -> Maybe Int -> Maybe Int
safeMinus Nothing _ = Nothing
safeMinus _ Nothing = Nothing
safeMinus (Just x) (Just y) = fmap (x -) (Just y)

safeMult :: Maybe Int -> Maybe Int -> Maybe Int
safeMult Nothing _ = Nothing
safeMult _ Nothing = Nothing
safeMult (Just x) (Just y) = fmap (x *) (Just y)

verySafeDiv :: Maybe Int -> Maybe Int -> Maybe Int
verySafeDiv Nothing _ = Nothing
verySafeDiv _ Nothing = Nothing
verySafeDiv _ (Just 0) = Nothing
verySafeDiv (Just x) (Just y) = fmap (`div` y) (Just x)

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup firstElem ((x, y):zs) =
    if firstElem == x then Just y else myLookup firstElem zs

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo _ _ Nothing = Nothing
maybeDo _ Nothing _ = Nothing
maybeDo f (Just x) (Just y) = Just (f x y)

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt string =
    case reads string of
    [(x, "")] -> Just x
    _ -> Nothing

-- IO

getLineLength :: IO Int
getLineLength = fmap length getLine

printAndGetLength :: String -> IO Int
printAndGetLength x = putStrLn x >> return (length x) :: IO Int

printBox :: Int -> IO()
printBox n = myEngine n [1..n]

myEngine :: Int -> [Int] -> IO()
myEngine _ [] = putStr ""
myEngine n (x:xs) = printLine (x == 1 || x == n) n >> myEngine n xs

putStrNTimes :: Int -> [Char] -> IO()
putStrNTimes 0 _ = putStr ""
putStrNTimes n c =
    if n > 1 then putStr c >> putStrNTimes (n - 1) c else putStr c

printLine :: Bool -> Int -> IO()
printLine isTopOrBottom n =
    putStr ext >>
    (if n >= 1 then putStrNTimes ((n - 1) * 2) mid else putStr "") >>
    putStrLn ext
    where
        ext = if isTopOrBottom then "+" else "|"
        mid = if isTopOrBottom then "-" else " "

concatLines :: Int -> IO String
concatLines n = fmap concat (replicateM n getLine)

getInt :: IO (Maybe Int)
getInt = fmap readInt getLine

myOperation :: String -> (Maybe Int -> Maybe Int -> Maybe Int)
myOperation op = case op of
    "+" -> safeAdd
    "-" -> safeMinus
    "/" -> verySafeDiv
    "*" -> safeMult
    "%" -> safeMod
    _ -> (\x y -> Nothing)

printMySafeInt :: Maybe Int -> IO()
printMySafeInt Nothing = exitWith (ExitFailure 84)
printMySafeInt (Just x) = print x

main :: IO()
main = do
    args <- getArgs
    if length args == 3
        then
        printMySafeInt (myOperation (head (tail args))
        (readInt (head args)) (readInt (last args)))
    else exitWith (ExitFailure 84)