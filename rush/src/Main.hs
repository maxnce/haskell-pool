module Main where

import Test.HUnit

import Data.List (sort)
import Data.Maybe (isJust)
import PushswapFunctions
import Tests
import Text.Read ( readMaybe )
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)

-- Creates the tuple of lists to keep both lists
-- in the same data structure
createListTuple :: [String] -> ([Maybe Int], [Maybe Int])
createListTuple [] = ([], [])
createListTuple (x : xs) =
  if isJust (readMaybe x:: Maybe Int)
    then ((readMaybe x:: Maybe Int) : fst (createListTuple xs), [])
    else ([Nothing], [Nothing]) -- Nothing in list so it's
                                -- an Input error

-- Takes cmd and redirect to rotation or error
redirectRotations ::
  ([Maybe Int], [Maybe Int]) -> String -> ([Maybe Int], [Maybe Int])
redirectRotations (x, y) cmd = case cmd of
  "ra" -> (rotateList x, y)
  "rb" -> (x, rotateList y)
  "rr" -> (rotateList x, rotateList y)
  "rra" -> (reverseRotateList x, y)
  "rrb" -> (x, reverseRotateList y)
  "rrr" -> (reverseRotateList x, reverseRotateList y)
  _ -> ([Nothing], [Nothing]) -- Unknown command -> Input error

-- Takes the action and execute the function linked
-- to this command
redirectActions ::
  ([Maybe Int], [Maybe Int]) -> String -> ([Maybe Int], [Maybe Int])
redirectActions ([Nothing], [Nothing]) _ = ([Nothing], [Nothing])
redirectActions (x, y) cmd = case cmd of
  "sa" -> (swapList x, y)
  "sb" -> (x, swapList y)
  "sc" -> (swapList x, swapList y)
  "pa" -> putInList (x, y) False
  "pb" -> putInList (x, y) True
  _ -> redirectRotations (x, y) cmd -- if cmd is a rotation or unknown

myEngine ::
  ([Maybe Int], [Maybe Int]) ->
  [String] ->
  ([Maybe Int], [Maybe Int])
myEngine lists [] = lists
myEngine ([Nothing], [Nothing]) _ = ([Nothing], [Nothing])
myEngine (x, y) (cmd : cmds) = myEngine (redirectActions (x, y) cmd) cmds

-- Check if the list is composed of Nothing meaning
-- there is an input error
verifList :: ([Maybe Int], [Maybe Int]) -> ([Maybe Int], [Maybe Int])
verifList ([], []) = ([Nothing], [Nothing])
verifList (x, y) =
  if length x == length (filter isJust x)
    then (x, y)
    else ([Nothing], [Nothing])

-- Prints OK or KO if there was no input error, depending
-- on if the list is sorted, exits 84 otherwise
printResult :: ([Maybe Int], [Maybe Int]) -> IO ()
printResult ([Nothing], [Nothing]) = exitWith (ExitFailure 84) -- Input error
printResult (x, []) =
  if sort x == x
    then putStrLn "OK" -- Good case
    else
      putStrLn "KO" -- Unordered list
printResult _ = putStrLn "KO" -- Elems in second list

main :: IO ()
main = do
  args <- getArgs       -- Get list values
  commands <- getLine   -- Get commands
  printResult
    ( myEngine
        (verifList (createListTuple args))
        (words commands)
    )

--
--  Test Main
--

testCreateListTupleEmpty =
    TestCase (assertEqual "for (createListTuple []),"
    ([], [])
    (createListTuple []))

testCreateListTuple =
    TestCase (assertEqual "for (createListTuple [1 2 3 4 5]),"
    ([Just 1, Just 2, Just 3, Just 4, Just 5], [])
    (createListTuple ["1", "2", "3", "4", "5"]))

testCreateListTupleError =
    TestCase (assertEqual "for (createListTuple [1 2 3 4 5a]),"
    ([Just 1, Just 2, Just 3, Just 4, Nothing], [])
    (createListTuple ["1", "2", "3", "4", "5a"]))

testRedirectActionsNothing =
    TestCase (assertEqual "for (redirectActions ([Nothing], [Nothing])),"
    ([Nothing], [Nothing])
    (redirectActions ([Nothing], [Nothing]) "sa"))

testRedirectActionsWrongCommand =
    TestCase (assertEqual "for (redirectActions wrongCMD),"
    ([Nothing], [Nothing])
    (redirectActions ([Just 3, Just 2, Just 1], [Just 5]) "sab"))

testMyEngineNoMoreCmds =
    TestCase (assertEqual "for (myEngine ([Just 3, Just 2, Just 1], [Just 5]) []),"
    ([Just 3, Just 2, Just 1], [Just 5])
    (myEngine ([Just 3, Just 2, Just 1], [Just 5]) []))

testVerifListGood =
    TestCase (assertEqual "for (verifList ([Just 3, Just 2, Just 1], [Just 5])),"
    ([Just 3, Just 2, Just 1], [Just 5])
    (verifList ([Just 3, Just 2, Just 1], [Just 5])))

testVerifListBad =
    TestCase (assertEqual "for (verifList ([Just 3, Just 2, Nothing], [Just 5])),"
    ([Nothing], [Nothing])
    (verifList ([Just 3, Just 2, Nothing], [Just 5])))

testVerifListNothing =
    TestCase (assertEqual "for (verifList ([Nothing], [Nothing])),"
    ([Nothing], [Nothing])
    (verifList ([Nothing], [Nothing])))

testCaseOne =
    TestCase (assertEqual "for (myEngine ([Just 2, Just 1, Just 3, Just 6, Just 5, Just 8], []) [sa pb pb pb sa pa pa pa]),"
    ([Just 1, Just 2, Just 3, Just 5, Just 6, Just 8], [])
    (myEngine ([Just 2, Just 1, Just 3, Just 6, Just 5, Just 8], []) ["sa","pb","pb","pb","sa","pa","pa","pa"]))

testCaseTwo =
    TestCase (assertEqual "for (myEngine ([Just 2, Just 1, Just 3, Just 6, Just 5, Just 8], []) [sa pb pb pb]),"
    ([Just 6, Just 5, Just 8], [Just 3, Just 2, Just 1])
    (myEngine ([Just 2, Just 1, Just 3, Just 6, Just 5, Just 8], []) ["sa","pb","pb","pb"]))

testCaseAlreadySorted =
    TestCase (assertEqual "for (myEngine ([Just 1, Just 2, Just 3, Just 5, Just 5, Just 8], []) []),"
    ([Just 1, Just 2, Just 3, Just 5, Just 5, Just 8], [])
    (myEngine ([Just 1, Just 2, Just 3, Just 5, Just 5, Just 8], []) []))

testsMain = TestList
    [testCreateListTupleEmpty, testCreateListTuple,
    testCreateListTupleError, testRedirectActionsNothing,
    testRedirectActionsWrongCommand, testMyEngineNoMoreCmds,
    testVerifListGood, testVerifListBad, testCaseOne,
    testCaseTwo, testCaseAlreadySorted, testVerifListNothing]