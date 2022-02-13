module Main where

import Data.List (sort)
import Data.Maybe (isJust)
import PushswapFunctions
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
  if length x == length (filter isJust x) &&
    length y == length (filter isJust y)
    then (x, y)
    else ([Nothing], [Nothing])

-- Prints OK or KO if there was no input error, depending
-- on if the list is sorted, exits 84 otherwise
printResult :: ([Maybe Int], [Maybe Int]) -> IO ()
printResult ([Nothing], [Nothing]) = exitWith (ExitFailure 84) -- Input error
printResult (x, y) = do
    print (x, y)
    putStrLn "Enter command to modify list, enter otherwise"
    newCmdSet <- getLine
    if newCmdSet /= "" then printResult (myEngine (x, y) (words newCmdSet))
    else printMeTheResult (x, y)

printMeTheResult :: ([Maybe Int], [Maybe Int]) -> IO ()
printMeTheResult (x, y) = if sort x == x && null y
        then putStrLn "OK" -- Good case
        else putStrLn "KO" -- Unordered list

main :: IO ()
main = do
  args <- getArgs       -- Get list values
  let list = verifList (createListTuple args)
  if list == ([Nothing], [Nothing])
    then putStrLn "input error" >> exitWith (ExitFailure 84)
    else putStrLn "Enter command or set of commands"
  commands <- getLine   -- Get commands
  printResult (myEngine list (words commands))