module Tests where

import Test.HUnit
import PushswapFunctions

--
-- swapList
--
testSwapList =
    TestCase (assertEqual "for (swapList [Just 1, Just 2, Just 3]),"
    [Just 2,Just 1,Just 3]
    (swapList [Just 1, Just 2, Just 3]))

testSwapListEmpty =
    TestCase (assertEqual "for (swapList []),"
    []
    (swapList []))

testSwapListOneElem =
    TestCase (assertEqual "for (swapList [Just 1]),"
    [Just 1]
    (swapList [Just 1]))

--
-- rotateList
--

testRotateList =
    TestCase (assertEqual "for (rotateList [Just 1, Just 2, Just 3]),"
    [Just 2,Just 3,Just 1]
    (rotateList [Just 1, Just 2, Just 3]))

testRotateEmpty =
    TestCase (assertEqual "for (rotateList []),"
    []
    (rotateList []))

testRotateOneElem =
    TestCase (assertEqual "for (rotateList [Just 1]),"
    [Just 1]
    (rotateList [Just 1]))

--
--  reverseRotateList
--

testRRotateList =
    TestCase (assertEqual "for (reverseRotateList [Just 1, Just 2, Just 3]),"
    [Just 3,Just 1,Just 2]
    (reverseRotateList [Just 1, Just 2, Just 3]))

testRRotateEmpty =
    TestCase (assertEqual "for (reverseRotateList []),"
    []
    (reverseRotateList []))

testRRotateOneElem =
    TestCase (assertEqual "for (reverseRotateList [Just 1]),"
    [Just 1]
    (reverseRotateList [Just 1]))

--
--  putList
--

testPutListA =
    TestCase (assertEqual "for (putInList ([Just 1, Just 2],[Just 3]) False),"
    ([Just 3, Just 1, Just 2], [])
    (putInList ([Just 1, Just 2],[Just 3]) False))

testPutListB =
    TestCase (assertEqual "for (putInList ([Just 1, Just 2],[Just 3]) True),"
    ([Just 2], [Just 1, Just 3])
    (putInList ([Just 1, Just 2],[Just 3]) True))

testPutListAEmpty =
    TestCase (assertEqual "for (putInList ([Just 1, Just 2],[]) False),"
    ([Just 1, Just 2], [])
    (putInList ([Just 1, Just 2],[]) False))

testPutListBEmpty =
    TestCase (assertEqual "for (putInList ([],[Just 2]) True),"
    ([], [Just 2])
    (putInList ([],[Just 2]) True))

--
--  Tests
--

tests =
    TestList    [testSwapList, testSwapListEmpty, testSwapListOneElem,
                testRotateList, testRotateEmpty, testRotateOneElem,
                testRRotateList, testRRotateEmpty, testRRotateOneElem,
                testPutListA, testPutListB, testPutListAEmpty,
                testPutListBEmpty]
