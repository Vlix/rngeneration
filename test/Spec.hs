{-# LANGUAGE OverloadedStrings #-}
module Main where


import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Yaml (decodeFileEither)
import Test.Tasty as Tasty
import Test.Tasty.HUnit

import RNGeneration


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = testGroup "RNGeneration"
    [ parseTests
    , smushTests
    ]

parseTests :: TestTree
parseTests = testGroup "Parsing"
    [ parseTest "Req 1" "test0.yml" $ mkReq "A"
    , parseTest "Req List" "test1.yml" $ mkReq "A" :&& mkReq "B"
    , parseTest "IMPOSSIBLE" "test2.yml" $ IMPOSSIBLE
    , parseTest "Choice" "test3.yml" $ mkReq "A" :|| IMPOSSIBLE :|| (mkReq "B" :&& mkReq "C")
    , parseTest "AND/OR" "test4.yml" $ mkReq "A" :&& (mkReq "B" :|| mkReq "C")
    , parseTest "Complex" "test5.yml" $ (mkReq "A" :&& IMPOSSIBLE :&& (mkReq "B" :|| mkReq "C"))
                                    :&& (mkReq "D" :|| mkReq "E")
    ]

parseTest :: String -> FilePath -> Requirement Text -> TestTree
parseTest s fp = parseTest' s fp . assertEqual "wrong requirement"

parseTest' :: String -> FilePath -> (Requirement Text -> Assertion) -> TestTree
parseTest' s fp f = testCase s $ decodeFileEither ("test/" <> fp) >>= either go f
  where go e = error $ fp <> ": did not parse: " <> show e

smushTest :: String -> FilePath -> Requirement Text -> TestTree
smushTest s fp req = parseTest' s fp $ assertEqual "wrong outcome" req . smushReq

smushTests :: TestTree
smushTests = testGroup "Smushing"
    [ smushTest "Req List" "test1.yml" $ mkReqs ["A","B"]
    , smushTest "Choice" "test3.yml" $ mkReq "A" :|| mkReqs ["B","C"]
    , smushTest "Complex" "test5.yml" IMPOSSIBLE
    , smushTest "Reduced (easy)" "test6.yml" $ mkReq "B" :&& (mkReq "A" :|| mkReq "C")
    , smushTest "Reduced (hard)" "test7.yml" test7Req
    ]

test7Req :: Requirement Text
test7Req = mkReq "Z" :&& (mkReq "E" :|| (mkReqs ["A","B"] :|| mkReqs ["C","D"]))

mkReq :: (Hashable a, Eq a) => a -> Requirement a
mkReq a = mkReqs [a]

mkReqs :: (Hashable a, Eq a) => [a] -> Requirement a
mkReqs = Req . HS.fromList
