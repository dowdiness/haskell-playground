{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit as HU

import Lib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "My Project Tests"
  [ unitTests ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ HU.testCase "Parse URI Test" $
      let uri = case parseUri "http://mark:secret@example.com" of
            Right a -> a
            Left err -> error $ "Unexpected error: " ++ show err
      in uri @?= createUri
        SchemeHttp
        (Just (createAutority (Just ("mark","secret")) "example.com" Nothing))
        (Just (createPath Nothing Nothing Nothing)),
    HU.testCase "Parse URI with Query Test" $
      let uri = case parseUri "https://example.com/path?query=1" of
            Right a -> a
            Left err -> error $ "Unexpected error: " ++ show err
      in uri @?= createUri
        SchemeHttps
        (Just (createAutority Nothing "example.com" Nothing))
        (Just (createPath (Just "/path") (Just (createQuery "query" "1")) Nothing))
  ]
