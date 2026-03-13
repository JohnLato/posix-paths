{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import System.Posix.Directory.Traversals

import Test.DocTest
import Test.HUnit

main :: IO ()
main = do
    doctest
      [ "-isrc"
      , "-XOverloadedStrings"
      , "src/System/Posix/FilePath"
      ]
    _ <- runTestTT unitTests
    pure ()

unitTests :: Test
unitTests = test
    [ TestCase $ do
        a <- allDirectoryContents "."
        b <- allDirectoryContents' "."
        assertEqual "allDirectoryContents == allDirectoryContents'" a b
    ]
