module Main where

import Test.DocTest

main = doctest ["-isrc", "-XOverloadedStrings", "src/System/Posix/FilePath"]
