-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2020 Arnau Abella
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Arnau Abella <arnauabella@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Main (main) where

import System.IO
import Test.Hspec
import Test.OPL

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetBuffering stdout NoBuffering
  hspec allUnitTests

allUnitTests :: Spec
allUnitTests = describe "Heuristics" $ do
  oplSpec
