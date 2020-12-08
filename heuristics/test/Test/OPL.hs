-----------------------------------------------------------------------------
-- |
-- Module      :  Test.OPL
-- Copyright   :  (C) 2020 Arnau Abella
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Arnau Abella <arnauabella@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Test.OPL where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Internal.Generators
import Internal.OPL
import Internal.Types
import Test.Hspec
import Lens.Micro.Platform

oplSpec :: Spec
oplSpec =
  describe "OPL Tests" $ do
    decodeSpec

decodeSpec :: Spec
decodeSpec = describe "decodeUtf8OPL" $ do
  it "decodeUtf8OPL . encodeUtf8OPL == id" $ do
    problem <- sample 20
    print problem
    let result = decodeUtf8OPL . encodeUtf8OPL $ problem
    checkProblem result problem

  -- it "should not throw parsing the examples" $ do
  --   lbs <- L.readFile "examples/sample_30.dat"
  --   let r = decodeUtf8OPL lbs
  --   r^.cities.to length `shouldBe` 20
  --   r^.facilitiesLocation.to length `shouldBe` 10
  --   r^.facilityTypes.to length `shouldBe` 12

checkProblem :: Problem -> Problem -> Expectation
checkProblem result expected = do
  result^.cities `shouldMatchList` expected^.cities
  result^.facilitiesLocation `shouldMatchList` expected^.facilitiesLocation
  result^.dCenter `shouldBe` expected^.dCenter
