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
    let f = decodeUtf8OPL . encodeUtf8OPL
    problem <- sample 20
    LC.putStrLn . encodeUtf8OPL $ problem
    f problem `shouldBe` problem

  it "should not throw parsing the examples" $ do
    lbs <- L.readFile "examples/sample_30.dat"
    let r = decodeUtf8OPL lbs
    r^.cities.to length `shouldBe` 20
    r^.facilitiesLocation.to length `shouldBe` 10
    r^.facilityTypes.to length `shouldBe` 12
