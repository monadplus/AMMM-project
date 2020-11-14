module Internal.OPL where

----------------------------------------------

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Internal.Types
import Lens.Micro.Platform

----------------------------------------------

class OPL a where
  toOPL :: a -> Builder

instance OPL Int where
  toOPL = B.intDec

instance OPL Double where
  toOPL = B.doubleDec

instance (OPL a) => OPL [a] where
  toOPL xs =
    B.charUtf8 '['
      <> mconcat [B.charUtf8 ' ' <> toOPL x | x <- xs]
      <> B.stringUtf8 " ]"

instance OPL Location where
  toOPL loc =
    B.charUtf8 '['
      <> loc ^. x . to toOPL
      <> B.charUtf8 ' '
      <> loc ^. y . to toOPL
      <> B.charUtf8 ']'

instance OPL Population where
  toOPL p = p ^. population . to toOPL

-- | encodeUtf8OPL
--
-- >>> encodeUtf8OPL p
-- nLocations = 5;
-- nCities = 10;
-- nTypes = 3;
-- posCities = [ [1 1] [2 3] [0 1] [4 1] [1 2] [2 2] [0 1] [1 1] [3 4] [2 4] ];
-- posLocations = [ [2 3] [1 2] [1 1] [0 2] [1 3] ];
-- p = [ 5 3 6 1 2 5 2 3 4 1 ];
-- d_city = [2 4 7];
-- cap = [18 14 5];
-- cost = [50 45 15];
-- d_center = 1.2;
encodeUtf8OPL :: Problem -> L.ByteString
encodeUtf8OPL = B.toLazyByteString . problemBuilder
  where
    problemBuilder :: Problem -> Builder
    problemBuilder p =
      lineBuilder "nLocations" (lengthOf facilitiesLocation p)
        <> lineBuilder "nCities" (lengthOf cities p)
        <> lineBuilder "nTypes" (lengthOf facilityTypes p)
        <> lineBuilder "posCities" (p ^.. cities . folded . cLocation)
        <> lineBuilder "posLocations" (p ^. facilitiesLocation)
        <> lineBuilder "p" (p ^.. cities . folded . cPopulation)
        <> lineBuilder "d_city" (p ^.. facilityTypes . folded . dCity)
        <> lineBuilder "cap" (p ^.. facilityTypes . folded . cap)
        <> lineBuilder "cost" (p ^.. facilityTypes . folded . cost)
        <> lineBuilder "d_center" (p ^. dCenter)

    lineBuilder :: OPL a => String -> a -> Builder
    lineBuilder name x =
      B.stringUtf8 name
        <> B.stringUtf8 " = "
        <> toOPL x
        <> B.charUtf8 ';'
        <> B.charUtf8 '\n'

    lengthOf l = length . toListOf (l.folded)
