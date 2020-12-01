{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Internal.OPL where

----------------------------------------------

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Coerce
import Internal.Types
import Lens.Micro.Platform
import Text.Printf

----------------------------------------------

class OPL a where
  toOPL :: a -> Builder
  fromOPL :: L.ByteString -> a
  default fromOPL :: Read a => L.ByteString -> a
  fromOPL = read @a . LC.unpack

instance OPL Int where
  toOPL = B.intDec

instance OPL Double where
  toOPL = B.doubleDec

instance (OPL a) => OPL [a] where
  toOPL xs =
    B.charUtf8 '['
      <> mconcat [B.charUtf8 ' ' <> toOPL x | x <- xs]
      <> B.stringUtf8 " ]"

  fromOPL lbs =
    let (content, _) = (takeUntil ']' . takeChar '[') lbs
        splitContent = (LC.split ' ' content)^.. each . filtered (not . LC.null)
     in fromOPL <$> splitContent

instance {-# OVERLAPPING #-} OPL [Location] where
  -- | copied to avoid infinite loop
  toOPL xs =
    B.charUtf8 '['
      <> mconcat [B.charUtf8 ' ' <> toOPL x | x <- xs]
      <> B.stringUtf8 " ]"

  -- TODO Parsing this is not so easy without couting [ [5 2] [4 2] [2 1] [0 1] [1 0] [0 2] ]
  fromOPL =
    let toLocations lbs
          | LC.notElem '[' lbs = [] -- hack: If [ does not appear we are finished
          | otherwise =
              let (x, rem) = takeUp ']' lbs
               in x : toLocations rem
     in (fmap fromOPL) . toLocations . takeChar '['

instance OPL Location where
  toOPL loc =
    B.charUtf8 '['
      <> loc ^. x . to toOPL
      <> B.charUtf8 ' '
      <> loc ^. y . to toOPL
      <> B.charUtf8 ']'

  fromOPL lbs =
    let (x : y : []) = fromOPL @[Int] lbs
     in Location x y

instance OPL Population where
  toOPL p = p ^. population . to toOPL

  fromOPL = coerce . fromOPL @Int

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

    lengthOf l = length . toListOf (l . folded)

-- | decodeUtf8OPL
--
-- >>> decodeUtf8OPL . encodeUtf8OPL == id
decodeUtf8OPL :: L.ByteString -> Problem
decodeUtf8OPL lbs = Problem {..}
  where
    lines :: [L.ByteString]
    lines = LC.lines lbs

    getLine :: L.ByteString -> L.ByteString
    getLine key =
      let xs = LC.span (/= ' ') <$> lines
       in case filter ((== key) . fst) xs of
            [(_, rem)] -> rem
            _ -> error $ printf "Key %s not found." (LC.unpack key)

    getLineContent :: OPL a => L.ByteString -> a
    getLineContent =
      fromOPL
        . LC.takeWhile (/= ';') -- drop ';' and comments
        . LC.dropWhile (== ' ')
        . LC.tail
        . LC.dropWhile (/= '=')
        . getLine

    posCities :: [Location]
    posCities = getLineContent "posCities"

    _facilitiesLocation :: [Location]
    _facilitiesLocation = getLineContent "posLocations"

    population :: [Population]
    population = getLineContent "p"

    d_city :: [Double]
    d_city = getLineContent "d_city"

    cap :: [Int]
    cap = getLineContent "cap"

    cost :: [Int]
    cost = getLineContent "cost"

    _dCenter :: Double
    _dCenter = getLineContent "d_center"

    _cities :: [City]
    _cities = uncurry City <$> zip posCities population

    _facilityTypes :: [FacilityType]
    _facilityTypes = uncurry3 FacilityType <$> zip3 d_city cap cost

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

takeChar :: Char -> L.ByteString -> L.ByteString
takeChar c lbs
  | c' == c = rem
  | otherwise = error $ printf "Expecting %c but got %c" c c'
  where
    Just (c', rem) = LC.uncons $ LC.dropWhile (== ' ') lbs

-- | The char is dropped at the remaining part.
takeUntil :: Char -> L.ByteString -> (L.ByteString, L.ByteString)
takeUntil c lbs =
  let (xs, rem) = LC.span (/= c) lbs
   in (xs, takeChar c rem)

-- | 'takeUntil' including the character
takeUp :: Char -> L.ByteString -> (L.ByteString, L.ByteString)
takeUp c lbs =
  let (xs, rem) = takeUntil c lbs
   in (LC.snoc xs c, rem)
