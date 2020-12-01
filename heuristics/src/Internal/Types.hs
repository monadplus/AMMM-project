{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Internal.Types where

-----------------------------------------------------------------

import Lens.Micro.Platform
import Data.Coerce

-----------------------------------------------------------------

data Location = Location
  { _x :: {-# UNPACK #-} !Int,
    _y :: {-# UNPACK #-} !Int
  } deriving stock (Show, Eq, Ord)

newtype Population = Population {_population :: Int}
   deriving newtype (Show, Eq, Ord)

data City = City
  { _cLocation :: Location,
    _cPopulation :: Population
  }  deriving stock (Show, Eq)

-- | Logistic Center Type
data FacilityType = FacilityType
  { _dCity :: Double,
    _cap :: Int,
    _cost :: Int
  } deriving stock (Show, Eq)

-- NOTE (if needed) change to https://hackage.haskell.org/package/array-0.5.4.0/docs/Data-Array.html
data Problem = Problem
  { _cities :: [City],
    _facilitiesLocation :: [Location],
    _facilityTypes :: [FacilityType],
    _dCenter :: Double
  } deriving stock (Show, Eq)

data Facility = Facility
  { _fLocation :: Location,
    _fType :: FacilityType
  } deriving stock (Show, Eq)

data Assignment = Assignment
  { _city :: City
  , _primary :: Facility
  , _secondary :: Facility
  } deriving stock (Show, Eq)

data Solution = Solution
  { _assignments :: Assignment
  , _facilities :: Facility
  , _objectiveFunction :: Double -- Total Cost
  }  deriving stock (Show, Eq)

data Grid = Grid
  { _gridX :: Int,
    _gridY :: Int
  }

newtype Distance = Distance { _distance :: Double }
  deriving newtype (Num, Eq, Ord)

makeLenses ''Location
makeLenses ''Population
makeLenses ''City
makeLenses ''FacilityType
makeLenses ''Problem
makeLenses ''Facility
makeLenses ''Assignment
makeLenses ''Solution
makeLenses ''Grid
makeLenses ''Distance

pow2 :: Int -> Double
pow2 x = fromIntegral $ x ^ (2 :: Int)

diagonal :: Grid -> Double
diagonal (Grid x y) = sqrt ((pow2 x) + (pow2 y))
  where
    pow2 x = fromIntegral $ x ^ (2 :: Int) :: Double

-- | Rectangular 'Grid'
--
-- Property: gridX*gridY / n = density
--           where n = "expected number of elements in the grid"
rectGrid :: Double -> Int -> Grid
rectGrid density n =
  let x = floor . sqrt $ fromIntegral n / density
   in Grid x x

euclideanDistance :: Location -> Location -> Distance
euclideanDistance l1 l2 = coerce $ sqrt (f x + f y)
  where
    f l = pow2 (l2^.l - l1^.l)
