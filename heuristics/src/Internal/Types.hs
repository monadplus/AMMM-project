{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}

module Internal.Types where

-----------------------------------------------------------------

import Lens.Micro.Platform

-----------------------------------------------------------------

data Location = Location
  { _x :: {-# UNPACK #-} !Int,
    _y :: {-# UNPACK #-} !Int
  } deriving stock (Eq)

newtype Population = Population {_population :: Int}

data City = City
  { _cLocation :: Location,
    _cPopulation :: Population
  }

-- | Logistic Center Type
data FacilityType = FacilityType
  { _dCity :: Double,
    _cap :: Int,
    _cost :: Int
  }

-- NOTE (if needed) change to https://hackage.haskell.org/package/array-0.5.4.0/docs/Data-Array.html
data Problem = Problem
  { _cities :: [City],
    _facilitiesLocation :: [Location],
    _facilityTypes :: [FacilityType],
    _dCenter :: Double
  }

data Facility = Facility
  { _fLocation :: Location,
    _fType :: FacilityType
  }

data Grid = Grid
  { _gridX :: Int,
    _gridY :: Int
  }

makeLenses ''Location
makeLenses ''Population
makeLenses ''City
makeLenses ''FacilityType
makeLenses ''Facility
makeLenses ''Problem
makeLenses ''Grid

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
