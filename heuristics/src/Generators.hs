{-# LANGUAGE TemplateHaskell #-}

module Generators where

-----------------------------------------------------------------

import Lens.Micro.Platform
import qualified Data.ByteString.Builder as BBS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS

-----------------------------------------------------------------
-----------------------------------------------------------------

data Location = Location
  { _x :: {-# UNPACK #-} !Int,
    _y :: {-# UNPACK #-} !Int
  }

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

data Facility = Facility
  {_lLocation :: Location}

-- TODO change to https://hackage.haskell.org/package/array-0.5.4.0/docs/Data-Array.html
--      if needed
data Problem = Problem
  { _cities :: [City],
    _facilities :: [Facility],
    _facilityType :: [FacilityType],
    _dCenter :: Double
  }

makeLenses ''Location
makeLenses ''Population
makeLenses ''City
makeLenses ''FacilityType
makeLenses ''Facility
makeLenses ''Problem

---------------------------------------------------------------------
---------------------------------------------------------------------

{- TODO

- The idea is to create Gen that dependent on the size given.
  For example, arrays should be of size [n-epsilon, n+epsilon]

- Then this calls to the gen and samples one
-}

-- | Generates a sample of size n
sample :: Int -> IO Problem
sample n = undefined

---------------------------------------------------------------------
---------------------------------------------------------------------

{- Expected Input File

nLocations = 5;
nCities = 10;
nTypes = 3;
posCities = [ [1 1] [2 3] [0 1] [4 1] [1 2] [2 2] [0 1] [1 1] [3 4] [2 4] ];
posLocations = [ [2 3] [1 2] [1 1] [0 2] [1 3] ];
p = [ 5 3 6 1 2 5 2 3 4 1 ];
d_city = [2 4 7];
cap = [18 14 5];
cost = [50 45 15];
d_center = 1.2;
-}

{- TODO

- A class that transform lists, each newtype/data, etc to its OPL .dat representation
- Use String.Builder to build https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Builder.html
- Construct manually each entry + toOPL
-}

class OPL a where
  toOPL :: a -> ByteString

instance OPL Problem where
  toOPL p = undefined

instance OPL Location where
  toOPL = undefined

instance OPL a => OPL [a] where
  toOPL = undefined
