{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Internal.Types
-- Copyright   :  (C) 2020 Arnau Abella
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Arnau Abella <arnauabella@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Internal.Types where

import Data.Coerce
import Data.Generics.Product.Types
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup (Sum (..))
import qualified Data.Set as Set
import Data.Word
import GHC.Generics (Generic)
import Lens.Micro.Platform

data LocalSearchStrategy
  = -- | Stop as soon as you find an improvement
    FirstImprovement
  | -- | Find all improvement and pick the best one
    BestImprovement
  deriving stock (Show)

data Tier = Primary | Secondary
  deriving stock (Show, Eq, Ord, Generic)

newtype Alpha = Alpha {_alpha :: Double}
  deriving newtype (Show, Read, Eq, Ord, Num, Fractional)

newtype Seconds = Seconds {_seconds :: Word64}
  deriving newtype (Show, Read, Eq, Ord, Num)

newtype Cost = Cost { _unsafeCost :: Int }
  deriving newtype (Show, Eq, Ord, Enum, Num, Real, Integral)
  deriving (Semigroup, Monoid) via (Sum Int)

newtype Id = Id {_unsafeId :: Word64}
  deriving newtype (Show, Eq, Ord, Num, Enum)

newtype Occupancy = Occupancy { _unsafeOccupancy :: Double }
  deriving newtype (Show, Eq, Ord, Enum, Num, Real, Fractional)
  deriving (Semigroup, Monoid) via (Sum Double)

newtype Population = Population {_population :: Int}
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Enum, Num, Real, Integral)
  deriving (Semigroup, Monoid) via (Sum Int)

newtype Distance = Distance {_distance :: Double}
  deriving newtype (Show, Num, Eq, Ord, Fractional)
  deriving (Semigroup, Monoid) via (Sum Double)

-- | Discrete 'Location' in a discrete 'Grid'.
data Location = Location
  { _x :: {-# UNPACK #-} !Int,
    _y :: {-# UNPACK #-} !Int
  }
  deriving stock (Show, Eq, Ord, Generic)

-- | Metaheuristic algorithm
data Algorithm
  = Greedy
      { -- | 'Nothing' to disable Local Search
        _strategy :: Maybe LocalSearchStrategy
      }
  | GRASP
      { _threshold :: Alpha,
        _timeLimit :: Seconds
      }
  deriving stock (Show)

data City = City
  { _cId :: Id,
    _cLocation :: Location,
    _cPopulation :: Population
  }
  deriving stock (Show, Eq, Ord, Generic)

-- | Logistic Center Type
data FacilityType = FacilityType
  { _dCity :: Distance,
    _cap :: Int,
    _cost :: Cost
  }
  deriving stock (Show, Eq, Ord, Generic)

data Problem = Problem
  { _cities :: [City],
    _facilitiesLocation :: [Location],
    _facilityTypes :: [FacilityType],
    _dCenter :: Distance
  }
  deriving stock (Show, Eq)

data Facility = Facility
  { _fLocation :: Location,
    _fType :: FacilityType,
    _tier :: Tier
  }
  deriving stock (Show, Eq, Ord, Generic)

data Assignment = Assignment
  { _primary :: Facility,
    _secondary :: Facility
  }
  deriving stock (Show, Eq, Generic)

newtype Solution = Solution
  { _assignments :: Map City Assignment
  }
  deriving stock (Show)

data Grid = Grid
  { _gridX :: Int,
    _gridY :: Int
  }

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
makeLenses ''Algorithm -- Notice this generates Traversal' for lenses that do not appear in all cases.
makeLenses ''Alpha
makeLenses ''Seconds
makeLenses ''Cost
makeLenses ''Occupancy

getOccupancy :: Tier -> City -> Occupancy
getOccupancy Primary = fromIntegral . view cPopulation
getOccupancy Secondary = (0.1 *) . fromIntegral . view cPopulation

pow2 :: Int -> Double
pow2 x = fromIntegral $ x ^ (2 :: Int)

diagonal :: Grid -> Double
diagonal (Grid x y) = sqrt ((pow2 x) + (pow2 y))
  where
    pow2 x = fromIntegral $ x ^ (2 :: Int) :: Double

-- | Rectangular 'Grid'
--
-- prop> (gridX*gridY)/population = density
rectGrid :: Double -> Int -> Grid
rectGrid density population
  | density <= 0 && density > 1 = error "0 < density <= 1"
  | otherwise =
    let x = floor $ sqrt ((fromIntegral population) / density)
     in Grid x x

doubleGrid :: Grid -> Grid
doubleGrid = (gridX %~ (* 2)) . (gridY %~ (* 2))

euclideanDistance :: Location -> Location -> Distance
euclideanDistance l1 l2 = coerce $ sqrt (f x + f y)
  where
    f l = pow2 (l2 ^. l - l1 ^. l)

computeObjectiveValue :: Solution -> Cost
computeObjectiveValue solution =
  Set.foldl'
    (\acc facility -> acc + facilityCost facility)
    (0 :: Cost)
    (Set.fromList facilities)
  where
    assignments :: [Assignment]
    assignments = Map.elems (coerce solution)

    facilities :: [Facility]
    facilities = concat (toListOf (types @Facility) <$> assignments)

    facilityCost :: Facility -> Cost
    facilityCost = view (fType . cost)
