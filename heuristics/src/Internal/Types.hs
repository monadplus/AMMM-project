{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingVia #-}

module Internal.Types where

-----------------------------------------------------------------

import Data.Coerce
import Data.Generics.Product.Types
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro.Platform
import Data.Word
import Data.Semigroup(Sum(..))

-----------------------------------------------------------------

-- | Metaheuristic algorithm type.
data Algorithm
  = Greedy (Maybe LocalSearchStrategy)
  | GRASP
  deriving stock Show

data LocalSearchStrategy
  = FirstImprovement
  | BestImprovement
  deriving stock Show

-- | Cost of the facility
type Cost = Int

-- | Facility occupancy
type Occupancy = Double

-- | Minimum distance between facilities
type MinDistLoc = Double

newtype Id = Id { _unsafeId :: Word64 }
  deriving newtype (Show, Eq, Ord, Num, Enum)

data Location = Location
  { _x :: {-# UNPACK #-} !Int,
    _y :: {-# UNPACK #-} !Int
  }
  deriving stock (Show, Eq, Ord, Generic)

newtype Population = Population {_population :: Int}
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Enum, Num, Real, Integral)

data City = City
  { _cId :: Id,
    _cLocation :: Location,
    _cPopulation :: Population
  }
  deriving stock (Show, Eq, Ord, Generic)

-- | Logistic Center Type
data FacilityType = FacilityType
  { _dCity :: Double,
    _cap :: Int,
    _cost :: Cost
  }
  deriving stock (Show, Eq, Ord, Generic)

data Problem = Problem
  { _cities :: [City],
    _facilitiesLocation :: [Location],
    _facilityTypes :: [FacilityType],
    _dCenter :: MinDistLoc
  }
  deriving stock (Show, Eq)

data Tier = Primary | Secondary
  deriving stock (Show, Eq, Ord, Generic)

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

newtype Distance = Distance {_distance :: Double}
  deriving newtype (Show, Num, Eq, Ord, Fractional)
  deriving (Semigroup, Monoid) via (Sum Double)

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

getOccupancy :: Tier -> City -> Occupancy
getOccupancy Primary = fromIntegral . view cPopulation
getOccupancy Secondary = (0.1*) . fromIntegral . view cPopulation

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
    f l = pow2 (l2 ^. l - l1 ^. l)

computeObjectiveValue :: Solution -> Double
computeObjectiveValue solution =
  Set.foldl'
    (\acc facility -> acc + facilityCost facility)
    (0.0 :: Double)
    (Set.fromList facilities)
  where
    assignments :: [Assignment]
    assignments = Map.elems (coerce solution)

    facilities :: [Facility]
    facilities = concat (toListOf (types @Facility) <$> assignments)

    facilityCost :: Facility -> Double
    facilityCost = view (fType . cost . to fromIntegral)
