{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Internal.Greedy where

------------------------------------------------

import Lens.Micro.Platform
import Internal.Types
import Data.Function
import Data.List
import Data.Ord
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy
import Data.Maybe (catMaybes)
import Data.Coerce

------------------------------------------------

type Cost = Int
type Occupacy = Double

-- | As Primary or Secondary city
data As = As
  { _aType :: FacilityType
  , _aPrimary :: [City]
  , _aSecondary :: [City]
  }

-- | Partial Assignment
newtype PA = PA { _pa :: Map Location As}
makeLenses ''PA
makeLenses ''As

emptyPA :: PA
emptyPA = PA Map.empty

sortByPopulationDecr :: [City] -> [City]
sortByPopulationDecr = sortBy (compare `on` view (cPopulation . to Down))

-- TODO
toSolution :: PA -> Solution
toSolution = undefined

data Tier = Primary | Secondary

tierToLens :: Tier -> Lens As As [City] [City]
tierToLens Primary = aPrimary
tierToLens Secondary = aSecondary

data Cause = Distance | Capacity

{- | 'Nothing' if the assignment is not feasible. Otherwise, the cost difference.

Constraints:

* primary facility != secondary facility
* min. distance between facility's locations
* city - primary facility - at most dCity
* city - secondary facility - at most 3*dCity
* Capacity of a center >= sum(population primary) + 0.1*sum(population secondary)

Cost of a location:
 * If the location is infeasible, return infinity
 * If the location has a logistic center, within a feasible distance and the capacity of the location - the population is > 0, return 0 cost
 * If the location doesn't has a logistic center or the current does not fullfil the constrants, then add/upgrade the one with the smallest cost that satisfies the distance and population constraint
-}
computeCost
  :: Tier -> PA -> City -> [FacilityType]
  -> Location -> Maybe (Cost, Location, PA)
computeCost tier (PA w) c fTypes l =
  case Map.lookup l w of
    Nothing -> do
      undefined --TODO
      -- Check min distance
      -- Pick the smallest cost, with enough distance and capacity
    Just info@(As (FacilityType maxDist maxCap cost) _ _) -> do
      let currentOccupacy = getOccupacy info
          hasCapacity = currentOccupacy <= (fromIntegral maxCap)
      case (hasRange (coerce maxDist), hasCapacity) of
        (True, True) -> do
          let updated = coerce $ Map.adjust (tierToLens tier <>~ [c]) l w
          Just (0, l, updated)
        (b1, b2) -> tryToUpdate b1 b2 currentOccupacy

  where
    dist :: Distance
    dist = euclideanDistance (c^.cLocation) l

    hasRange :: Distance -> Bool
    hasRange maxDist = case tier of
      Primary -> dist <= maxDist
      Secondary -> dist <= 3*maxDist

    getOccupacy :: As -> Occupacy
    getOccupacy info =
      let getPop l = info^..l.folded.cPopulation.population
          sumOf = fromIntegral . sum
          primaryPop = sumOf (getPop aPrimary)
          secondaryPop = sumOf (getPop aSecondary)
          cityPop = fromIntegral (c^.cPopulation.population)
       in case tier of
        Primary -> (cityPop + primaryPop) + 0.1*secondaryPop
        Secondary -> primaryPop + 0.1*(secondaryPop + cityPop)

    -- range capacity
    tryToUpdate :: Bool -> Bool -> Occupacy -> Maybe (Cost, Location, PA)
    tryToUpdate True False occupacy = undefined
    tryToUpdate False True occupacy = undefined
    tryToUpdate False False occupacy = undefined
    tryToUpdate _ _ _ = error "Unreachable"


pickBestAndUpdate
  :: MonadState PA m
  => [(Cost, Location, PA)] -> m Location
pickBestAndUpdate candidates = do
  let (_, location, newPA) = minimumBy (compare `on` view _1) candidates
  put newPA
  return location

assignBest :: MonadState PA m
  => Tier -> City -> [FacilityType] -> [Location] -> m Location
assignBest t c ft locations = do
  w <- get
  pickBestAndUpdate $
    catMaybes (computeCost t w c ft <$> locations)

runProblem :: State PA a -> Solution
runProblem = toSolution . flip execState emptyPA

greedy :: Problem -> Solution
greedy problem = runProblem $ do
  let ft = problem^.facilityTypes
      locations = problem^.facilitiesLocation
  forM (problem^.cities. to sortByPopulationDecr) $ \c -> do
    primary <- assignBest Primary c ft locations
    let locations' = filter (/= primary) locations     -- Primary != Secondary
    void $ assignBest Secondary c ft locations'
