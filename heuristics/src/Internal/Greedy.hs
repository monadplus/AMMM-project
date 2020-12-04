{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Internal.Greedy where

------------------------------------------------

import Control.Exception
import Control.Monad
import Control.Monad.State.Lazy
import Data.Coerce
import Data.Function
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Ord
import Internal.Types
import Lens.Micro.Platform
import Text.Printf
-- import Debug.Trace

------------------------------------------------

data Infeasible = Infeasible
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Cost of the facility
type Cost = Int

-- | Facility occupancy
type Occupacy = Double

-- | Minimum distance between facilities
type MinDistLoc = Double

-- | Information related to location
data LocationInfo = LocationInfo
  { _aType :: FacilityType,
    _aPrimary :: [City],
    _aSecondary :: [City]
  }
  deriving stock Show

-- | Partial Assignment
newtype PA = PA {_pa :: Map Location LocationInfo}
  deriving stock Show

makeLenses ''LocationInfo
makeLenses ''PA

-- | Empty Partial Assignment
emptyPA :: PA
emptyPA = PA Map.empty

-- | From a Partial Assignment to a Solution
--
-- This is just data manipulation
toSolution :: PA -> Solution
toSolution =
  coerce
    . Map.map toAssignment
    . Map.foldlWithKey' go Map.empty
    . coerce
  where
    go :: Map City [Facility] -> Location -> LocationInfo -> Map City [Facility]
    go dict l info =
      foldl'
        (reduce Secondary)
        (foldl' (reduce Primary) dict $ info ^. aPrimary)
        (info ^. aSecondary)
      where
        reduce t dict c =
          let facility = Facility l (info ^. aType) t
           in Map.alter (Just . maybe [facility] (facility :)) c dict

    -- Throws unless the assignment has a primary and a secondary facility.
    toAssignment :: [Facility] -> Assignment
    toAssignment (f1 : f2 : []) =
      case (f1 ^. tier, f2 ^. tier) of
        (Primary, Secondary) -> Assignment f1 f2
        (Secondary, Primary) -> Assignment f2 f1
        _ -> error "Expecting a primary and a secondary facility"
    toAssignment xs = error $ printf "Expecting 2 facilities but got %d" (length xs)

-- | Tier lens like
tierToLens :: Tier -> Lens LocationInfo LocationInfo [City] [City]
tierToLens Primary = aPrimary
tierToLens Secondary = aSecondary

-- | 'Nothing' if the assignment is not feasible. Otherwise, the cost difference.
--
-- Constraints:
--
--  * primary facility != secondary facility
--  * min. distance between facility's locations
--  * city - primary facility - at most dCity
--  * city - secondary facility - at most 3*dCity
--  * Capacity of a center >= sum(population primary) + 0.1*sum(population secondary)
--
-- Objective Function:
--
--  * If the location is infeasible, return infinity
--  * If the location has a logistic center, within a feasible distance and the capacity of the location - the population is > 0, return 0 cost
--  * If the location doesn't has a logistic center or the current does not fullfil the constrants, then add/upgrade the one with the smallest cost that satisfies the distance and population constraint
computeCost ::
  Tier ->
  MinDistLoc ->
  PA ->
  City ->
  [FacilityType] ->
  Location ->
  Maybe (Cost, Location, PA)
computeCost tier minDistLoc' (PA w) c facilityTypes l =
  case Map.lookup l w of
    -- The location doesn't have a facility
    Nothing -> do
      if checkMinDist
        then do
          let currentOccupacy = getOccupacy Nothing
          ft <- findFacilityType currentOccupacy
          let locationInfo = (LocationInfo ft [] []) & tierToLens tier .~ [c]
              updated = coerce $ Map.insert l locationInfo w
              costDiff = ft ^. cost
          Just (costDiff, l, updated)
        else -- Infeasible due to the min distance constraint.
          Nothing

    -- The location has a facility
    Just info@(LocationInfo (FacilityType maxDist maxCap oldCost) _ _) -> do
      let currentOccupacy = getOccupacy (Just info)
          hasRange' = hasRange (coerce maxDist)
          hasCapacity' = currentOccupacy <= (fromIntegral maxCap)
      case hasRange' && hasCapacity' of
        -- Feasible assignment
        True -> do
          let updated = coerce $ Map.adjust (tierToLens tier <>~ [c]) l w
              costDiff = 0 :: Cost
          Just (costDiff, l, updated)
        -- Requires update of facility
        False -> do
          ft <- findFacilityType currentOccupacy
          let updated = coerce $ Map.adjust (set aType ft) l w
              costDiff = (ft ^. cost) - oldCost
          Just (costDiff, l, updated)
  where
    -- Minimum distance between locations.
    minDistLoc :: Distance
    minDistLoc = coerce minDistLoc'

    -- Distance between the city c and the location l
    dist :: Distance
    dist = euclideanDistance (c ^. cLocation) l

    checkMinDist :: Bool
    checkMinDist =
      all (\l2 -> l2 == l || euclideanDistance l l2 >= minDistLoc) $ Map.keys w

    hasRange :: Distance -> Bool
    hasRange maxDist = case tier of
      Primary -> dist <= maxDist
      Secondary -> dist <= 3 * maxDist

    -- Increment of occupacy when city c is assigned to location l
    cityOccupacyIncr :: Occupacy
    cityOccupacyIncr =
      let cityPop = fromIntegral (c ^. cPopulation . population)
       in case tier of
            Primary -> cityPop
            Secondary -> 0.1 * cityPop

    -- Includes the current candidate city and an optional assignment
    getOccupacy :: Maybe LocationInfo -> Occupacy
    getOccupacy Nothing = cityOccupacyIncr
    getOccupacy (Just info) =
      let getPop tier = fromIntegral . sum $ info ^.. (tierToLens tier) . folded . cPopulation . population
          primaryPop = getPop Primary
          secondaryPop = getPop Secondary
          occupacyWOAssig = primaryPop + 0.1 * secondaryPop
       in occupacyWOAssig + cityOccupacyIncr

    -- Returns the facility with minimum cost or Infeasible
    findFacilityType :: Occupacy -> Maybe FacilityType
    findFacilityType currentOccupacy =
      (flip find) facilityTypes $ \ft ->
        let maxCapacity = ft ^. cap . to fromIntegral
            hasRange' = hasRange (coerce $ ft ^. dCity)
            hasCapacity' = currentOccupacy <= maxCapacity
         in hasRange' && hasCapacity'

-- | Given a city c computes the best assignment wrt the current partial solution
assignBest ::
  (MonadIO m, MonadState PA m) =>
  Tier ->
  MinDistLoc ->
  City ->
  [FacilityType] ->
  [Location] ->
  m Location
assignBest t minDistLoc c ft locations = do
  w <- get
  case catMaybes (computeCost t minDistLoc w c ft <$> locations) of
    [] -> liftIO $ throwIO Infeasible
    candidates -> pickBestAndUpdate candidates
  where
    pickBestAndUpdate candidates = do
      liftIO $ print (candidates^..folded._1)
      let (_, location, newPA) = minimumBy (compare `on` view _1) candidates
      liftIO $ print (c, location, t)
      put newPA
      return location

-- | Greedy Algorithm
greedy :: Problem -> IO (Maybe Solution)
greedy problem = run computation
  where
    -- Facilities sorted by cost incr
    opts :: [FacilityType]
    opts = problem ^. facilityTypes . to (sortBy (compare `on` view cost))

    minDistLoc :: MinDistLoc
    minDistLoc = problem ^. dCenter

    locations :: [Location]
    locations = problem ^. facilitiesLocation

    -- Sort cities by decreasing population
    sortedCities :: [City]
    sortedCities =
      let sortDecr = sortBy (compare `on` view (cPopulation . to Down))
       in problem ^. cities . to sortDecr

    computation :: (MonadIO m, MonadState PA m) => m ()
    computation = forM_ sortedCities $ \c -> do
      liftIO $ print "======================="
      primary <- assignBest Primary minDistLoc c opts locations
      void $ assignBest Secondary minDistLoc c opts (filter (/= primary) locations)

    run :: StateT PA IO () -> IO (Maybe Solution)
    run problem = do
      r <- try @Infeasible (execStateT problem emptyPA)
      case r of
        Left _ -> return Nothing
        Right pa -> return $ Just (toSolution pa)
