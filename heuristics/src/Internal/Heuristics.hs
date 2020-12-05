{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Internal.Heuristics where

------------------------------------------------

import Control.Exception
import Control.Monad
import Control.Monad.State.Lazy
import Data.Coerce
import Data.Function
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ord
import Internal.Types
import Lens.Micro.Platform
import Text.Printf

------------------------------------------------

data Infeasible = Infeasible
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Information related to location
data LocationInfo = LocationInfo
  { _aType :: FacilityType,
    _aPrimary :: [City],
    _aSecondary :: [City]
  }
  deriving stock (Show)

-- | Partial Assignment
newtype PA = PA {_pa :: Map Location LocationInfo}
  deriving stock (Show)

makeLenses ''LocationInfo
makeLenses ''PA

-- | Empty Partial Assignment
emptyPA :: PA
emptyPA = PA Map.empty

data UpdateCmd = UpdateCmd
  { _previousFacility :: Location,
    _reassignedCity :: City,
    _newFacility :: Location,
    _facilityNewType :: FacilityType,
    -- | Positive costs improvements are good.
    _costImprovement :: Cost
  }

makeLenses ''UpdateCmd

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
          let currentOccupancy = getOccupancy Nothing
          ft <- findFacilityType currentOccupancy
          let locationInfo = (LocationInfo ft [] []) & tierToLens tier .~ [c]
              updated = coerce $ Map.insert l locationInfo w
              costDiff = ft ^. cost
          Just (costDiff, l, updated)
        else -- Infeasible due to the min distance constraint.
          Nothing

    -- The location has a facility
    Just info@(LocationInfo (FacilityType maxDist maxCap oldCost) _ _) -> do
      let currentOccupancy = getOccupancy (Just info)
          hasRange' = hasRange (coerce maxDist)
          hasCapacity' = currentOccupancy <= (fromIntegral maxCap)
      case hasRange' && hasCapacity' of
        -- Feasible assignment
        True -> do
          let updated = coerce $ Map.adjust (tierToLens tier %~ (c :)) l w
              costDiff = 0 :: Cost
          Just (costDiff, l, updated)
        -- Requires update of facility
        False -> do
          ft <- findFacilityType currentOccupancy
          let updated = coerce $ Map.adjust ((tierToLens tier %~ (c :)) . (set aType ft)) l w
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

    -- Increment of occupancy when city c is assigned to location l
    cityOccupancyIncr :: Occupancy
    cityOccupancyIncr =
      let cityPop = fromIntegral (c ^. cPopulation . population)
       in case tier of
            Primary -> cityPop
            Secondary -> 0.1 * cityPop

    -- Includes the current candidate city and an optional assignment
    getOccupancy :: Maybe LocationInfo -> Occupancy
    getOccupancy Nothing = cityOccupancyIncr
    getOccupancy (Just info) = (toOccupancy info) + cityOccupancyIncr

    -- Returns the facility with minimum cost or Infeasible
    findFacilityType :: Occupancy -> Maybe FacilityType
    findFacilityType currentOccupancy =
      (flip find) facilityTypes $ \ft ->
        let maxCapacity = ft ^. cap . to fromIntegral
            hasRange' = hasRange (coerce $ ft ^. dCity)
            hasCapacity' = currentOccupancy <= maxCapacity
         in hasRange' && hasCapacity'

toOccupancy :: LocationInfo -> Occupancy
toOccupancy locInfo =
  let getPop tier = fromIntegral . sum $ locInfo ^.. (tierToLens tier) . folded . cPopulation . population
   in (getPop Primary) + 0.1 * (getPop Secondary)

maximumSafe :: (Ord a) => a -> [a] -> a
maximumSafe a [] = a
maximumSafe _ xs = maximum xs


-- | Minimum distance required by a 'FacilityType' to handle the cities associated with the given 'Facility'.
toMinDist :: Location -> LocationInfo -> Distance
toMinDist facility locInfo =
  let minDist tier =
        maximumSafe (0 :: Distance)
          $ locInfo^.. (tierToLens tier) . folded . cLocation . to (euclideanDistance facility)
   in max (minDist Primary) ((minDist Secondary) / 3)

-- TODO implement BestImprovement
-- TODO improve by secondary assignments
-- FacilityTypes are sorted!
runLocalSearch :: LocalSearchStrategy -> [FacilityType] -> PA -> PA
runLocalSearch BestImprovement _ _ = error "Not implemented"
runLocalSearch FirstImprovement opts (PA solution) =
  let occupancyInfo = Map.map toOccupancy (coerce solution)
   in coerce $ runLocalSearch' occupancyInfo solution
  where
    runLocalSearch' :: Map Location Occupancy -> Map Location LocationInfo -> Map Location LocationInfo
    runLocalSearch' facilityOccupancy pa =
      case findBetterAssignment pa of
        Just updateCmd ->
          -- Solution improved, keep improving!
          runLocalSearch' (updateOccupancy updateCmd) (updatePA updateCmd)
        -- Nothing to improve, done.
        Nothing -> pa
      where
        -- Given a list of facilities and its associated info,
        --  find one which cost can be reduced by moving one of its cities to another facility.
        findBetterAssignment :: Map Location LocationInfo -> Maybe UpdateCmd
        findBetterAssignment =
          fstMaybe
            ( \(l1, i) ->
                let candidates = i ^. aPrimary -- Primary centers are the ones with better improvement
                 in fstMaybe (findImprovement l1) candidates
            )
            . Map.toList

        -- For all facilities but the current one, check if the city can be assigned.
        -- If the city can be assigned, check if the current facility can improve its
        -- facilityType (cost). If the cost is improved, return the city and the new facility.
        findImprovement :: Location -> City -> Maybe UpdateCmd
        findImprovement l1 c = case tryToImprove of
          -- Reassigning this city doesn't improve the solution.
          Nothing -> Nothing
          -- Reassigning improves the solution but we need to find a feasible assignment.
          Just (newFacilityType, cost) -> case find (\l2 -> l1/= l2 && isValid l2 && hasResources l2) (Map.keys pa) of
            -- The city cannot be reassigned to another facility.
            Nothing -> Nothing
            -- A better solution was found.
            Just l2 -> return (UpdateCmd l1 c l2 newFacilityType cost)
          where
            -- Try to improve the original facility after the city is being removed
            tryToImprove :: Maybe (FacilityType, Cost)
            tryToImprove = do
              let oldInfo = (pa ! l1)
                  updatedInfo = oldInfo & aPrimary %~ filter (/= c)
                  minCapacity = toOccupancy updatedInfo
                  minDist = toMinDist l1 updatedInfo
              newFacilityType <-
                find
                  ( \(FacilityType dCity cap _) ->
                      (fromIntegral cap) >= minCapacity && dCity >= (coerce minDist)
                  )
                  opts
              let costDecr = (oldInfo ^. aType . cost) - (newFacilityType ^. cost)
              if costDecr > 0
                then Just (newFacilityType, costDecr)
                else Nothing

            -- Is valid when the facility it not assigned to the same city as a secondary tier.
            isValid :: Location -> Bool
            isValid l2 = notElem c $ (pa ! l2) ^. aSecondary

            -- Has the facility enough resources to handle the city ?
            hasResources :: Location -> Bool
            hasResources l2 =
              let newOccupancy = (facilityOccupancy ! l2) + (getOccupancy Primary c)
                  maxCap = (pa ! l2) ^. aType . cap
              in newOccupancy <= (fromIntegral maxCap)

        -- Remove the occupancy from the old facility
        -- and add it to the new facility.
        updateOccupancy :: UpdateCmd -> Map Location Occupancy
        updateOccupancy cmd =
          let cityOccupancy = cmd ^. reassignedCity . to (getOccupancy Primary)
           in facilityOccupancy
                & Map.adjust (subtract cityOccupancy) (cmd ^. previousFacility)
                & Map.adjust (+ cityOccupancy) (cmd ^. newFacility)

        -- Remove the city from the old facility, update the old facility type,
        -- and add the city to the new facility.
        updatePA :: UpdateCmd -> Map Location LocationInfo
        updatePA (UpdateCmd l1 c l2 ft _) =
          pa
            & Map.adjust (aPrimary %~ (c :)) l2
            & Map.adjust
              ( \info ->
                  info
                    & aType .~ ft
                    & aPrimary %~ filter (/= c)
              )
              l1

-- | Returns the first element that maps into something.
fstMaybe :: (a -> Maybe b) -> [a] -> Maybe b
fstMaybe _ [] = Nothing
fstMaybe f (x : xs) = maybe (fstMaybe f xs) pure $ f x

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
      let (_, location, newPA) = minimumBy (compare `on` view _1) candidates
      put newPA
      return location

-- | Heuristic Algorithms
-- TODO GRASP
runAlgorithm' :: Problem -> Algorithm -> IO (Maybe Solution)
runAlgorithm' problem algorithm = run computation
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
      primary <- assignBest Primary minDistLoc c opts locations
      void $ assignBest Secondary minDistLoc c opts (filter (/= primary) locations)

    run :: StateT PA IO () -> IO (Maybe Solution)
    run problem = do
      r <- try @Infeasible (execStateT problem emptyPA)
      case r of
        Left _ -> return Nothing
        Right pa -> do
          case algorithm of
            Greedy Nothing ->
              return . Just $ toSolution pa
            Greedy (Just strategy) -> do
              return . Just . toSolution . runLocalSearch strategy opts $ pa
            GRASP ->
              error "Not implemented"
