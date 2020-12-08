{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

----------------------------------------------------------------------------

-- |
-- Module      :  Internal.Geneartors
-- Copyright   :  (C) 2020 Arnau Abella
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Arnau Abella <arnauabella@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
module Internal.Generators where

import Control.Monad.Reader
import Data.Coerce (coerce)
import Data.Function
import Data.Generics.Product.Typed
import Data.List
import Internal.Types
import Lens.Micro.Platform
import qualified System.Random.MWC as R
import qualified System.Random.MWC.Distributions as R

type HasGen r m = (MonadReader r m, HasType R.GenIO r)

type HasGenIO r m = (HasGen r m, MonadIO m, MonadFail m)

askGen :: HasGen r m => m R.GenIO
askGen = getTyped <$> ask

-- | Generates a sample of approximately size @n@
sample :: Int -> IO Problem
sample n = runReaderT genProblem =<< R.createSystemRandom
  where
    -- Arbitrary value
    averageCost :: Cost
    averageCost = 50

    -- Arbitrary limitation to make generation easier.
    maxPopulation :: Population
    maxPopulation = 10

    uniform :: HasGenIO r m => Double -> Int -> m Int
    uniform p x = do
      let p' = floor $ (fromIntegral x) * p
      when (p < 0.0 || p > 1.0) $ fail "0 <= p <= 1"
      liftIO . R.uniformRM (x - p', x + p') =<< askGen

    genLocation :: HasGenIO r m => Grid -> m Location
    genLocation grid = do
      let biggerGrid = doubleGrid grid
          getCoordinate gridLens =
            liftIO . R.uniformRM (0, biggerGrid ^. gridLens . to (subtract 1)) =<< askGen
      Location <$> getCoordinate gridX <*> getCoordinate gridY

    genPopulation :: HasGenIO r m => m Population
    genPopulation = do
      p <- liftIO . R.uniformRM (1, maxPopulation ^. population) =<< askGen
      return (coerce p)

    {-
      95% of the data is between [-2sigma, +2sigma]
      99.5% of the data is between [-3sigma, +3sigma]

      We want the capacities distributed between [mu-(mu/2), mu+(mu/2)]
      where mu is the minimum capacity all location should have to cover all cities at max population.
    -}
    genCapacity :: HasGenIO r m => Population -> Int -> m Int
    genCapacity totalPopulation nLocations = do
      gen <- askGen
      let mu = fromIntegral totalPopulation / fromIntegral nLocations :: Double
          sigma = mu / 6.0
      cap <- liftIO $ R.normal mu sigma gen
      return . ceiling . abs $ cap

    genDistCity :: HasGenIO r m => Grid -> m Distance
    genDistCity grid = do
      let biggerGrid = doubleGrid . doubleGrid $ grid
      gen <- askGen
      let radius = (diagonal biggerGrid) / 2.0
          mu = radius / 2.0
          sigma = mu / 3.0
      distance <- liftIO $ R.normal mu sigma gen
      return . coerce . abs $ distance

    -- Between [0.0, 2.0+epsilon], concentrated on 1.0
    -- Because the minimum distance in the grid is 1.
    genDistCenter :: HasGenIO r m => m Distance
    genDistCenter = do
      gen <- askGen
      let mu = 1.0 :: Double
          sigma = mu / 3.0
      distance <- liftIO $ R.normal mu sigma gen
      return . coerce . abs $ distance

    -- 95% of the samples will have cost in the range (averageCost / 2).
    genCost :: HasGenIO r m => m Cost
    genCost = do
      gen <- askGen
      let mu = fromIntegral averageCost
          sigma = mu / 4
      cost <- liftIO $ R.normal mu sigma gen
      return . ceiling . abs $ cost

    genCity :: HasGenIO r m => Grid -> m City
    genCity grid = do
      let tmpId = 0
      City tmpId <$> genLocation grid <*> genPopulation

    genFacilityType :: HasGenIO r m => Grid -> Population -> Int -> m FacilityType
    genFacilityType grid totalPopulation nLocations =
      FacilityType
        <$> genDistCity grid
        <*> genCapacity totalPopulation nLocations
        <*> genCost

    nubByLocation :: forall s. (Lens' s Location) -> [s] -> [s]
    nubByLocation l = nubBy ((==) `on` (view l))

    genProblem :: HasGenIO r m => m Problem
    genProblem = do
      let density = 0.9
          grid = rectGrid density n

      nCities <- uniform 0.1 n
      nLocations <- uniform 0.1 (floor (fromIntegral n / 3 :: Double))
      nTypes <- uniform 0.5 5

      _cities <- zipWith (\id -> cId .~ id) [0 ..] <$> replicateM nCities (genCity grid)
      _facilitiesLocation <- nubByLocation id <$> replicateM nLocations (genLocation grid)
      let totalPopulation = sum $ _cities ^.. folded . cPopulation
      _facilityTypes <- replicateM nTypes (genFacilityType grid totalPopulation nLocations)
      _dCenter <- genDistCenter
      return $ Problem {..}
