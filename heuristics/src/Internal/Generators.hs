{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Internal.Generators where

----------------------------------------------

import Control.Applicative (liftA2, liftA3)
import Control.Monad.Reader
import Data.Generics.Product.Typed
import Internal.Types
import Lens.Micro.Platform
import qualified System.Random.MWC as R
import qualified System.Random.MWC.Distributions as R
import Data.List
import Data.Function

----------------------------------------------

type HasGen r m = (MonadReader r m, HasType R.GenIO r)

type HasGenIO r m = (HasGen r m, MonadIO m, MonadFail m)

askGen :: HasGen r m => m R.GenIO
askGen = getTyped <$> ask

-- | Generates a sample of size n
sample :: Int -> IO Problem
sample n = runReaderT genProblem =<< R.createSystemRandom
  where
    uniform :: HasGenIO r m => Double -> Int -> m Int
    uniform p x = do
      gen <- askGen
      when (p < 0.0 || p > 1.0) $ fail "0 <= p <= 1"
      let p' = floor $ (fromIntegral x) * p
      liftIO $ R.uniformRM (x - p', x + p') gen

    genLocation :: HasGenIO r m => Grid -> m Location
    genLocation grid = do
      gen <- askGen
      x <- liftIO $ R.uniformRM (0, grid ^. gridX) gen
      y <- liftIO $ R.uniformRM (0, grid ^. gridY) gen
      return (Location x y)

    maxPopulation = 10 :: Int

    genPopulation :: HasGenIO r m => m Population
    genPopulation = do
      gen <- askGen
      p <- liftIO $ R.uniformRM (0, maxPopulation) gen
      return $ Population p

    {-
      95% of the data is between [-2sigma, +2sigma]
      99.5% of the data is between [-3sigma, +3sigma]

      We want the capacities distributed between [0, 2*mu]
      where mu is the minimum capacity all location should have to cover all cities at max population.
    -}
    genCap :: HasGenIO r m => Int -> Int -> m Int
    genCap nCities nLocations = do
      gen <- askGen
      let mu = fromIntegral (nCities * maxPopulation) / fromIntegral nLocations :: Double
          sigma = mu / 3.0
      cap <- liftIO $ R.normal mu sigma gen
      return . ceiling . abs $ cap

    genDistCity :: HasGenIO r m => Grid -> m Double
    genDistCity grid = do
      gen <- askGen
      let radius = (diagonal grid) / 2.0
          mu = radius / 2.0
          sigma = mu / 3.0
      distance <- liftIO $ R.normal mu sigma gen
      return $ abs distance

    -- Between [0.0, 2.0+epsilon], concentrated on 1.0
    -- Because the minimum distance in the grid is 1.
    genDistCenter :: HasGenIO r m => m Double
    genDistCenter = do
      gen <- askGen
      let mu = 1.0 :: Double
          sigma = mu / 3.0
      distance <- liftIO $ R.normal mu sigma gen
      return $ abs distance

    genCost :: HasGenIO r m => m Int
    genCost = do
      gen <- askGen
      let mu = 25 :: Double -- arbitrary
          sigma = mu / 4
      cost <- liftIO $ R.normal mu sigma gen
      return . ceiling . abs $ cost

    genCity :: HasGenIO r m => Grid -> m City
    genCity grid = liftA2 City (genLocation grid) genPopulation

    genFacilityType :: HasGenIO r m => Grid -> Int -> Int -> m FacilityType
    genFacilityType grid nCities nLocations =
      liftA3 FacilityType (genDistCity grid) (genCap nCities nLocations) genCost

    nubByLocation :: forall s. (Lens' s Location) -> [s] -> [s]
    nubByLocation l = nubBy ((==) `on` (view l))

    genProblem :: HasGenIO r m => m Problem
    genProblem = do
      let density = 0.8
          grid = rectGrid density n

      nCities <- uniform 0.2 n
      nLocations <- uniform 0.2 (floor (fromIntegral n / 2 :: Double))
      nTypes <- uniform 0.2 (floor (fromIntegral n / 3 :: Double))

      _cities' <- replicateM nCities (genCity grid)
      _facilitiesLocation' <- replicateM nLocations (genLocation grid)
      _facilityTypes <- replicateM nTypes (genFacilityType grid nCities nLocations)
      _dCenter <- genDistCenter
      let _cities = nubByLocation cLocation _cities'
          _facilitiesLocation = nubByLocation id _facilitiesLocation'

      return $ Problem {..}
