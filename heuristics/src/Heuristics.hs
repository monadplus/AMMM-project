-----------------------------------------------------------------------------
-- |
-- Module      :  Heuristics
-- Copyright   :  (C) 2020 Arnau Abella
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Arnau Abella <arnauabella@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Heuristics
  ( runAlgorithm
  , runAlgorithm'
  , Algorithm(..)
  , LocalSearchStrategy(..)
  )
where

import qualified Data.ByteString.Lazy as L
import Internal.Heuristics
import Internal.OPL
import Internal.Pretty
import Internal.Types

runAlgorithm :: FilePath -> Algorithm -> IO ()
runAlgorithm fp algo = do
  problem <- decodeUtf8OPL <$> L.readFile fp
  r <- runAlgorithm' problem algo
  case r of
    Nothing -> print "Infeasible"
    Just solution ->
      printSolution solution
