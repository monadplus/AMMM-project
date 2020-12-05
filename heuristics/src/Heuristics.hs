module Heuristics
  ( runAlgorithm
  , runAlgorithm'
  , Algorithm(..)
  , LocalSearchStrategy(..)
  )
where

-----------------------------------------------------------------

import qualified Data.ByteString.Lazy as L
import Internal.Heuristics
import Internal.OPL
import Internal.Pretty
import Internal.Types

-----------------------------------------------------------------

runAlgorithm :: FilePath -> Algorithm -> IO ()
runAlgorithm fp algo = do
  problem <- decodeUtf8OPL <$> L.readFile fp
  r <- runAlgorithm' problem algo
  case r of
    Nothing -> print "Infeasible"
    Just solution ->
      printSolution solution
