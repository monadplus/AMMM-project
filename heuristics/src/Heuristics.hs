module Heuristics
  ( runGreedy
  )
where

-----------------------------------------------------------------

import qualified Data.ByteString.Lazy as L
import Internal.Greedy
import Internal.OPL
import Internal.Pretty

-----------------------------------------------------------------

runGreedy :: FilePath -> IO ()
runGreedy fp = do
  problem <- decodeUtf8OPL <$> L.readFile fp
  r <- greedy problem
  case r of
    Nothing -> print "Infeasible"
    Just solution ->
      printSolution solution
