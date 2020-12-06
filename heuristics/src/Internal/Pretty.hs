{-# LANGUAGE TypeApplications #-}

module Internal.Pretty where

-------------------------------------------

import Control.Monad.IO.Class
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import Internal.Types
import Lens.Micro.Platform
import Text.Printf

-------------------------------------------

printSolution :: (MonadIO m) => Solution -> m ()
printSolution = liftIO . putStr . prettySolution

prettySolution :: Solution -> String
prettySolution solution =
    Map.foldlWithKey' go mempty (coerce solution)
    <> prettyObjectiveValue solution
  where
    prettyObjectiveValue :: Solution -> String
    prettyObjectiveValue = printf "\nObjective value: %d\n" . coerce @Cost @Int . computeObjectiveValue

    go :: String -> City -> Assignment -> String
    go acc c a = acc <> pretty c a

    pretty :: City -> Assignment -> String
    pretty (City _ l _) (Assignment p s) =
      printf
        "City %s\n* Primary facility %s\n* Secondary facility %s\n"
        (prettyLocation l)
        (prettyFacility p)
        (prettyFacility s)

    prettyFacility :: Facility -> String
    prettyFacility (Facility l t _) =
      printf "%s with cost %d" (prettyLocation l) (t ^. cost)

    prettyLocation :: Location -> String
    prettyLocation (Location x y) = printf "(%d, %d)" x y
