-----------------------------------------------------------------------------
-- |
-- Module      :  Generators
-- Copyright   :  (C) 2020 Arnau Abella
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Arnau Abella <arnauabella@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Generators
  ( genSample,
    module Internal.Generators,
    module Internal.OPL,
    module Internal.Types,
  )
where

import qualified Data.ByteString.Lazy as L
import Internal.Generators
import Internal.OPL
import Internal.Types

genSample :: FilePath -> Int -> IO ()
genSample fp n = do
  problem <- sample n
  let lbs = encodeUtf8OPL problem
  L.writeFile fp lbs
