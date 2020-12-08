{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2020 Arnau Abella
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Arnau Abella <arnauabella@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Main where

-----------------------------------------------

import Generators
import Heuristics
import Options.Applicative

-----------------------------------------------

data Config
  = SolverConfig FilePath Algorithm
  | GenConfig FilePath Int

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (configParser <**> helper) fullDesc

configParser :: Parser Config
configParser =
  hsubparser
    ( command "generator" (info genParser (progDesc "Problem instance generator"))
        <> command "solver" (info solverParser (progDesc "Heuristic solver"))
    )

genParser :: Parser Config
genParser =
  GenConfig
    <$> fileParser "Output file"
    <*> option
      auto
      ( long "size"
          <> short 'n'
          <> help "Size of the instance"
          <> metavar "INT"
      )

solverParser :: Parser Config
solverParser =
  SolverConfig
    <$> fileParser "Input file"
    <*> hsubparser
      ( command "greedy" (info greedySolverParser (progDesc "Greedy Algorithm"))
          <> command "grasp" (info graspSolverParser (progDesc "GRASP Algorithm"))
      )

greedySolverParser :: Parser Algorithm
greedySolverParser =
  f
    <$> switch
      ( long "localSearch"
          <> short 'l'
          <> help "Adds a local search phase in the greedy algorithm."
      )
    <*> flag
      FirstImprovement
      BestImprovement
      ( long "bestImprovement"
          <> short 'b'
          <> showDefault
          <> help "Use Best Improvement instead of First Improvement"
      )
  where
    f False _ = Greedy Nothing
    f True strategy = Greedy (Just strategy)

graspSolverParser :: Parser Algorithm
graspSolverParser =
  GRASP
    <$> option
      auto
      ( long "threshold"
          <> short 't'
          <> help "Solution's Quality Treshold"
          <> showDefault
          <> value 0.5
          <> metavar "[0,1]"
      )
    <*> option
      auto
      ( long "limit"
          <> short 'l'
          <> help "Time Limit"
          <> showDefault
          <> value 60
          <> metavar "SECONDS"
      )

fileParser :: String -> Parser FilePath
fileParser helpMsg =
  strOption
    ( long "file"
        <> short 'f'
        <> help helpMsg
        <> metavar "FILE"
    )

run :: Config -> IO ()
run = \case
  (GenConfig fp n) ->
    genSample (addSizetoFile fp n) n
  SolverConfig fp algorithm ->
    runAlgorithm fp algorithm

addSizetoFile :: FilePath -> Int -> FilePath
addSizetoFile fp n =
  let (baseName, extension) = span (/= '.') fp
   in baseName ++ "_" ++ (show n) ++ extension
