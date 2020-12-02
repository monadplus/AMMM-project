{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}

module Main where

-----------------------------------------------

import Generators
import Heuristics
import Options.Applicative

-----------------------------------------------

data Algorithm
  = Greedy Bool
  | GRASP
  deriving stock Show

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
    <$> strOption
      ( long "file"
          <> short 'f'
          <> help "Output file"
          <> metavar "STRING"
      )
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
    <$> strOption
      ( long "file"
          <> short 'f'
          <> help "Output file"
          <> metavar "STRING"
      )
    <*> algorithmParser


algorithmParser :: Parser Algorithm
algorithmParser =
    f <$> flag (Greedy False) GRASP
        ( long "GRASP"
        <> short 'g'
        <> help "Pick an algorithm" )
      <*> switch
        ( long "local search"
        <> short 'l'
        <> help "Adds a Local Search for the Greedy Algorithm" )
  where
    f (Greedy _) localSearch = Greedy localSearch
    f GRASP _ = GRASP

run :: Config -> IO ()
run = \case
  (GenConfig fp n) -> genSample (addSizetoFile fp n) n
  SolverConfig fp algorithm ->
    case algorithm of
      Greedy False -> runGreedy fp
      -- TODO
      _ -> error "not implemented"

addSizetoFile :: FilePath -> Int -> FilePath
addSizetoFile fp n =
  let (baseName, extension) = span (/= '.') fp
   in baseName ++ "_" ++ (show n) ++ extension
