{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

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
  f
    <$> flag
      (Greedy Nothing)
      GRASP
      ( long "grasp"
          <> short 'g'
          <> showDefault
          <> help "Use GRASP instead of a Greedy algorihtm."
      )
    <*> switch
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
    f (Greedy _) False _ = Greedy Nothing
    f (Greedy _) True strategy = Greedy (Just strategy)
    f GRASP _ _ = GRASP

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
