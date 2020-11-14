{-# LANGUAGE LambdaCase #-}

module Main where

-----------------------------------------------

import Generators
import Options.Applicative

-----------------------------------------------

data Config
  = SolverConfig
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

-- TODO
solverParser :: Parser Config
solverParser = pure SolverConfig

run :: Config -> IO ()
run = \case
  (GenConfig fp n) -> genSample (addSizetoFile fp n) n
  -- TODO
  SolverConfig -> fail "not implemented"

addSizetoFile :: FilePath -> Int -> FilePath
addSizetoFile fp n =
  let (baseName, extension) = span (/= '.') fp
   in baseName ++ "_" ++ (show n) ++ extension
