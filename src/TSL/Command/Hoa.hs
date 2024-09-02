module TSL.Command.Hoa (command) where

import Data.Maybe (fromJust)
import Options.Applicative (Parser, ParserInfo, action, fullDesc, header, help, helper, info, long, metavar, optional, progDesc, short, showDefault, strOption, value)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import TSL.Error (warn)
import qualified TSL.LTL as LTL
import qualified TSL.ModuloTheories as ModuloTheories
import qualified TSL.Preprocessor as Preprocessor
import qualified TSL.TLSF as TLSF
import TSL.Utils (readInput, writeOutput)

data Options = Options
  { inputPath :: Maybe FilePath,
    outputPath :: Maybe FilePath,
    solverPath :: FilePath,
    ltlsyntPath :: FilePath
  }

optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info (helper <*> optionsParser) $
    fullDesc
      <> progDesc "Spec (TSL) -> synthesized controller (HOA)"
      <> header "tsl hoa"

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional
      ( strOption $
          long "input"
            <> short 'i'
            <> metavar "FILE"
            <> help "Input file (STDIN, if not set)"
            <> action "file"
      )
    <*> optional
      ( strOption $
          long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Output file (STDOUT, if not set)"
            <> action "file"
      )
    <*> strOption
      ( long "solver"
          <> value "cvc5"
          <> showDefault
          <> metavar "SOLVER"
          <> help "Path to SMT and SyGus solver"
          <> action "file"
      )
    <*> strOption
      ( long "ltlsynt"
          <> value "ltlsynt"
          <> showDefault
          <> metavar "LTLSYNT"
          <> help "Path to ltlsynt"
      )

hoa :: Options -> IO ()
hoa (Options {inputPath, outputPath, solverPath, ltlsyntPath}) = do
  -- Read input
  input <- readInput inputPath

  -- user-provided TSLMT spec (String) -> desugared TSLMT spec (String)
  preprocessedSpec <- Preprocessor.preprocess input

  -- desugared TSLMT spec (String) -> theory-encoded TSL spec (String)
  theorizedSpec <- ModuloTheories.theorize solverPath preprocessedSpec

  -- theory-encoded TSL spec (String) -> TLSF (String)
  tlsfSpec <- TLSF.lower' theorizedSpec

  -- TLSF (String) -> HOA controller (String)
  hoaController <- LTL.synthesize ltlsyntPath tlsfSpec

  hoaController <-
    case hoaController of
      Nothing -> TLSF.counter' theorizedSpec >>= LTL.synthesize ltlsyntPath >>= return . Left . fromJust
      Just c -> return $ Right c

  case hoaController of
    Left c -> do
      warn "Warning: Unrealizable Spec, generating counterstrategy"
      writeOutput outputPath c
      exitWith $ ExitFailure 1
    Right c -> do
      writeOutput outputPath c
      exitSuccess

-- Write to output

command :: ParserInfo (IO ())
command = hoa <$> optionsParserInfo
