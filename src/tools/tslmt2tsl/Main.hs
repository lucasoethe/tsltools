----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Wonhyuk Choi
--
-- Underapproximates a Temporal Stream Logic Modulo Theories specification
-- into a Temporal Stream Logic specification so that it can be synthesized.
-- Procedure is based on the paper
-- "Can Reactive Synthesis and Syntax-Guided Synthesis Be Friends?"
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import Config (Configuration(..), Flag(..), parseArguments)

import EncodingUtils (initEncoding)

import FileUtils (writeContent, loadTSL)

import TSL ( Specification(..)
           , SymbolTable(..)
           , fromSpec
           , getPredicateTerms
           )

import System.Exit(die)

-----------------------------------------------------------------------------

filterNotDQuote :: String -> String
filterNotDQuote = filter (/= '\"')

writeOutput :: Maybe FilePath -> Either String String -> IO ()
writeOutput _ (Left errMsg)      = die errMsg
writeOutput path (Right content) = writeContent path $ filterNotDQuote content

main :: IO ()
main = do
  initEncoding
  Configuration{input, output, flag} <- parseArguments

  spec <- loadTSL input
  
  let unhash  = stName $ symboltable spec
      content = case flag of
                  (Just Predicates) -> Right $ unlines $ map (show . (fmap unhash)) $ getPredicateTerms spec
                  (Just Grammar)    -> Right $ show $ fromSpec spec
                  Nothing           -> Left $ "tslmt2tsl end-to-end not yet supported"
                  (Just flag')      -> Left $ "Unimplemented flag: " ++ show flag'

  writeOutput output content