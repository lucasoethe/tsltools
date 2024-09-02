-- | Test for Modulo Theories work.
-- These tests are meant for regression.
-- There are four types of tests corresponding to each flag of tslmt2tsl,
-- each with increasing amount of complexity:
--
-- * Predicates
-- * Context-Free Grammar
-- * Consistency Checking
-- * Syntax-Guided Synthesis
module ModuloTheoriesTests
  ( tests,
  )
where

import Control.Monad.Trans.Except
import Data.Either (isRight)
import qualified Data.Map as Map
import Distribution.Simple.Utils (warn)
import Distribution.TestSuite
  ( Progress (..),
    Result (..),
    Test (..),
    TestInstance (..),
  )
import System.Directory (doesFileExist)
import TSL.Error (warn)
import TSL.ModuloTheories
  ( Cfg (..),
    buildDtoList,
    cfgFromSpec,
    consistencyDebug,
    generateSygusAssumptions,
    predsFromSpec,
  )
import qualified TSL.ModuloTheories as MT
import Test.HUnit ((@=?))
import qualified Test.HUnit as H

convert2Cabal :: String -> IO H.Test -> Test
convert2Cabal name = Test . testInstance name

testInstance :: String -> IO H.Test -> TestInstance
testInstance name test =
  TestInstance
    { run = runTest test,
      name = name,
      tags = [],
      options = [],
      setOption = \_ _ -> Right $ testInstance name test
    }

runTest :: IO H.Test -> IO Progress
runTest = (fmap snd . H.performTest onStart onError onFailure us =<<)
  where
    onStart :: H.State -> Progress -> IO Progress
    onStart _ = return

    onError :: a -> String -> H.State -> Progress -> IO Progress
    onError _ msg _ _ = return $ Finished (Error $ concatMap (++ " ") (lines msg))

    onFailure :: a -> String -> H.State -> Progress -> IO Progress
    onFailure _ msg _ _ = return $ Finished (Fail $ concatMap (++ " ") (lines msg))

    us :: Progress
    us = Finished Pass

makeTestName :: String -> String
makeTestName = ("Modulo Theories >> " ++)

cvc5Path :: FilePath
cvc5Path = "deps/bin/cvc5"

predicatesTests :: [Test]
predicatesTests = [convert2Cabal (makeTestName "Predicates") hUnitTest]
  where
    path = "test/regression/ModuloTheories/functions_and_preds.tslmt"
    expectedNumPreds = 2

    hUnitTest = do
      (mTheory, spec, _) <- readFile path >>= MT.parse
      case mTheory of
        Nothing -> return $ H.TestCase $ H.assertFailure "Does not invoke ModuloTheory (no theory tag)."
        Just theory ->
          return $
            H.TestCase $ case predsFromSpec theory spec of
              Right preds -> expectedNumPreds @=? length preds
              Left errMsg -> H.assertFailure $ show errMsg

cfgTests :: [Test]
cfgTests = [convert2Cabal (makeTestName "CFG") hUnitTest]
  where
    path = "test/regression/ModuloTheories/functions_and_preds.tslmt"
    expectedCfgSize = 1
    expectedProductionRuleSize = 1

    hUnitTest = do
      (mTheory, spec, _) <- readFile path >>= MT.parse
      case mTheory of
        Nothing -> return $ H.TestCase $ H.assertFailure "Does not invoke ModuloTheory (no theory tag)."
        Just theory ->
          return $ case cfgFromSpec theory spec of
            Right cfg ->
              let assocs = Map.assocs $ grammar cfg
                  (_, rules) = head assocs
               in H.TestList
                    [ H.TestCase $ expectedCfgSize @=? length assocs,
                      H.TestCase $ expectedProductionRuleSize @=? length rules
                    ]
            Left errMsg -> H.TestCase $ H.assertFailure $ show errMsg

isSuccess :: (Monad m) => ExceptT e m a -> m Bool
isSuccess = fmap isRight . runExceptT

countSuccess :: (Monad m) => [ExceptT e m a] -> m Int
countSuccess = fmap (length . filter id) . mapM isSuccess

consistencyTests :: [Test]
consistencyTests = [convert2Cabal (makeTestName "Consistency") hUnitTest]
  where
    path = "test/regression/ModuloTheories/euf.tslmt"
    expectedNumAssumptions = 9
    expectedNumQueries = 15

    hUnitTest = do
      (mTheory, spec, _) <- readFile path >>= MT.parse
      case mTheory of
        Nothing -> return $ H.TestCase $ H.assertFailure "Does not invoke ModuloTheory (no theory tag)."
        Just theory -> do
          let preds = case predsFromSpec theory spec of
                Left err -> error $ show err
                Right ps -> ps
              results = consistencyDebug cvc5Path preds

          actualNumAssumptions <- countSuccess results

          return $
            H.TestList $
              map
                H.TestCase
                [ expectedNumQueries @=? length results,
                  expectedNumAssumptions @=? actualNumAssumptions
                ]

sygusTests :: [Test]
sygusTests =
  let f :: Integer -> IO H.Test -> Test
      f a = convert2Cabal (makeTestName ("SyGuS " ++ show a))
   in zipWith
        f
        [0 ..]
        testCases
  where
    directory = "test/regression/ModuloTheories"
    files =
      [ "next_sygus.tslmt",
        "eventually_sygus.tslmt"
      ]
    paths = map ((directory ++ "/") ++) files
    numExpectedAssumptions =
      [ 1,
        1
      ]
    lengthsMatch =
      return $
        H.TestCase $
          length paths
            @=? length numExpectedAssumptions
    testCases =
      (lengthsMatch :) $
        zipWith (curry makeTestCase) paths numExpectedAssumptions

    makeTestCase (path, numExpected) = do
      (mTheory, spec, _) <- readFile path >>= MT.parse
      case mTheory of
        Nothing -> return $ H.TestCase $ H.assertFailure "Does not invoke ModuloTheory (no theory tag)."
        Just theory -> do
          let preds = case predsFromSpec theory spec of
                Left err -> error $ "PREDICATES ERROR: " ++ show err
                Right ps -> ps
              cfg = case cfgFromSpec theory spec of
                Left err -> error $ "CFG ERROR: " ++ show err
                Right grammar -> grammar
              dtos = buildDtoList preds
          numActual <- countSuccess $ generateSygusAssumptions cvc5Path cfg dtos
          return $ H.TestCase $ numExpected @=? numActual

allTests :: [Test]
allTests = concat [predicatesTests, cfgTests, consistencyTests, sygusTests]

tests :: IO [Test]
tests = do
  cvc5Exists <- doesFileExist cvc5Path
  if cvc5Exists
    then return allTests
    else do
      _ <- TSL.Error.warn ("Warning: CVC5 PATH " ++ cvc5Path ++ " NOT FOUND!")
      return []
