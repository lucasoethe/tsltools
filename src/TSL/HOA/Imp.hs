{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- | Template code writer for imperative languages
module TSL.HOA.Imp where

import Control.Monad (zipWithM)
import Control.Monad.Reader (Reader)
import qualified Control.Monad.Reader as Reader
import Data.Char (isSpace)
import Data.List
  ( intercalate,
    isPrefixOf,
  )
import qualified Hanoi as H
import qualified TSL.HOA.Codegen as CG

functionName :: String
functionName = "updateState"

cellOutputNextPrefix :: String
cellOutputNextPrefix = "_next_"

data ImpConfig = ImpConfig
  { -- binary functions
    impAdd :: String,
    impSub :: String,
    impMult :: String,
    impDiv :: String,
    -- binary comparators
    impEq :: String,
    impNeq :: String,
    impLt :: String,
    impGt :: String,
    impLte :: String,
    impGte :: String,
    -- logic
    impAnd :: String,
    impTrue :: String,
    impFalse :: String,
    impNot :: String,
    -- language constructs
    impIf :: String,
    impElif :: String,
    impCondition :: String -> String,
    impFuncApp :: String -> [String] -> String,
    impAssign :: String -> String -> String,
    impIndent :: Int -> String,
    impBlockStart :: String,
    impBlockEnd :: Bool -> String,
    impInitialIndent :: Int
  }

withConfig :: ImpConfig -> Bool -> H.HOA -> String
withConfig config isCounterStrat hoa =
  let prog = CG.codegen hoa in Reader.runReader (writeProgram isCounterStrat prog) config -- TODO: make the False a flag for indicating counterstrategy

withConfig' :: ImpConfig -> Bool -> CG.Program -> String
withConfig' config isCounterStrat prog =
  Reader.runReader (writeProgram isCounterStrat prog) config -- TODO: make the False a flag for indicating counterstrategy

-- | WRITE PROGRAM TO STRING
type Imp a = Reader ImpConfig a

data IfElif = If | ElIf | FinalIf | FinalElIf deriving (Enum, Eq)

isFinalHelper :: IfElif -> Bool
isFinalHelper If = False
isFinalHelper ElIf = False
isFinalHelper FinalIf = True
isFinalHelper FinalElIf = True

writeProgram :: Bool -> CG.Program -> Imp String
writeProgram isCounterStrat (CG.Program stateTransList) = do
  ImpConfig {..} <- Reader.ask
  lines <-
    concat <$> zipWithM (writeStateTrans isCounterStrat) (first : replicate (stateTransListLen - 2) ElIf ++ last) stateTransList
  return $ impIndent impInitialIndent ++ intercalate ("\n" ++ impIndent impInitialIndent) (discardEmptyLines lines)
  where
    discardEmptyLines lines =
      filter (\l -> not (null l) && not (all isSpace l)) lines
    stateTransListLen = length stateTransList
    first = if stateTransListLen == 1 then FinalIf else If
    last = if stateTransListLen == 1 then [] else [FinalElIf]

writeStateTrans :: Bool -> IfElif -> CG.StateTrans -> Imp [String]
writeStateTrans isCounterStrat useElif (CG.StateTrans state transList) = do
  ImpConfig {..} <- Reader.ask
  let opIf = if useElif == If || useElif == FinalIf then impIf else impElif

  innerLines <- concat <$> zipWithM (writeTrans isCounterStrat) (first : replicate (transListLen - 2) ElIf ++ last) transList

  return $
    [ opIf
        ++ " "
        ++ impCondition ("currentState " ++ impEq ++ " " ++ state)
        ++ impBlockStart
    ]
      ++ map (impIndent 1 ++) innerLines
      ++ [impBlockEnd $ isFinalHelper useElif]
  where
    transListLen = length transList
    first = if transListLen == 1 then FinalIf else If
    last = if transListLen == 1 then [] else [FinalElIf]

writeTrans :: Bool -> IfElif -> CG.Trans -> Imp [String]
writeTrans isCounterStrat useElif (CG.Trans ps us target) = do
  ImpConfig {..} <- Reader.ask
  let opIf = if useElif == If || useElif == FinalIf then impIf else impElif

  ps' <- mapM writePredicate ps
  us' <- mapM writeUpdate us
  let condition = intercalate (" " ++ impAnd ++ " ") ps'
      assignments = map (impIndent 1 ++) us'
      (condition', assignments') =
        if isCounterStrat then (intercalate " && " assignments, ["  " ++ condition]) else (condition, assignments)
  return $
    [opIf ++ " " ++ impCondition condition' ++ impBlockStart]
      ++ assignments'
      ++ [impIndent 1 ++ impAssign "currentState" target]
      ++ [impBlockEnd $ isFinalHelper useElif]

writePredicate :: CG.Predicate -> Imp String
writePredicate p = do
  ImpConfig {..} <- Reader.ask
  case p of
    CG.PTrue -> return impTrue
    CG.PFalse -> return impFalse
    CG.PNot (CG.PNot p') -> writePredicate p'
    CG.PNot CG.PTrue -> return impFalse
    CG.PNot CG.PFalse -> return impTrue
    CG.PNot (CG.PTerm (Eq x1 x2)) -> writeTerm (Neq x1 x2)
    CG.PNot (CG.PTerm (Neq x1 x2)) -> writeTerm (Eq x1 x2)
    CG.PNot (CG.PTerm (Lt x1 x2)) -> writeTerm (Gte x1 x2)
    CG.PNot (CG.PTerm (Gt x1 x2)) -> writeTerm (Lte x1 x2)
    CG.PNot (CG.PTerm (Lte x1 x2)) -> writeTerm (Gt x1 x2)
    CG.PNot (CG.PTerm (Gte x1 x2)) -> writeTerm (Lt x1 x2)
    CG.PNot p' -> (impNot ++) <$> writePredicate p'
    CG.PTerm term -> writeTerm term

writeUpdate :: CG.Update -> Imp String
writeUpdate (CG.Update var term) = do
  ImpConfig {..} <- Reader.ask
  impAssign (cellOutputNextPrefix ++ var) <$> writeTermNoParens term

writeTerm :: CG.Term -> Imp String
writeTerm term = do
  case term of
    CG.Var x -> return x
    CG.App f args ->
      if isTSLMTBinOp f args
        then do
          bin <- writeTermApp f args
          return $ "(" ++ bin ++ ")"
        else writeTermNoParens term

writeTermNoParens :: CG.Term -> Imp String
writeTermNoParens term = do
  case term of
    CG.Var x -> return x
    CG.App f args -> writeTermApp f args

writeTermApp :: String -> [CG.Term] -> Imp String
writeTermApp f args
  | isTSLMTLiteral f args = return $ replaceTSLMTLiteral f
  | isTSLMTBinOp f args = do
      args' <- mapM writeTerm args
      case args' of
        [x1, x2] -> replaceTSLMTBinOp f x1 x2
        _ -> error "BUG: writeTermApp isTSLMTBinOp - should only have 2 arguments for binary operations."
  | otherwise = do
      ImpConfig {..} <- Reader.ask
      impFuncApp f <$> mapM writeTermNoParens args

-- | HELPERS
pickIfOrElif :: Bool -> Imp String
pickIfOrElif useElif = do
  if useElif then Reader.asks impElif else Reader.asks impIf

isTSLMTLiteral :: String -> [CG.Term] -> Bool
isTSLMTLiteral s args = null args && (isReal s || isInt s)

replaceTSLMTLiteral :: String -> String
replaceTSLMTLiteral s
  | isReal s = replaceNeg $ drop 4 s
  | isInt s = replaceNeg $ drop 3 s
  | otherwise = s
  where
    replaceNeg x = if "Neg" `isPrefixOf` x then "-" ++ drop 3 x else x

isReal :: String -> Bool
isReal s = "real" `isPrefixOf` s

isInt :: String -> Bool
isInt s = "int" `isPrefixOf` s

isTSLMTBinOp :: String -> [CG.Term] -> Bool
isTSLMTBinOp f args = length args == 2 && isOp
  where
    isOp = case f of
      "add" -> True
      "sub" -> True
      "mult" -> True
      "div" -> True
      "eq" -> True
      "neq" -> True
      "lt" -> True
      "gt" -> True
      "lte" -> True
      "gte" -> True
      _ -> False

replaceTSLMTBinOp :: String -> String -> String -> Imp String
replaceTSLMTBinOp f x1 x2 = do
  ImpConfig {..} <- Reader.ask
  return $ case f of
    "add" -> useBinOp impAdd
    "sub" -> useBinOp impSub
    "mult" -> useBinOp impMult
    "div" -> useBinOp impDiv
    "eq" -> useBinOp impEq
    "neq" -> useBinOp impNeq
    "lt" -> useBinOp impLt
    "gt" -> useBinOp impGt
    "lte" -> useBinOp impLte
    "gte" -> useBinOp impGte
    x ->
      error ("Implementation bug: " ++ x ++ " is not a TSLMT binary operation.")
  where
    useBinOp op = x1 ++ " " ++ op ++ " " ++ x2

pattern Add :: CG.Term -> CG.Term -> CG.Term
pattern Add x1 x2 = CG.App "add" [x1, x2]

pattern Sub :: CG.Term -> CG.Term -> CG.Term
pattern Sub x1 x2 = CG.App "sub" [x1, x2]

pattern Mult :: CG.Term -> CG.Term -> CG.Term
pattern Mult x1 x2 = CG.App "mult" [x1, x2]

pattern Div :: CG.Term -> CG.Term -> CG.Term
pattern Div x1 x2 = CG.App "div" [x1, x2]

pattern Eq :: CG.Term -> CG.Term -> CG.Term
pattern Eq x1 x2 = CG.App "eq" [x1, x2]

pattern Neq :: CG.Term -> CG.Term -> CG.Term
pattern Neq x1 x2 = CG.App "neq" [x1, x2]

pattern Lt :: CG.Term -> CG.Term -> CG.Term
pattern Lt x1 x2 = CG.App "lt" [x1, x2]

pattern Gt :: CG.Term -> CG.Term -> CG.Term
pattern Gt x1 x2 = CG.App "gt" [x1, x2]

pattern Lte :: CG.Term -> CG.Term -> CG.Term
pattern Lte x1 x2 = CG.App "lte" [x1, x2]

pattern Gte :: CG.Term -> CG.Term -> CG.Term
pattern Gte x1 x2 = CG.App "gte" [x1, x2]
