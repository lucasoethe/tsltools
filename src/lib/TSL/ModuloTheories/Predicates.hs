-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Predicates
-- Description :  Predicate term operations for TSL-MT
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Predicates( TheoryPredicate(..)
                                       , predsFromSpec
                                       , enumeratePreds
                                       , predTheory
                                       , pred2Tsl
                                       , pred2Smt
                                       , predInfo
                                       ) where

-------------------------------------------------------------------------------

import Control.Exception(assert)

import TSL.Error(Error)

import TSL.Specification(Specification(..))

import TSL.SymbolTable(SymbolTable(..))

import TSL.Types(arity)

import TSL.Logic(PredicateTerm, Formula(..), foldFormula)

import TSL.Ast(AstInfo, (+++), fromPredicateTerm)

import TSL.ModuloTheories.Theories( Theory
                                  , TAst
                                  , TheorySymbol
                                  , applySemantics
                                  , tastTheory
                                  , tast2Smt
                                  , tast2Tsl
                                  , tastInfo
                                  )

-------------------------------------------------------------------------------

data TheoryPredicate =
      PLiteral TAst
    | NotPLit  TheoryPredicate 
    | OrPLit   TheoryPredicate TheoryPredicate
    | AndPLit  TheoryPredicate TheoryPredicate

instance Show TheoryPredicate where show = pred2Smt

pred2Smt :: TheoryPredicate -> String
pred2Smt = \case
  PLiteral tast  -> tast2Smt tast
  NotPLit p      -> "(not " ++ pred2Smt p ++ ")"
  OrPLit p q     -> "(or "  ++ pred2Smt p ++ " " ++ pred2Smt q ++ ")"
  AndPLit p q    -> "(and " ++ pred2Smt p ++ " " ++ pred2Smt q ++ ")"

pred2Tsl :: TheoryPredicate -> String
pred2Tsl = \case
  PLiteral tast -> tast2Tsl tast
  NotPLit p     -> "!" ++ pred2Tsl p
  OrPLit p q    -> "(" ++ pred2Tsl p ++ " || " ++ pred2Tsl q ++ ")"
  AndPLit p q   -> "(" ++ pred2Tsl p ++ " && " ++ pred2Tsl q ++ ")"

predTheory :: TheoryPredicate -> Theory
predTheory = \case
  PLiteral tast -> tastTheory tast
  NotPLit p     -> predTheory p
  OrPLit p q    -> assert (predTheory p == predTheory q) (predTheory p)
  AndPLit p q   -> assert (predTheory p == predTheory q) (predTheory p)

predInfo :: TheoryPredicate -> AstInfo TheorySymbol
predInfo = \case
  PLiteral tast -> tastInfo tast
  NotPLit p     -> predInfo p
  OrPLit p q    -> predInfo p +++ predInfo q
  AndPLit p q   -> predInfo p +++ predInfo q

-- FIXME: make this tractable
enumeratePreds :: [TheoryPredicate] -> [TheoryPredicate]
enumeratePreds preds = iterateAll $ preds ++ map (NotPLit) preds
  where
    iterateAll []     = []
    iterateAll (x:xs) = map (AndPLit x) xs ++ iterateAll xs

predsFromSpec :: Theory -> Specification -> Either Error [TheoryPredicate]
predsFromSpec theory (Specification a g s) = mapM toTheoryPred asts
  where 
    pTerms       = concat $ map fromFormula $ a ++ g
    unhash       = stName s
    toAst        = fromPredicateTerm (arity . (stType s))
    asts         = map ((fmap unhash) . toAst) pTerms
    toTheoryPred = (fmap PLiteral) . (applySemantics theory)

fromFormula :: Formula a -> [PredicateTerm a]
fromFormula = foldFormula baseCaseExtender []
  where baseCaseExtender (Check p) ps = p:ps
        baseCaseExtender _ ps         = ps
