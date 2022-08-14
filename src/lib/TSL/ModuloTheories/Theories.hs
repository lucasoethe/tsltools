-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Theories
-- Description :  Supported First-Order Theories.
-- Maintainer  :  Wonhyuk Choi
-- One may wonder why there is a separate TAst data structure instead of 
-- using `Ast TheorySymbol`.
-- This is because on data value level, there is no way to 
-- enforce relationships between `Theory` and `TheorySymbol`.
-- Instead, we use TAst to hold information about the Theory
-- as well as the actual Abstract Syntax Tree.
-- This means there is a lot more boilerplate code for adding
-- a new theory, but it achieves type safety.

-------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TypeFamilies    #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Theories( Theory(..)
                                  , TAst
                                  , TheorySymbol
                                  , readTheory
                                  , smtSortDecl
                                  , applySemantics
                                  , tastTheory
                                  , tast2Tsl
                                  , tast2Smt
                                  , tastInfo
                                  , symbol2Tsl
                                  , symbol2Smt
                                  , symbolType
                                  ) where
-------------------------------------------------------------------------------

import TSL.Error (Error, errMtParse)

import TSL.Ast(Ast, AstInfo, stringifyAst, getAstInfo)

import qualified TSL.ModuloTheories.Theories.Base as Base(TheorySymbol(..))

import qualified TSL.ModuloTheories.Theories.Uf as Uf(UfSymbol)
import qualified TSL.ModuloTheories.Theories.Lia as Lia(LiaSymbol)

-------------------------------------------------------------------------------

data Theory = 
      Uf
    | Lia
    deriving(Eq)

instance Show Theory where
  show = \case
    Uf  -> "UF"
    Lia -> "LIA"

readTheory :: String -> Either Error Theory
readTheory "#UF"  = Right Uf
readTheory "#LIA" = Right Lia
readTheory other  = errMtParse other

smtSortDecl :: Theory -> String
smtSortDecl = \case
  Uf  -> "(declare-sort UF 0)"
  Lia -> ""

data TAst =
    UfAst  (Ast Uf.UfSymbol)
  | LiaAst (Ast Lia.LiaSymbol)

instance Show TAst where show = tast2Smt

tastTheory :: TAst -> Theory
tastTheory (UfAst  _) = Uf
tastTheory (LiaAst _) = Lia

tast2Tsl :: TAst -> String
tast2Tsl (UfAst  ast) = stringifyAst Base.toTsl ast
tast2Tsl (LiaAst ast) = stringifyAst Base.toTsl ast

tast2Smt :: TAst -> String
tast2Smt (UfAst  ast) = stringifyAst Base.toSmt ast
tast2Smt (LiaAst ast) = stringifyAst Base.toSmt ast

applySemantics :: Theory -> Ast String -> Either Error TAst
applySemantics Uf  ast = UfAst  <$> traverse Base.readT ast
applySemantics Lia ast = LiaAst <$> traverse Base.readT ast

data TheorySymbol = 
    UfSymbol  Uf.UfSymbol
  | LiaSymbol Lia.LiaSymbol

tastInfo :: TAst -> AstInfo TheorySymbol
tastInfo = \case
  UfAst ast  -> fmap UfSymbol  $ getAstInfo ast
  LiaAst ast -> fmap LiaSymbol $ getAstInfo ast

symbol2Tsl :: TheorySymbol -> String
symbol2Tsl (UfSymbol  symbol) = Base.toTsl symbol
symbol2Tsl (LiaSymbol symbol) = Base.toTsl symbol

symbol2Smt :: TheorySymbol -> String
symbol2Smt (UfSymbol  symbol) = Base.toSmt symbol
symbol2Smt (LiaSymbol symbol) = Base.toSmt symbol

symbolTheory :: TheorySymbol -> Theory
symbolTheory (UfSymbol  _) = Uf
symbolTheory (LiaSymbol _) = Lia

symbolType :: TheorySymbol -> String
symbolType = show . symbolTheory
