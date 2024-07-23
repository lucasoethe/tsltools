-- | Implement HOA controller in ST
module TSL.HOA.StructuredText
  ( implement,
  )
where

import Data.List (intercalate)
import qualified Data.Set as Set
import qualified Hanoi as H
import TSL.HOA.Codegen (codegen, splitInputsCellsOutputs)
import TSL.HOA.Imp
  ( ImpConfig (..),
    cellOutputNextPrefix,
    functionName,
    withConfig',
  )

implement :: Bool -> H.HOA -> String
implement isCounterStrat hoa =
  let prog = codegen hoa
      controller = withConfig' config isCounterStrat prog
      (is', cs', os') = splitInputsCellsOutputs prog
      is = Set.toList is'
      cs = Set.toList cs'
      os = Set.toList os'
   in "PROGRAM "
        ++ functionName
        ++ "\n"
        ++ controller
        ++ "\n"
        ++ intercalate ";\n" (map cellToNext (cs ++ os))
        ++ "\nEND_PROGRAM"
  where
    cellToNext c = c ++ " := " ++ cellOutputNextPrefix ++ c

indent :: Int -> String
indent n = replicate (2 * n) ' '

config :: ImpConfig
config =
  ImpConfig
    { -- binary functions
      impAdd = "+",
      impSub = "-",
      impMult = "*",
      impDiv = "/",
      -- binary comparators
      impEq = "==",
      impNeq = "!=",
      impLt = "<",
      impGt = ">",
      impLte = "<=",
      impGte = ">=",
      -- logic
      impAnd = "AND",
      impTrue = "True",
      impFalse = "False",
      impNot = "NOT ",
      -- language constructs
      impIf = "IF",
      impElif = "ELSEIF",
      impCondition = (++ " "),
      impFuncApp = \f args -> f ++ "(" ++ intercalate ", " args ++ ")",
      impAssign = \x y -> x ++ " := " ++ y,
      impIndent = indent,
      impBlockStart = "THEN",
      impBlockEnd = \x -> if x then "END_IF" else "",
      impInitialIndent = 1
    }
