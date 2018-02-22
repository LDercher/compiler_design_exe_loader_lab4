module Expr42 where

import Data.Int
import Data.List
import Data.Maybe
import Machine
import X86


data VarExpr = ConstV Int64 | AddV VarExpr VarExpr | MulV VarExpr VarExpr
             | Var String | String := VarExpr | Seq VarExpr VarExpr
  deriving (Show)

infixr 0 `Seq`
infix  1 :=
infixl 6 `AddV`
infixl 7 `MulV`

vars (ConstV _)   = []
vars (Var x)      = [x]
vars (x := e)     = nub (x : vars e)
vars (AddV e1 e2) = nub (vars e1 ++ vars e2)
vars (MulV e1 e2) = nub (vars e1 ++ vars e2)
vars (Seq e1 e2)  = nub (vars e1 ++ vars e2)

comp2 e =
 [ global "function" $
   [ pushq ~%RBP
   , movq ~%RSP ~%RBP
   , subq ~$(fromIntegral (length xs) * 8) ~%RSP ] ++
   compV xs e ++
   [ popq ~%RAX
   , movq ~%RBP ~%RSP
   , popq ~%RBP
   , retq ] ]
     where xs = vars e

-- same invariant as compE

compConst xs op i e =
  compV xs e ++
  [ op ~$i ~#RSP ]
compBin compile op e1 e2 =
 compile e1 ++
 compile e2 ++
 [ popq ~%RAX
 , popq ~%RDX
 , op ~%RDX ~%RAX
 , pushq ~%RAX ]

compV _ (ConstV i) =
 [ pushq ~$i ]
compV xs (Var x) =
 [ pushq ~#(offset xs x, RBP) ]
compV xs (x := e) =
 compV xs e ++
 [ pushq ~#RSP
 , popq ~#(offset xs x, RBP) ]
compV xs (AddV (ConstV i) e) =
 compConst xs addq i e
compV xs (AddV e1 e2) =
 compBin (compV xs) addq e1 e2
compV xs (MulV (ConstV i) e) =
 compConst xs imulq i e
compV xs (MulV e1 e2) =
 compBin (compV xs) imulq e1 e2
compV xs (Seq e1 e2) =
 compV xs e1 ++
 [ addq ~$8 ~%RSP ] ++
 compV xs e2

offset xs x = -8 * fromIntegral (fromJust (elemIndex x xs) + 1)

sampleExpr2b =
  "X" := ConstV 3 `Seq`
  "Y" := ConstV 3 `Seq`
  (Var "X" `AddV` Var "Y") `MulV`
  (Var "X" `AddV` ("Y" := Var "Y" `AddV` ConstV 1 `Seq` Var "Y"))
  -- also should be 42, less obvs.

program =
 comp2 sampleExpr2b ++
 [ global "main"
   [ callq ~$$"function"
   , jmp ~$haltAddr ] ]
