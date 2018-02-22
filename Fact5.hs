module Fact5 where

import Machine
import X86

fact n =
 [ global "function"
   [ movq ~$1 ~%RAX
   , movq ~$n ~%RCX ]
 , text "loop"
   [ cmpq ~$0 ~%RCX
   , j Le ~$$"exit"
   , imulq ~%RCX ~%RAX
   , decq ~%RCX
   , jmp ~$$"loop" ]
 , text "exit"
   [ retq ] ]

program =
 fact 5 ++
 [ global "main"
   [ callq ~$$"function"
   , jmp ~$haltAddr ] ]
