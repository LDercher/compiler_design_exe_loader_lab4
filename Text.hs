{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Text where

import Data.Int
import Data.List (intercalate)
import System.Info (os)
import X86

class Printable t
    where textOf :: t -> String

instance Printable Int64
    where textOf = show

instance Printable Immediate
    where textOf (Literal i) = show i
          textOf (Label s)   = s

instance Printable Register
    where textOf RSI = "%RSI"
          textOf RDI = "%RDI"
          textOf RBP = "%RBP"
          textOf RSP = "%RSP"
          textOf RAX = "%RAX"
          textOf RBX = "%RBX"
          textOf RCX = "%RCX"
          textOf RDX = "%RDX"
          textOf R08 = "%R08"
          textOf R09 = "%R09"
          textOf R10 = "%R10"
          textOf R11 = "%R11"
          textOf R12 = "%R12"
          textOf R13 = "%R13"
          textOf R14 = "%R14"
          textOf R15 = "%R15"

byteRegister r
    | r >= R08 = textOf r ++ "B"
    | r >= RAX = ['%', a, 'l']
    | otherwise = ['%', a, b, 'l']
    where ['%', 'R', a, b] = textOf r

instance Printable i => Printable (Operand i)
    where textOf (Imm i) = "$" ++ textOf i
          textOf (Reg r) = textOf r
          textOf (IndImm i) = textOf i
          textOf (IndReg r) = "(" ++ textOf r ++ ")"
          textOf (IndBoth i r) = show i ++ "(" ++ textOf r ++ ")"

instance Printable Condition
    where textOf Eq = "e"
          textOf Neq = "ne"
          textOf Gt = "g"
          textOf Ge = "ge"
          textOf Lt = "l"
          textOf Le = "le"

instance Printable Operation
    where textOf Movq = "movq"
          textOf Pushq = "pushq"
          textOf Popq = "popq"
          textOf Leaq = "leaq"
          textOf Incq = "incq"
          textOf Decq = "decq"
          textOf Negq = "negq"
          textOf Notq = "notq"
          textOf Addq = "addq"
          textOf Subq = "subq"
          textOf Imulq = "imulq"
          textOf Xorq = "xorq"
          textOf Orq = "orq"
          textOf Andq = "anq"
          textOf Shlq = "shlq"
          textOf Shrq = "shrq"
          textOf Sarq = "sarq"
          textOf Jmp = "jmp"
          textOf (J cond) = "j" ++ textOf cond
          textOf Cmpq = "cmpq"
          textOf (Set cond) = "set" ++ textOf cond
          textOf Callq = "callq"
          textOf Retq = "retq"

instance Printable (Instruction Immediate)
    where textOf (opcode, operands) =
              case opcode of
                J _ -> instr jumpOperand
                Jmp -> instr jumpOperand
                Callq -> instr jumpOperand
                Set _ -> instr setOperand
                _ -> instr textOf
              where instr f = textOf opcode ++ "\t" ++ intercalate "," (map f operands)
                    jumpOperand (Imm (Literal i)) = show i
                    jumpOperand (Imm (Label s))
                        | os == "darwin"          = '_' : s
                        | otherwise               = s
                    jumpOperand arg               = '*' : textOf arg
                    setOperand (Reg r)            = byteRegister r
                    setOperand arg                = textOf arg

instance Printable Asm
    where textOf (Text instrs) = unlines (map textOf instrs)
          textOf (Data _) = error "don't do this"

instance Printable Prog
    where textOf blocks = unlines (map f blocks)
                where f (l, True, asm)  = ".global " ++ prefix l ++ "\n" ++ prefix l ++ ":\n" ++ textOf asm
                      f (l, False, asm) = prefix l ++ ":\n" ++ textOf asm
                      prefix l | os == "darwin" = '_' : l
                               | otherwise = l
