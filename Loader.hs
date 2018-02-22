module Loader where

import Data.Array.IArray
import Data.Char
import Data.Int
import Data.List
import Data.Maybe

import Machine
import X86

--------------------------------------------------------------------------------
-- Executable images                                                          --
--------------------------------------------------------------------------------

{-

An executable image is a half-way point between the source assembly language
program and the machine image.  You can think of it as a dramatic simplification
of the ELF or PE formats used in practice.

Our executable images contain:

  - The list of `SByte`s making up the executable code, called `textSegment`,
    along with the starting address for the text segment `textAddr`;

  - The list of `SByte`s making up the data, called `dataSegment`, along with
    the starting byte for the data segment `dataAddr`;

  - The starting address for execution (i.e., the initial RIP), called `start`.
    Note that this means that execution need not start at the lowest address in
    the code segment.
-}

data Executable = E { start, textAddr, dataAddr :: Int64
                    , textSegment, dataSegment :: [SByte] }

--------------------------------------------------------------------------------
-- Assembler                                                                  --
--------------------------------------------------------------------------------

{-

Your first task is to write an assembler (and linker); this is responsible for
taking a `Prog` value and producing an `Executable` value.  Conceptually, this
has three steps.

 - You need to transform the blocks of assembly code in the source program into
   a continuous stream of `SByte`s.  The interesting step here is that you need
   to turn textual labels in the source program into concrete integers in the
   executable image.

 - You need to transform the data blocks in the source program into a stream of
   `SByte`s.  Note that, following the slides, we want all labels in a generated
   program to be 4-byte aligned.  For the instruction stream, this will be easy,
   as we are assuming that all instructions are represented by 4-`SByte`
   sequences.  For data values, you will have to ensure that each label is on a
   4-byte boundary, inserting padding (0 bytes will do) if needed.

 - Finally, you will need to compute the start address.  Assume that programs
   will always start execution with the "main" label.

A couple of tips.

 - You may find Haskell's list-folding functions---`foldl` and `foldr`---
   particularly useful in implementing your assembler.

 - You do not need to handle errors gracefully---if the program refers to a
   label that does not exist, for example, your assembler can just crash.

 - While it is not mandatory, the sample solutions will always place the code
   segment below the data segment in memory.  If you do the same, you may find
   it easier to compare your results against the provided machine images.

   data Machine =
    M { mem :: Array Int64 SByte
      , regs :: Array Register Int64
      , flags :: (Bool, Bool, Bool)
      , rip :: Int64 }
  deriving (Read, Show)

  setRIP :: Int64 -> Machine -> Machine
  setRIP val m = m{ rip = val }

  data Data = String [Char] | Word Immediate
  deriving (Eq, Show)

  data Asm = Text [SourceInstr] | Data [Data]
  deriving (Eq, Show)

  type Prog = [(String, Bool, Asm)]

  data Executable = E { start, textAddr, dataAddr :: Int64
                    , textSegment, dataSegment :: [SByte] }

  type Instruction imm = (Operation, [Operand imm])



  type SourceInstr = Instruction Immediate

  type MachineInstr = Instruction Int64

  data SByte = Inst MachineInstr | More | Byte Word8
  deriving (Read, Show)

  data Immediate =
      Literal Int64
    | Label String
    deriving (Eq, Show)

-}


assemble :: Prog -> Executable
assemble prog =  E { start = memoryFloor--setRIP program
                  , textAddr = memoryFloor
                  , dataAddr = textEnd
                  , textSegment = instToSbyte (sourceInst2MachineInst (textLabels ++ dataLabels) textBlocks)
                  , dataSegment = dataToSbyte dataBlocks
                  }
                 where textBlocks = (filterSI prog)
                       dataBlocks = (filterData prog)
                       (textLabels, textEnd) = mapLabeltoAddr memoryFloor textBlocks
                       (dataLabels, dataEnd) = mapDataLabeltoAddr textEnd dataBlocks


filterSI :: [(String, Bool, Asm)] -> [(String,[SourceInstr])]
filterSI [] = []
filterSI ((s, _, (Text t)):insts) = [(s,t)] ++ filterSI insts

filterData::  [(String, Bool, Asm)] -> [(String, [Data])]
filterData [] = []
filterData ((s, _, (Data d)):insts) = [(s,d)] ++ filterData insts

filterData::  [(String, Bool, Asm)] -> [[Data]]
filterData [] = []
filterData ((_, _, (Data d)):insts) = [d] ++ filterData insts

sourceInst2MachineInst :: [(String,Int64)] -> [(String,[SourceInstr])] -> [MachineInstr]
sourceInst2MachineInst table insts = map patchInst $ concat (map snd insts)
                    where patchInst (op, xs)         = (op, map patch xs)
                            where patch (Imm (Literal l))    = Imm l
                                  patch (Imm (Label s))      = Imm (fromJust (lookup s table))
                                  patch (Reg r)              = Reg r
                                  patch (IndImm (Literal l)) = IndImm l
                                  patch (IndImm (Label s))   = IndImm (fromJust (lookup s table))
                                  patch (IndReg r)           = IndReg r
                                  patch (IndBoth l r)        = IndBoth l r
                                  --  where table                = mapLabeltoAddr 0x400000 insts

mapLabeltoAddr :: Int64 -> [(String, [SourceInstr])] -> ([(String,Int64)], Int64)
mapLabeltoAddr startAdd textBlocks =  foldl (\(labels,endAddr) (name,sis)->((name,endAddr):labels,endAddr+(4*fromIntegral (length sis)))) ([], startAdd) textBlocks


mapDataLabeltoAddr :: Int64 -> [(String, [Data])] -> ([(String,Int64)], Int64)
mapDataLabeltoAddr startAdd dataBlocks =  foldl (\(labels,endAddr) (name,ds)->((name,endAddr):labels,endAddr+(sum $ map dataLength ds))) ([], startAdd) dataBlocks


instToSbyte :: [MachineInstr] -> [SByte]
instToSbyte [] = []
instToSbyte (inst:insts) = [Inst (inst)] ++ [More] ++ [More] ++ [More] ++  instToSbyte insts

dataToSbyte :: [Data] -> [SByte]
dataToSbyte [] = []
dataToSbyte ((String s):ss) = map (\s-> Byte (fromIntegral (ord s))) s ++ take (fromIntegral (dataLength (String s)) - length s) (repeat (Byte 0)) ++ dataToSbyte ss
dataToSbyte ((Word(Literal b)):bs) = (map Byte (fromQuad b)) ++ dataToSbyte bs
dataToSbyte (Word(Label l):ls) =  map (\l-> Byte (fromIntegral (ord l))) l ++ dataToSbyte ls


dataLength :: Data -> Int64
dataLength (String s) = fromIntegral ((length s + 1) + if m == 0 then 0 else 4 - m)
                          where m = (length s + 1) `mod` 4
dataLength (Word _) = 8


--------------------------------------------------------------------------------
-- Loader                                                              --
--------------------------------------------------------------------------------

{-

Second, you will write a loader, which transforms and `Executable` program into
a machine image.  That is, you will have to produce a machine image with the
`textSegment` loaded into memory starting from `textAddr`, the `dataSegment`
loaded into memory starting from `dataAddr`, and with the registers set to begin
execution.

A few tips:

 - The Machine module defines a value `zeroMachine`, which is a machine image
   with everything set to 0.  You may find it helpful to build your machine
   image starting from `zeroMachine`.

 - The Haskell array update operator (//) can be used to do a number of updates
   at once.  So, for example, to update bytes i,..,i+3 to b1,..b4, you could do

       mem // [(i, b1), (i + 1, b2), (i + 2, b3), (i + 3, b4)]

 - Be sure to initialize both the instruction pointer and the stack pointer.

-}

load :: Executable -> Machine
load = error "unimplemented"
