{-# LANGUAGE FlexibleInstances #-}

module Hdcpu16.Types where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Bits
import Debug.Trace
import Numeric

data Register = A | B | C | X | Y | Z | I | J deriving (Eq, Show)


data Operand =   Reg Register 
               | RRef Register 
               | RNW Register Word16
               | POP
               | PEEK
               | PUSH
               | SP
               | PC
               | OVF
               | NWRef Word16
               | NWLit Word16
               | Ident String -- This can't be assembled and must be resolved
               | LV Word8  deriving (Show, Eq)

data Opcode =   NonBasic
              | SET
              | ADD
              | SUB
              | MUL
              | DIV
              | MOD
              | SHL
              | SHR
              | AND
              | BOR
              | XOR
              | IFE
              | IFN
              | IFG
              | IFB deriving (Show, Eq)

data NBOpcode = JSR deriving (Show, Eq)

data Program = Prog [Instruction] deriving Show

data Instruction =   BI Opcode Operand Operand 
                   | NBI NBOpcode Operand 
                   | L Label deriving (Eq, Show)

data Label = Label String deriving (Eq, Show)


instance Binary Instruction where
          get = parseIns
          put x = ins2word16 x 

instance Binary Program where
          get = do
                  ins <- parseManyIns
                  return $ Prog ins
          put (Prog xs) = do
                            mapM ins2word16 xs
                            return ()
--
--End Types
--
--

parseManyIns :: Get [Instruction]
parseManyIns = do
                  n <- remaining
                  if n > 0
                    then do
                            i <- parseIns
                            rem <- parseManyIns
                            return (i:rem)
                    else do
                            return []

parseIns :: Get Instruction
parseIns = do
              fst <- getWord16be
              let opc = fst
              let opc' = 0x0F .&. fst
              if opc' == 0x0
                then do
                       --parse non-basic instruction
                       let opc' = word2noc $ (rotateR opc 4) .&. 0x3F
                       op1  <- procword $ word2op $ (rotateR opc 10) .&. 0x3F
                       return $ NBI opc' op1 
                        
                else do
                       let opc' = word2oc $ opc .&. 0xF
                       op1 <- procword $ word2op $ (rotateR opc 4) .&. 0x3F
                       op2 <- procword $ word2op $ (rotateR opc 10) .&. 0x3F
                       return $ BI opc' op1 op2
                       --parse basic instruction
  where
    --finish these partially completed ops
    procword (RNW r 0) = do 
                            res <- getWord16be
                            return $ RNW r res
    procword (NWRef 0) = do
                            res <- getWord16be
                            return $ NWRef res
    procword (NWLit 0) = do
                            res <- getWord16be
                            return $ NWLit res
    procword x = return x


--
--Encoding 
--
ins2word16 :: Instruction -> Put 
ins2word16 (BI oc op1 op2) = do
                                putWord16be $ (rotateL (op2word16 op2) 10) .|. (rotateL (op2word16 op1) 4) .|. (oc2word16 oc)
                                if Prelude.length b2 > 0 then putWord16be (head b2) else return ()
                                if Prelude.length b3 > 0 then putWord16be (head b3) else return ()
            where
              b2 = prjword op1
              b3 = prjword op2

ins2word16 (NBI noc op1) = do
                             putWord16be $ (rotateL (op2word16 op1) 10) .|. (rotateL (noc2word16 noc) 4)
                             if length b2 > 0 then putWord16be (head b2) else return ()
            where
              b2 = prjword op1
              
prjword (NWRef w) = [w]
prjword (NWLit w) = [w]
prjword (RNW _ w) = [w]
prjword _ = []

reg2val :: Register -> Word16
reg2val reg = case reg of
                   A -> 0x0
                   B -> 0x1
                   C -> 0x2
                   X -> 0x3
                   Y -> 0x4
                   Z -> 0x5
                   I -> 0x6
                   J -> 0x7

val2reg :: Word16 -> Register
val2reg val = case val of
                   0x0 -> A
                   0x1 -> B
                   0x2 -> C
                   0x3 -> X
                   0x4 -> Y
                   0x5 -> Z
                   0x6 -> I
                   0x7 -> J

oc2word16 :: Opcode -> Word16
oc2word16 oc = case oc of
                   NonBasic -> 0x0
                   SET -> 0x1
                   ADD -> 0x2
                   SUB -> 0x3
                   MUL -> 0x4
                   DIV -> 0x5
                   MOD -> 0x6
                   SHL -> 0x7
                   SHR -> 0x8
                   AND -> 0x9
                   BOR -> 0xA
                   XOR -> 0xB
                   IFE -> 0xC
                   IFN -> 0xD
                   IFG -> 0xE
                   IFB -> 0xF

word2oc :: Word16 -> Opcode
word2oc wrd = case wrd of
                   0x0 -> NonBasic
                   0x1 -> SET 
                   0x2 -> ADD
                   0x3 -> SUB
                   0x4 -> MUL
                   0x5 -> DIV
                   0x6 -> MOD
                   0x7 -> SHL
                   0x8 -> SHR
                   0x9 -> AND
                   0xA -> BOR
                   0xB -> XOR
                   0xC -> IFE
                   0xD -> IFN
                   0xE -> IFG
                   0xF -> IFB

noc2word16 :: NBOpcode -> Word16
noc2word16 noc = case noc of
                      JSR -> 0x01

word2noc :: Word16 -> NBOpcode
word2noc wrd = case wrd of
                    0x01 -> JSR

op2word16 :: Operand -> Word16
op2word16 op = case op of
                  Reg r  -> reg2val r
                  RRef r -> 0x8 + reg2val r
                  RNW r _  -> 0x10 + reg2val r
                  POP -> 0x18
                  PEEK -> 0x19
                  PUSH -> 0x1a
                  SP -> 0x1b 
                  PC -> 0x1c
                  OVF -> 0x1d
                  (NWRef _) -> 0x1e
                  (NWLit _) -> 0x1f
                  LV n -> (fromIntegral n) --Requires prior checking

word2op :: Word16 -> Operand
word2op op = case op of
                  0x00 -> Reg A
                  0x01 -> Reg B
                  0x02 -> Reg C
                  0x03 -> Reg X
                  0x04 -> Reg Y
                  0x05 -> Reg Z
                  0x06 -> Reg I
                  0x07 -> Reg J
                  0x08 -> RRef A
                  0x09 -> RRef B
                  0x0A -> RRef C
                  0x0B -> RRef X
                  0x0C -> RRef Y
                  0x0D -> RRef Z
                  0x0E -> RRef I
                  0x0F -> RRef J
                  0x10 -> RNW A 0 --Second pass fills these in
                  0x11 -> RNW B 0
                  0x12 -> RNW C 0
                  0x13 -> RNW X 0
                  0x14 -> RNW Y 0
                  0x15 -> RNW Z 0
                  0x16 -> RNW I 0
                  0x17 -> RNW J 0
                  0x18 -> POP
                  0x19 -> PEEK
                  0x1A -> PUSH
                  0x1B -> SP
                  0x1C -> PC
                  0x1D -> OVF
                  0x1E -> NWRef 0 --Second pass fills this in
                  0x1F -> NWLit 0 --Second pass fills this in
                  n -> if n <= 0x3F && n >= 0x20 
                          then (LV ((fromIntegral n))) 
                          else error "Bad opcode."
                  
---End Encoding
