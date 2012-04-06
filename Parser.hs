module Parser where

import Types
import Text.Parsec
import Text.Parsec.String
import Data.Either
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)


lexer :: P.TokenParser ()
lexer = P.makeTokenParser
         (emptyDef
          {
            P.commentLine = ";",
            P.reservedNames = ["A","B","C","X","Y","Z","I","J","POP","PEEK","PUSH","SP","PC","O",
                               "SET","ADD","SUB","MUL","DIV","MOD","SHL","SHR","AND","BOR","XOR",
                               "IFE","IFN","IFG","IFB","JSR"]
          })

whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
symbol     = P.symbol lexer
natural    = P.natural lexer
parens     = P.parens lexer
brackets   = P.brackets lexer
semi       = P.semi lexer
colon      = P.colon lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer

program = do
            ins <- manyTill line eof
            return ins

line = do
          optional whiteSpace
          l <- optionMaybe lbl
          whiteSpace
          i <- optionMaybe instruction
          optional whiteSpace
          case (l,i) of
                (Nothing,Nothing) -> return []
                (Just x,Nothing) -> return [L x]
                (Just x,Just y) -> return [L x,y]
                (Nothing,Just y) -> return [y]

instruction :: Parser Instruction
instruction = do
                try (do
                        oc <- opcode
                        op1 <- operand
                        symbol (",")
                        op2 <- operand
                        return $ BI oc op1 op2)
              <|>  try (do nb <- nbopcode; op <- operand; return $ NBI nb op)


lbl :: Parser Label
lbl = do
          colon
          n <- identifier
          return $ Label n

opcode :: Parser Opcode
opcode = try (do
           reserved "SET"
           return SET)
      <|>try (do
           reserved "ADD"
           return ADD)
      <|>try (do
           reserved "SUB"
           return SUB)
      <|>try (do
           reserved "MUL"
           return MUL)
      <|>try (do
           reserved "DIV"
           return DIV)
      <|>try (do
           reserved "MOD"
           return MOD)
      <|>try (do
           reserved "SHL"
           return SHL)
      <|>try (do
           reserved "SHR"
           return SHR)
      <|>try (do
           reserved "AND"
           return AND)
      <|>try (do
           reserved "BOR"
           return BOR)
      <|>try (do
           reserved "XOR"
           return XOR)
      <|>try (do
           reserved "IFE"
           return IFE)
      <|>try (do
           reserved "IFN"
           return IFN)
      <|>try (do
           reserved "IFG"
           return IFG)
      <|>do
           reserved "IFB"
           return IFB

nbopcode :: Parser NBOpcode
nbopcode = do
             reserved "JSR"
             return JSR


operand :: Parser Operand
operand = do
            reg <- register --should probably be tidied up to use "reserved" instead
            return $ Reg reg
       <|>do
            try (do reserved "PC"; return PC) <|> try (do reserved "PEEK"; return PEEK) <|> try (do reserved "POP"; return POP) <|> do reserved "PUSH"; return PUSH
       <|>do
            reserved "SP"
            return SP
       <|>do
            reserved "O"
            return SP
       <|>do
            try (brackets (do reg <- register; return $ RRef reg))
       <|>do
            try (brackets (do nat <- natural; return $ NWRef (fromIntegral nat))) --TODO: Add error checking on nat size
       <|>do
            try (brackets (do nat <- natural; oplus; reg <- register; return $ RNW reg (fromIntegral nat))) --TODO: Clean this up to check for small nums
       <|>do
            try (do nat <- natural; return $ NWLit $ fromIntegral nat) --TODO: Add error checking on nat size
       <|>do
            i <- identifier
            return $ Ident i

oplus :: Parser ()
oplus = do
          whiteSpace
          char '+'
          whiteSpace
          return ()

register :: Parser Register
register = do
              whiteSpace
              reg <- oneOf "ABCXYZIJ"
              whiteSpace
              return $ mpreg reg
      


--testing and debugging
testparser p str = runParser p () "tester" str

testprog fn = do
                s <- readFile fn
                let output = testparser program s
                return output

--Ancillary character mappings
mpreg str = case str of
                 'A' -> A
                 'B' -> B
                 'C' -> C
                 'X' -> X
                 'Y' -> Y
                 'Z' -> Z
                 'I' -> I
                 'J' -> J

