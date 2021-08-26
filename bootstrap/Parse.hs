module Parse where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Seq = Seq [Stmt]
    deriving (Show)

data Stmt = 
      NOP 
    | Stmt Expr
    | If Expr Seq Seq
    | Fn Expr [Expr] Seq -- [Expr] is the paramlist ([Var])
    | While Expr Seq
    | Dec Expr
    | Assign Expr Expr
    | Return Expr
    deriving (Show)

data Expr = 
      Var String
    | PInt Integer
    | PFloat Double
    | PString String
    | PChar Char
    | PBool Bool
    | Dot Expr Expr
    | Ap Expr Expr
    | Ifx Expr Expr Expr -- If expr
    | Op1 String Expr
    | Op2 String Expr Expr
    deriving (Show)

def =
  emptyDef { Token.commentStart     = "{-"
           , Token.commentEnd       = "-}"
           , Token.commentLine      = "--"
           , Token.identStart       = letter
           , Token.identLetter      = alphaNum
           , Token.reservedNames    = [ "If"
                                      , "Else"
                                      , "if"
                                      , "else"
                                      , "dec"
                                      , "fn"
                                      , "While"
                                      , "return"
                                      , "and"
                                      , "or"
                                      , "xor"
                                      , "fst"
                                      , "snd"
                                      , "printChar"
                                      , "True"
                                      , "False"
                                      ]
           , Token.reservedOpNames  = [ "+"
                                      , "+."
                                      , "-"
                                      , "-."
                                      , "*"
                                      , "*."
                                      , "/"
                                      , "/."
                                      , "="
                                      , "/="
                                      , "<"
                                      , ">"
                                      , ">="
                                      , "<="
                                      , "<-"
                                      , "->"
                                      , "::"
                                      , ","
                                      , "@"
                                      ]
           }

lexer = Token.makeTokenParser def

identifier  = Token.identifier lexer -- parses an identifier
reserved    = Token.reserved   lexer -- parses a reserved name
reservedOp  = Token.reservedOp lexer -- parses an operator
parens      = Token.parens     lexer -- parses surrounding parenthesis:
                                     --   parens p
                                     -- takes care of the parenthesis and
                                     -- uses p to parse what's inside them
braces      = Token.braces     lexer
integer     = Token.integer    lexer -- parses an integer
float       = Token.float      lexer 
stringLiteral = Token.stringLiteral lexer
charLiteral = Token.charLiteral lexer
semi        = Token.semi       lexer -- parses a semicolon
semiSep     = Token.semiSep    lexer
whiteSpace  = Token.whiteSpace lexer -- parses whitespace
dot         = Token.dot        lexer 

operators = [  
               [Infix   (reservedOp ""  >> return (Ap)) AssocLeft]
            ,  [Infix   (reservedOp "->" >> return (Op2 "->")) AssocLeft]
            ,  [Prefix  (reservedOp "-" >> return (Op1 "-")),
                Prefix  (reserved "not" >> return (Op1 "not")),
                Prefix  (reserved "fst" >> return (Op1 "fst")),
                Prefix  (reserved "snd" >> return (Op1 "snd")),
                Prefix  (reserved "printChar" >> return (Op1 "printChar")),
                Prefix  (reserved "show" >> return (Op1 "show"))
                ]
            ,  [Infix   (reservedOp "*" >> return (Op2 "*")) AssocLeft,
                Infix   (reservedOp "*." >> return (Op2 "*.")) AssocLeft,
                Infix   (reservedOp "/" >> return (Op2 "/")) AssocLeft,
                Infix   (reservedOp "/." >> return (Op2 "/.")) AssocLeft]
            ,  [Infix   (reservedOp "+" >> return (Op2 "+")) AssocLeft,
                Infix   (reservedOp "+." >> return (Op2 "+.")) AssocLeft,
                Infix   (reservedOp "-" >> return (Op2 "-")) AssocLeft,
                Infix   (reservedOp "-." >> return (Op2 "-.")) AssocLeft]
            ,  [Infix   (reservedOp ">" >> return (Op2 ">")) AssocLeft,
                Infix   (reservedOp "<" >> return (Op2 "<")) AssocLeft,
                Infix   (reservedOp ">=" >> return (Op2 ">=")) AssocLeft,
                Infix   (reservedOp "<=" >> return (Op2 "<=")) AssocLeft]
            ,  [Infix   (reserved "and" >> return (Op2 "and")) AssocLeft,
                Infix   (reserved "or"  >> return (Op2 "or")) AssocLeft,
                Infix   (reserved "xor" >> return (Op2 "xor")) AssocLeft]
            ,  [Infix   (reserved "=" >> return (Op2 "=")) AssocLeft,
                Infix   (reserved "/="  >> return (Op2 "/=")) AssocLeft]
            ,  [Infix   (reservedOp "::" >> return (Op2 "::")) AssocLeft]
            ,  [Infix   (reservedOp "," >> return (Op2 ",")) AssocRight,
                Prefix  (reservedOp "@" >> return (Op1 "@"))]

            ]

readProg :: String -> Seq
readProg input = case parse beginParse "" input of
    Left err -> error $ show err
    Right val -> val

beginParse = whiteSpace >> stmtSeq

block = braces stmtSeq

stmtSeq = fmap Seq (semiSep statement)

statement = 
        ifStmt
    <|> whileStmt
    <|> decStmt
    <|> returnStmt
    <|> fnStmt
    <|> try assignStmt
    <|> baseStmt
    <|> (return NOP)

decStmt = do
    reserved "dec"
    vs <- expr
    return $ Dec vs

returnStmt = do
    reserved "return"
    e <- expr
    return $ Return e

assignStmt = 
        try plainAssignStmt 
    <|> try plusAssignStmt
    <|> try minusAssignStmt
    <|> try divAssignStmt 
    <|> mulAssignStmt 

plainAssignStmt = do
    left    <- expr
    reservedOp "<-"
    right   <- expr
    return $ Assign left right

plusAssignStmt = do
    left    <- expr
    reservedOp "+<"
    right   <- expr
    return $ Assign left (Op2 "+" left right)

minusAssignStmt = do
    left    <- expr
    reservedOp "-<"
    right   <- expr
    return $ Assign left (Op2 "-" left right)

mulAssignStmt = do
    left    <- expr
    reservedOp "*<"
    right   <- expr
    return $ Assign left (Op2 "*" left right)

divAssignStmt = do
    left    <- expr
    reservedOp "/<"
    right   <- expr
    return $ Assign left (Op2 "/" left right)

fnStmt = do
    reserved "fn"
    n <- var
    params <- many var
    reservedOp "->"
    b <- block
    return $ Fn n params b

whileStmt = do
    reserved "While"
    e       <- parens expr
    reserved "->"
    b       <- block
    return $ While e b

ifStmt = try ifStmt1 <|> ifStmt2

ifStmt1 = do
    reserved "If"
    e       <- parens expr
    reservedOp "->"
    b       <- block
    eb     <- elseStmt
    return $ If e b eb

ifStmt2 = do
    reserved "If"
    e       <- parens expr
    reservedOp "->"
    b       <- block
    return $ If e b (Seq [])

elseStmt = do
    reserved "Else"
    b <- block
    return $ b

baseStmt = do
    e <- expr
    return $ Stmt e

expr = buildExpressionParser operators term

ifExpr = do
    reserved "if"
    e1 <- parens expr
    reservedOp "->"
    e2 <- expr
    reserved "else"
    e3 <- expr
    return $ Ifx e1 e2 e3

term = parens expr
    <|> ifExpr
    <|> liftM PString stringLiteral
    <|> liftM PChar charLiteral
    <|> try (reserved "True" >> return (PBool True))
    <|> try (reserved "False" >> return (PBool False))
    <|> try (liftM PFloat float)
    <|> try (genitive)
    <|> liftM Var identifier
    <|> liftM PInt integer

var = liftM Var identifier

genitive = do
    v1 <- var
    dot
    v2 <- var
    return $ Op2 "." v1 v2
