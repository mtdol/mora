module Parse where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Seq = Seq [Stmt]
    deriving (Show, Eq, Ord)

data Stmt = 
      NOP 
    | Block Seq -- a code block
    | Stmt Expr
    | If Expr Seq Seq
    --   Name   Params   Body
    | Fn String [String] Seq 
    | While Expr Seq
    | Dec Expr
    | Assign Expr Expr
    | Return Expr
    deriving (Show, Eq, Ord)

data Expr = 
      Var String
    | PInt Integer
    | PFloat Double
    | PString String
    | PChar Char
    | PBool Bool
    | PArray [Expr]
    | Dot Expr Expr
    | Ap Expr Expr
    | ApNull Expr 
    | Ifx Expr Expr Expr -- If expr
    --       params   body
    | Lambda [String] Seq
    | Op1 String Expr
    | Op2 String Expr Expr
    deriving (Show, Eq, Ord)

def =
  emptyDef { Token.commentStart     = "{-"
           , Token.commentEnd       = "-}"
           , Token.commentLine      = "--"
           , Token.identStart       = letter <|> char '_'
           , Token.identLetter      = alphaNum <|> char '_'
           , Token.reservedNames    = [ "If"
                                      , "Else"
                                      , "if"
                                      , "then"
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
                                      , "()"
                                      , "\\"
                                      , ","
                                      , "@"
                                      , "!"
                                      ]
           }

lexer = Token.makeTokenParser def

identifier  = Token.identifier  lexer -- parses an identifier
reserved    = Token.reserved    lexer -- parses a reserved name
reservedOp  = Token.reservedOp  lexer -- parses an operator
parens      = Token.parens      lexer 
brackets    = Token.brackets    lexer 
braces      = Token.braces      lexer
integer     = Token.integer     lexer -- parses an integer
float       = Token.float       lexer 
stringLiteral = Token.stringLiteral lexer
charLiteral = Token.charLiteral lexer
semi        = Token.semi        lexer -- parses a semicolon
semiSep     = Token.semiSep     lexer
whiteSpace  = Token.whiteSpace  lexer -- parses whitespace
dot         = Token.dot         lexer 
symbol      = Token.symbol      lexer

-- identifier = do
--     s1 <- many1 (letter <|> char '_')
--     s2 <- many (alphaNum <|> char '_')
--     s3 <- many (char '\'')
--     return $ s1++s2++s3

operators = [  
               [Infix   (reservedOp ""  >> return (Ap)) AssocLeft,
                Postfix (reservedOp "()"  >> return (ApNull))]
            ,  [Prefix  (reservedOp "-" >> return (Op1 "-")),
                Infix   (reserved "!" >> return (Op2 "!")) AssocLeft,
                Prefix  (reserved "not" >> return (Op1 "not")),
                Prefix  (reserved "fst" >> return (Op1 "fst")),
                Prefix  (reserved "snd" >> return (Op1 "snd")),
                Prefix  (reserved "printChar" >> return (Op1 "printChar")),
                Prefix  (reserved "show" >> return (Op1 "show")),
                Infix   
                    (do 
                        v <- infixVar
                        return (\x y -> (Ap (Ap v x) y)))
                    AssocLeft
                ]
            ,  [Infix   (reservedOp "->" >> return (Op2 "->")) AssocLeft]
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

decOperators = 
    [ 
        [Infix   (reservedOp "::" >> return (Op2 "::")) AssocLeft]
    ,   [Infix   (reservedOp "," >> return (Op2 ",")) AssocRight]
    

    ]

decExpr = buildExpressionParser decOperators decTerm

decTerm = var


-- lhsOperators = 
--     [ 
--         [Infix   (reservedOp "!" >> return (Op2 "!")) AssocLeft]
    

--     ]

lhsExpr = expr --buildExpressionParser lhsOperators lhsTerm

-- lhsTerm = term

readProg :: String -> Seq
readProg input = case parse beginParse "" input of
    Left err -> error $ show err
    Right val -> val

beginParse = do 
    whiteSpace
    ss <- stmtSeq 
    eof
    return $ ss

block = braces stmtSeq

stmtSeq :: Parser Seq
stmtSeq = do
    ss <- many statement
    return $ Seq ss

statement = 
        blockStmt
    <|> ifStmt
    <|> whileStmt
    <|> decStmt
    <|> returnStmt
    <|> fnStmt
    <|> try assignStmt
    <|> baseStmt
    <|> (semi >> (return NOP))

blockStmt = do
    b <- block
    return $ Block b

decStmt = do
    reserved "dec"
    vs <- decExpr
    semi
    return $ Dec vs

returnStmt = do
    reserved "return"
    e <- expr
    semi
    return $ Return e

assignStmt = 
        try plainAssignStmt 
    <|> try plusAssignStmt
    <|> try minusAssignStmt
    <|> try divAssignStmt 
    <|> mulAssignStmt 

plainAssignStmt = do
    left    <- lhsExpr
    reservedOp "<-"
    right   <- expr
    semi
    return $ Assign left right

plusAssignStmt = do
    left    <- lhsExpr
    reservedOp "+<"
    right   <- expr
    semi
    return $ Assign left (Op2 "+" left right)

minusAssignStmt = do
    left    <- lhsExpr
    reservedOp "-<"
    right   <- expr
    semi
    return $ Assign left (Op2 "-" left right)

mulAssignStmt = do
    left    <- lhsExpr
    reservedOp "*<"
    right   <- expr
    semi
    return $ Assign left (Op2 "*" left right)

divAssignStmt = do
    left    <- lhsExpr
    reservedOp "/<"
    right   <- expr
    semi
    return $ Assign left (Op2 "/" left right)

fnStmt = do
    reserved "fn"
    n <- identifier
    params <- many identifier
    b <- block
    return $ Fn n params b

whileStmt = do
    reserved "While"
    e       <- parens expr
    b       <- block
    return $ While e b

ifStmt = try ifStmt1 <|> ifStmt2

ifStmt1 = do
    reserved "If"
    e       <- parens expr
    b       <- block
    eb     <- elseStmt
    return $ If e b eb

ifStmt2 = do
    reserved "If"
    e       <- parens expr
    b       <- block
    return $ If e b (Seq [])

elseStmt = do
    reserved "Else"
    b <- block
    return $ b

baseStmt = do
    e <- expr
    semi
    return $ Stmt e

expr = buildExpressionParser operators superTerm

ifExpr = do
    reserved "if"
    e1 <- expr
    reserved "then"
    e2 <- expr
    reserved "else"
    e3 <- expr
    return $ Ifx e1 e2 e3

lamExpr = do
    reservedOp "\\"
    as <- many identifier
    b  <- block
    return $ Lambda as b

arrayLiteral = do
    reservedOp "["
    xs <- expr `sepBy` semi
    reservedOp "]"
    return $ PArray xs

superTerm = 
        try genitive
    <|> term

genitive = do
    x1 <- term
    dot
    x2 <- var
    return $ Op2 "." x1 x2

term = parens expr
    <|> ifExpr
    <|> lamExpr
    <|> liftM PString stringLiteral
    <|> arrayLiteral
    <|> liftM PChar charLiteral
    <|> try (reserved "True" >> return (PBool True))
    <|> try (reserved "False" >> return (PBool False))
    <|> try (liftM PFloat float)
    <|> liftM Var identifier
    <|> liftM PInt integer

var = liftM Var identifier
infixVar = do
    symbol "`"
    v <- var
    symbol "`"
    return v
