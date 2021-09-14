module Parse where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import qualified Data.Char as Char
import qualified Data.Set as Set

data Seq = Seq [Stmt]
    deriving (Show, Eq, Ord)

data Stmt = 
      NOP 
    | Block Seq -- a code block
    | Stmt Expr
    | If Expr Seq Seq
    --   Name   Params   Body
    | Fn Label [Label] Seq 
    | Op Label Label
    -- `List a := Cons val :: a, next :: List a | Null`
    -- -> DType "List" [(Var "a")] Elems;
    -- Elems -> 
    --  [ ("Cons", 
    --      [("val", (Var "a")), ("next", (Ap (Var "List") (Var "a")))])
    --  , ("Null", [])
    --  ]
    --
    --      type cons name  type vars   see below 
    | DType String          [String]    [DTypeElem]
    -- type String := List Char;
    --  -> TypeAlias "String" (Ap (Var "List") (Var "Char"))
    --
    --
    | TypeAlias Label Expr
    | While Expr Seq
    | Case Expr [CaseStmtElem]
    | Dec Expr
    | DecAssign Label Expr
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
    | PTuple [Expr]
    | PVoid
    | Ap Expr Expr
    | ApNull Expr 
    | Ifx Expr Expr Expr -- If expr
    --       params   body
    | Lambda [Label] Seq
    | CaseX Expr [CaseExprElem]
    | Op1 Label Expr
    | Op2 Label Expr Expr
    deriving (Show, Eq, Ord)

data PatExpr = 
      PatVar String
    | PatInt Integer
    | PatFloat Double
    | PatString String
    | PatChar Char
    | PatBool Bool
    | PatVoid
    | PatArray [PatExpr]
    | PatTuple [PatExpr]
    | PatAp PatExpr PatExpr
    -- ":" operator probably
    | PatOp2 String PatExpr PatExpr
    -- for example: `tp@(1,2,3)`
    | AsPattern String PatExpr
    deriving (Show, Eq, Ord)

type Program = Seq
--                vcons label       
type DTypeElem = (Label,           
--     field getter   field setter      type expr
    [((Maybe Label,  Maybe Label),    Expr)]
    )

--                   pattern expr     pattern result
type CaseStmtElem = (PatExpr,         Seq)
type CaseExprElem = (PatExpr,         Expr)

type Label      = String

-- vars like "Array" and "Cons" and "Null"
isConsVar :: String -> Bool
isConsVar label = label /= "" && Char.isUpper (label !! 0)

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
                                      , "data"
                                      , "type"
                                      , "While"
                                      , "return"
                                      , "True"
                                      , "False"
                                      , "Case"
                                      , "case"
                                      , "Void"
                                      ]
           , Token.reservedOpNames  = [ "+"
                                      , "+."
                                      , "-"
                                      , "-."
                                      , "*"
                                      , "*."
                                      , "**"
                                      , "**."
                                      , "/"
                                      , "/."
                                      , "="
                                      , "/="
                                      , "<"
                                      , ">"
                                      , ">="
                                      , "<="
                                      , "%"
                                      , ":="
                                      , "<-"
                                      , "->"
                                      , "."
                                      , "::"
                                      , "()"
                                      , "\\"
                                      , ","
                                      , "$"
                                      , "!"
                                      , ":"
                                      , ":'"
                                      , "@"
                                      , "@'"
                                      , "?'"
                                      , "?"
                                      , "$$"
                                      , "$'"
                                      , "++"
                                      , "+++"
                                      , "+'"
                                      , "-'"
                                      , "*'"
                                      , "/'"
                                      , "//"
                                      , "=="
                                      , "/=="
                                      , "='"
                                      , "/='"
                                      , "&&"
                                      , "&"
                                      , "&'"
                                      , "||"
                                      , "|"
                                      , "|'"
                                      , "^"
                                      , "^'"
                                      , "!!"
                                      , "!'"
                                      , ">'"
                                      , "<'"
                                      , ">>"
                                      , ">>="
                                      , ">>'"
                                      , ">>='"
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


-- ops that can be repurposed
customOpList = [
      "@'"
    , "@"
    , "?'"
    , "?"
    , "&'"
    , "&"
    , "|'"
    , "|"
    , "^'"
    , "^"
    , ":'"
    , ":"
    , "+++"
    , "++"
    , "+'"
    , "-'"
    , "*'"
    , "/'"
    , "//"
    , "=="
    , "/=="
    , "='"
    , "/='"
    , "!!"
    , "!'"
    , "$$"
    , "$'"
    , ">>='"
    , ">>="
    , ">>'"
    , ">>"
    , "<'"
    , ">'"
    ]

getReservedOp op = do 
    reservedOp op
    return op

customOpsSet = Set.fromList customOpList

customOp = choice (map getReservedOp customOpList)


operators = [  
               [
                Infix   (reservedOp ""  >> return (Ap)) AssocLeft,
                Postfix (reservedOp "()"  >> return (ApNull))
                ]
            ,  [
                Prefix  (reservedOp "-" >> return (Op1 "-")),
                Infix   (reservedOp "!'" >> return (Op2 "!'")) AssocLeft,
                Infix   (reservedOp "!!" >> return (Op2 "!!")) AssocLeft,
                Infix   (reservedOp "!" >> return (Op2 "!")) AssocLeft,
                Infix   
                    (do 
                        v <- infixVar
                        return (\x y -> (Ap (Ap v x) y)))
                    AssocLeft
                ]
            ,  [
                Infix   (reservedOp "**"  >> return (Op2 "**")) AssocLeft,
                Infix   (reservedOp "**."  >> return (Op2 "**.")) AssocLeft,
                Infix   (reservedOp "^'" >> return (Op2 "^'")) AssocLeft,
                Infix   (reservedOp "^" >> return (Op2 "^")) AssocLeft
                ]
            ,  [
                Infix   (reservedOp "*'" >> return (Op2 "*'")) AssocLeft,
                Infix   (reservedOp "*." >> return (Op2 "*.")) AssocLeft,
                Infix   (reservedOp "*" >> return (Op2 "*")) AssocLeft,
                Infix   (reservedOp "/'" >> return (Op2 "/'")) AssocLeft,
                Infix   (reservedOp "/" >> return (Op2 "/")) AssocLeft,
                Infix   (reservedOp "/." >> return (Op2 "/.")) AssocLeft,
                Infix   (reservedOp "%" >> return (Op2 "%")) AssocLeft
                ]
            ,  [
                Infix   (reservedOp "+'" >> return (Op2 "+'")) AssocLeft,
                Infix   (reservedOp "+." >> return (Op2 "+.")) AssocLeft,
                Infix   (reservedOp "+" >> return (Op2 "+")) AssocLeft,
                Infix   (reservedOp "-'" >> return (Op2 "-'")) AssocLeft,
                Infix   (reservedOp "-" >> return (Op2 "-")) AssocLeft,
                Infix   (reservedOp "-." >> return (Op2 "-.")) AssocLeft
                ]
            ,  [
                Infix   (reservedOp ">'" >> return (Op2 ">'")) AssocLeft,
                Infix   (reservedOp ">" >> return (Op2 ">")) AssocLeft,
                Infix   (reservedOp "<'" >> return (Op2 "<'")) AssocLeft,
                Infix   (reservedOp "<" >> return (Op2 "<")) AssocLeft,
                Infix   (reservedOp ">=" >> return (Op2 ">=")) AssocLeft,
                Infix   (reservedOp "<=" >> return (Op2 "<=")) AssocLeft,
                Infix   (reservedOp "='" >> return (Op2 "='")) AssocLeft,
                Infix   (reservedOp "=" >> return (Op2 "=")) AssocLeft,
                Infix   (reservedOp "/='"  >> return (Op2 "/='")) AssocLeft,
                Infix   (reservedOp "/="  >> return (Op2 "/=")) AssocLeft,
                Infix   (reservedOp "=="  >> return (Op2 "==")) AssocLeft,
                Infix   (reservedOp "/=="  >> return (Op2 "/==")) AssocLeft
                ]
            ,  [
                Infix   (reservedOp "&&" >> return (Op2 "&&")) AssocLeft,
                Infix   (reservedOp "&'" >> return (Op2 "&'")) AssocLeft,
                Infix   (reservedOp "&" >> return (Op2 "&")) AssocLeft,
                Infix   (reservedOp "||"  >> return (Op2 "||")) AssocLeft,
                Infix   (reservedOp "|'"  >> return (Op2 "|'")) AssocLeft,
                Infix   (reservedOp "|"  >> return (Op2 "|")) AssocLeft
                ]
            ,  [
                Infix   (reservedOp "+++" >> return (Op2 "+++")) AssocLeft,
                Infix   (reservedOp "++" >> return (Op2 "++")) AssocLeft,
                Infix   (reservedOp "//" >> return (Op2 "//")) AssocLeft,
                Infix   (reservedOp ">>'" >> return (Op2 ">>'")) AssocLeft,
                Infix   (reservedOp ">>" >> return (Op2 ">>")) AssocLeft,
                Infix   (reservedOp ">>='" >> return (Op2 ">>='")) AssocLeft,
                Infix   (reservedOp ">>=" >> return (Op2 ">>=")) AssocLeft,
                Infix   (reservedOp ":'" >> return (Op2 ":'")) AssocRight,
                Infix   (reservedOp ":" >> return (Op2 ":")) AssocRight
                ]
           ,   [
                Infix   (reservedOp "$$" >> return (Op2 "$$")) AssocLeft,
                Infix   (reservedOp "$'" >> return (Op2 "$'")) AssocRight,
                Infix   (reservedOp "$" >> return (Op2 "$")) AssocRight
                ]
            ,  [
                Prefix  (reservedOp "@'" >> return (Op1 "@'")),
                Prefix  (reservedOp "@" >> return (Op1 "@")),
                Prefix  (reservedOp "?'" >> return (Op1 "?'")),
                Prefix  (reservedOp "?" >> return (Op1 "?")),
                Infix   (reservedOp "::" >> return (Op2 "::")) AssocLeft
                ]
            ]

decOperators = 
    [ 
        [Infix   (reservedOp "::" >> return (Op2 "::")) AssocLeft]
    ,   [Infix   (reservedOp "," >> return (Op2 ",")) AssocRight]
    

    ]

decExpr = buildExpressionParser decOperators decTerm

decTerm = var


typeOperators = 
    [ 
        [Infix   (reservedOp "" >> return (Ap)) AssocLeft]
    ,   [Infix   (reservedOp "->" >> return (Op2 "->")) AssocLeft]
    ]

-- the rhs of the `::` operator
typeExpr = buildExpressionParser typeOperators typeTerm

typeTerm =
        parens typeExpr 
    <|> var


patOperators = 
    [ 
      [Infix   (reservedOp "" >> return (PatAp)) AssocLeft]
    , [Infix   (reservedOp ":'" >> return (PatOp2 ":'")) AssocRight,
       Infix   (reservedOp ":" >> return (PatOp2 ":")) AssocRight]
    , [Infix   (reservedOp "$" >> return (PatAp)) AssocRight]
    ]

patExpr = try asPattern 
      <|> buildExpressionParser patOperators patTerm

patTerm = 
        try patTupleLiteral
    <|> parens patExpr
    <|> patArrayLiteral
    <|> liftM PatString stringLiteral
    <|> liftM PatChar charLiteral
    <|> try (reserved "True" >> return (PatBool True))
    <|> try (reserved "False" >> return (PatBool False))
    <|> try (reserved "Void" >> return (PatVoid))
    <|> try (liftM PatFloat float)
    <|> liftM PatVar identifier
    <|> liftM PatInt integer

asPattern = do
    label <- identifier
    reservedOp "@"
    px <- patExpr 
    return $ AsPattern label px

patArrayLiteral = do
    reservedOp "["
    xs <- patExpr `sepBy` (reservedOp ",")
    reservedOp "]"
    return $ PatArray xs

patTupleLiteral = parens $ do
    -- we must have at least one ","
    x1 <- patExpr
    reservedOp ","
    xs <- patExpr `sepBy1` (reservedOp ",")
    return $ PatTuple (x1:xs)



lhsExpr = expr 

readProg :: String -> Seq
readProg input = case parse beginParse "" input of
    Left err  -> error $ show err
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
    <|> caseStmt
    <|> try decAssign
    <|> decStmt
    <|> returnStmt
    <|> fnStmt
    <|> opStmt
    <|> typeStmt
    <|> dataStmt
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

decAssign = do
    reserved "dec"
    s <- do 
        label <- identifier
        reservedOp "<-"
        x <- expr
        return $ DecAssign label x
    return s

returnStmt = do
    reserved "return"
    x <- option PVoid expr
    semi
    return $ Return x

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

opStmt = do
    reserved "op"
    opname <- customOp
    reservedOp ":="
    fname  <- identifier
    semi
    return $ Op opname fname

typeStmt = do
    reserved "type"
    label <- identifier
    reservedOp ":="
    tx <- typeExpr
    semi
    return $ TypeAlias label tx

dataStmt = do
    reserved "data"
    label <- identifier
    ts <- many identifier
    reservedOp ":="
    elems <- dataStmtRHSElem `sepBy1` (reservedOp "|")
    semi
    return $ DType label ts elems

dataStmtRHSElem = do
    label <- identifier
    elems <- many $ parens (do
        -- {l1, l2} or {l1} or {}
        es <- braces $ try (do
            l1 <- identifier 
            reservedOp ","
            l2 <- identifier
            return (Just l1, Just l2)
            ) <|> try (do
            l1 <- identifier
            return (Just l1, Nothing)
            ) <|> return (Nothing, Nothing)
        reservedOp "::"
        tx <- typeExpr
        return $ (es, tx)
        ) 
    return $ (label, elems)
 

whileStmt = do
    reserved "While"
    e       <- expr
    b       <- block
    return $ While e b

ifStmt = try ifStmt1 <|> ifStmt2

ifStmt1 = do
    reserved "If"
    e       <- expr
    b       <- block
    eb     <- elseStmt
    return $ If e b eb

ifStmt2 = do
    reserved "If"
    e       <- expr
    b       <- block
    return $ If e b (Seq [])

elseStmt = do
    reserved "Else"
    b <- block
    return $ b

caseStmt = do
    reserved "Case"
    x <- expr
    elems <- braces (many1 caseStmtElem)
    return $ Case x elems

caseStmtElem :: Parser CaseStmtElem
caseStmtElem = do
    px <- patExpr
    reservedOp "->"
    b <- block
    return $ (px,b)

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


caseExpr = do
    reserved "case"
    x <- expr
    elems <- braces (many1 caseExprElem)
    return $ CaseX x elems

caseExprElem :: Parser CaseExprElem
caseExprElem = do
    px <- patExpr
    reservedOp "->"
    x <- expr
    semi
    return $ (px,x)

arrayLiteral = do
    reservedOp "["
    xs <- expr `sepBy` (reservedOp ",")
    reservedOp "]"
    return $ PArray xs

tupleLiteral = parens $ do
    -- we must have at least one ","
    x1 <- expr
    reservedOp ","
    xs <- expr `sepBy1` (reservedOp ",")
    return $ PTuple (x1:xs)

superTerm = term

term = 
    try tupleLiteral
    <|> parens expr
    <|> ifExpr
    <|> lamExpr
    <|> caseExpr
    <|> liftM PString stringLiteral
    <|> arrayLiteral
    <|> liftM PChar charLiteral
    <|> try (reserved "True" >> return (PBool True))
    <|> try (reserved "False" >> return (PBool False))
    <|> try (reserved "Void" >> return (PVoid))
    <|> try (liftM PFloat float)
    <|> var
    <|> liftM PInt integer

var = liftM Var identifier
infixVar = do
    symbol "`"
    v <- var
    symbol "`"
    return v
