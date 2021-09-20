{-# OPTIONS_GHC -cpp #-}

-- makes a `NodeInfo`
#define MKNI (getPosition >>= \sp -> return $ NodeInfo (sourceLine sp) (sourceColumn sp))
-- shorthand for application of MKNI
#define APMKNI MKNI >>= \ni ->
module Parse where


import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import qualified Data.Char as Char
import qualified Data.Set as Set

import Error

type ModuleId = String

data Seq = Seq [Stmt]
    deriving (Show, Eq, Ord)

data Stmt = 
      NOP NodeInfo
    | Block NodeInfo Seq -- a code block
    | Stmt NodeInfo Expr
    | If NodeInfo Expr Seq Seq
    --            Name  Params  Body
    | Fn NodeInfo Label [Label] Seq 
    -- `List a := Cons val :: a, next :: List a | Null`
    -- -> DType "List" [(Var "a")] Elems;
    -- Elems -> 
    --  [ ("Cons", 
    --      [("val", (Var "a")), ("next", (Ap (Var "List") (Var "a")))])
    --  , ("Null", [])
    --  ]
    --
    --               type cons name  type vars   see below 
    | DType NodeInfo String          [String]    [DTypeElem]
    -- type String := List Char;
    --  -> TypeAlias "String" (Ap (Var "List") (Var "Char"))
    --
    --
    | TypeAlias NodeInfo Label Expr
    | While NodeInfo Expr Seq
    | Case NodeInfo Expr [CaseStmtElem]
    | Dec NodeInfo Expr
    | DecAssign NodeInfo Label Expr
    | Assign NodeInfo Expr Expr
    | Return NodeInfo Expr
    deriving (Show, Eq, Ord)

data Expr = 
      Var NodeInfo String
    | PInt NodeInfo Integer
    | PFloat NodeInfo Double
    | PString NodeInfo String
    | PChar NodeInfo Char
    | PBool NodeInfo Bool
    | PArray NodeInfo [Expr]
    | PTuple NodeInfo [Expr]
    | PVoid NodeInfo
    | Ap NodeInfo Expr Expr
    | ApNull NodeInfo Expr 
    | IfX NodeInfo Expr Expr Expr -- If expr
    --                params   body
    | Lambda NodeInfo [Label] Seq
    | CaseX NodeInfo Expr [CaseExprElem]
    | Op1 Label NodeInfo Expr
    | Op2 Label NodeInfo Expr Expr
    deriving (Show, Eq, Ord)

data PatExpr = 
      PatVar NodeInfo String
    | PatInt NodeInfo Integer
    | PatFloat NodeInfo Double
    | PatString NodeInfo String
    | PatChar NodeInfo Char
    | PatBool NodeInfo Bool
    | PatVoid NodeInfo
    | PatArray NodeInfo [PatExpr]
    | PatTuple NodeInfo [PatExpr]
    | PatAp NodeInfo PatExpr PatExpr
    -- ":" operator probably
    | PatOp2 String NodeInfo PatExpr PatExpr
    -- for example: `tp@(1,2,3)`
    | AsPattern NodeInfo String PatExpr
    deriving (Show, Eq, Ord)

data ModuleData = 
    ModuleData Manifest [ModuleStmt]
    deriving (Show, Eq, Ord)

data Manifest = 
      Including [Label] 
    | Excluding [Label]
    deriving (Show, Eq, Ord)

data ModuleStmt = 
      --     mid   manifest  "as" label
      Import Label Manifest  Label
    | OpDec Label Label
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
           , Token.identLetter      = alphaNum <|> char '_' <|> char '.'
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
                                      , "module"
                                      , "op"
                                      , "import"
                                      , "excluding"
                                      , "including"
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
                                      , "?"
                                      , "?'"
                                      , "??"
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
    , "??"
    , "?'"
    , "?'"
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
                Infix   (APMKNI reservedOp ""  >> return (Ap ni)) AssocLeft,
                Postfix (APMKNI reservedOp "()"  >> return (ApNull ni))
                ]
            ,  [
                Prefix  (APMKNI reservedOp "-" >> return (Op1 "-" ni)),
                Infix   (APMKNI reservedOp "!'" >> return (Op2 "!'" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "!!" >> return (Op2 "!!" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "!" >> return (Op2 "!" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "?'" >> return (Op2 "?'" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "??" >> return (Op2 "??" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "?" >> return (Op2 "?" ni)) AssocLeft,
                Infix   
                    (do 
                        ni <- MKNI
                        v <- infixVar
                        return (\x y -> (Ap ni (Ap ni v x) y)))
                    AssocLeft
                ]
            ,  [
                Infix   (APMKNI reservedOp "**"  >> return (Op2 "**" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "**."  >> return (Op2 "**." ni)) AssocLeft,
                Infix   (APMKNI reservedOp "^'" >> return (Op2 "^'" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "^" >> return (Op2 "^" ni)) AssocLeft
                ]
            ,  [
                Infix   (APMKNI reservedOp "*'" >> return (Op2 "*'" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "*." >> return (Op2 "*." ni)) AssocLeft,
                Infix   (APMKNI reservedOp "*" >> return (Op2 "*" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "/'" >> return (Op2 "/'" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "/" >> return (Op2 "/" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "/." >> return (Op2 "/." ni)) AssocLeft,
                Infix   (APMKNI reservedOp "%" >> return (Op2 "%" ni)) AssocLeft
                ]
            ,  [
                Infix   (APMKNI reservedOp "+'" >> return (Op2 "+'" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "+." >> return (Op2 "+." ni)) AssocLeft,
                Infix   (APMKNI reservedOp "+" >> return (Op2 "+" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "-'" >> return (Op2 "-'" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "-" >> return (Op2 "-" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "-." >> return (Op2 "-." ni)) AssocLeft
                ]
            ,  [
                Infix   (APMKNI reservedOp ">'" >> return (Op2 ">'" ni)) AssocLeft,
                Infix   (APMKNI reservedOp ">" >> return (Op2 ">" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "<'" >> return (Op2 "<'" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "<" >> return (Op2 "<" ni)) AssocLeft,
                Infix   (APMKNI reservedOp ">=" >> return (Op2 ">=" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "<=" >> return (Op2 "<=" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "='" >> return (Op2 "='" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "=" >> return (Op2 "=" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "/='"  >> return (Op2 "/='" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "/="  >> return (Op2 "/=" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "=="  >> return (Op2 "==" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "/=="  >> return (Op2 "/==" ni)) AssocLeft
                ]
            ,  [
                Infix   (APMKNI reservedOp "&&" >> return (Op2 "&&" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "&'" >> return (Op2 "&'" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "&" >> return (Op2 "&" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "||"  >> return (Op2 "||" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "|'"  >> return (Op2 "|'" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "|"  >> return (Op2 "|" ni)) AssocLeft
                ]
            ,  [
                Infix   (APMKNI reservedOp "+++" >> return (Op2 "+++" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "++" >> return (Op2 "++" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "//" >> return (Op2 "//" ni)) AssocLeft,
                Infix   (APMKNI reservedOp ">>'" >> return (Op2 ">>'" ni)) AssocLeft,
                Infix   (APMKNI reservedOp ">>" >> return (Op2 ">>" ni)) AssocLeft,
                Infix   (APMKNI reservedOp ">>='" >> return (Op2 ">>='" ni)) AssocLeft,
                Infix   (APMKNI reservedOp ">>=" >> return (Op2 ">>=" ni)) AssocLeft,
                Infix   (APMKNI reservedOp ":'" >> return (Op2 ":'" ni)) AssocRight,
                Infix   (APMKNI reservedOp ":" >> return (Op2 ":" ni)) AssocRight
                ]
           ,   [
                Infix   (APMKNI reservedOp "$$" >> return (Op2 "$$" ni)) AssocLeft,
                Infix   (APMKNI reservedOp "$'" >> return (Op2 "$'" ni)) AssocRight,
                Infix   (APMKNI reservedOp "$" >> return (Op2 "$" ni)) AssocRight
                ]
            ,  [
                Prefix  (APMKNI reservedOp "@'" >> return (Op1 "@'" ni)),
                Prefix  (APMKNI reservedOp "@" >> return (Op1 "@" ni)),
                Infix   (APMKNI reservedOp "::" >> return (Op2 "::" ni)) AssocLeft
                ]
            ]

decOperators = 
    [ 
        [Infix   (APMKNI reservedOp "::" >> return (Op2 "::" ni)) AssocLeft]
    ,   [Infix   (APMKNI reservedOp "," >> return (Op2 "," ni)) AssocRight]
    

    ]

decExpr = buildExpressionParser decOperators decTerm

decTerm = var


typeOperators = 
    [ 
        [Infix   (APMKNI reservedOp "" >> return (Ap ni)) AssocLeft]
    ,   [Infix   (APMKNI reservedOp "->" >> return (Op2 "->" ni)) AssocLeft]
    ]

-- the rhs of the `::` operator
typeExpr = buildExpressionParser typeOperators typeTerm

typeTerm =
        parens typeExpr 
    <|> var


patOperators = 
    [ 
      [Infix   (APMKNI reservedOp "" >> return (PatAp ni)) AssocLeft]
    , [Infix   (APMKNI reservedOp ":'" >> return (PatOp2 ":'" ni)) AssocRight,
       Infix   (APMKNI reservedOp ":" >> return (PatOp2 ":" ni)) AssocRight]
    , [Infix   (APMKNI reservedOp "$" >> return (PatAp ni)) AssocRight]
    ]

patExpr = try asPattern 
      <|> buildExpressionParser patOperators patTerm

patTerm = 
        try patTupleLiteral
    <|> parens patExpr
    <|> patArrayLiteral
    <|> try (APMKNI stringLiteral >>= \s -> return $ PatString ni s)
    <|> try (APMKNI charLiteral >>= \c -> return $ PatChar ni c)
    <|> try (APMKNI reserved "True" >> return (PatBool ni True))
    <|> try (APMKNI reserved "False" >> return (PatBool ni False))
    <|> try (APMKNI reserved "Void" >> return (PatVoid ni))
    <|> try (APMKNI float >>= \f -> return $ PatFloat ni f)
    <|> try (APMKNI identifier >>= \label -> return $ PatVar ni label)
    <|> try (APMKNI integer >>= \i -> return $ PatInt ni i)

asPattern = do
    ni <- MKNI
    label <- identifier
    reservedOp "@"
    px <- patExpr 
    return $ AsPattern ni label px

patArrayLiteral = do
    ni <- MKNI
    reservedOp "["
    xs <- patExpr `sepBy` (reservedOp ",")
    reservedOp "]"
    return $ PatArray ni xs

patTupleLiteral = parens $ do
    ni <- MKNI
    -- we must have at least one ","
    x1 <- patExpr
    reservedOp ","
    xs <- patExpr `sepBy1` (reservedOp ",")
    return $ PatTuple ni (x1:xs)


moduleData = do
    reserved "module"
    man <- manifest
    mss <- braces $ many moduleStmt
    return $ ModuleData man mss

manifest = starManifest <|> excludingManifest <|> includingManifest

starManifest = do
    reservedOp "*"
    return $ Excluding []

excludingManifest = do
    reserved "excluding"
    labels <- parens $ identifier `sepBy` reservedOp ","
    return $ Excluding labels

includingManifest = 
    (parens (identifier `sepBy` reservedOp ",") 
        >>= \ls -> return $ Including ls)
    <|> do
    reserved "including"
    labels <- parens $ identifier `sepBy` reservedOp ","
    return $ Including labels

moduleStmt = 
        moduleImportStmt
    <|> moduleOpStmt

moduleImportStmt = do
    reserved "import"
    label <- identifier
    man <- option (Excluding []) manifest
    as  <- option "" (reserved "as" >> identifier)
    semi
    return $ Import label man as

moduleOpStmt = do
    reserved "op"
    op <- customOp
    label <- identifier
    semi
    return $ OpDec op label



lhsExpr = expr 

readProg :: String -> (Maybe ModuleData, Seq)
readProg input = case parse beginParse "" input of
    Left err  -> error $ show err
    Right val -> val

beginParse = do 
    whiteSpace
    mdat <- optionMaybe $ moduleData
    ss <- stmtSeq 
    eof
    return $ (mdat, ss)

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
    <|> try fnStmt
    <|> fnX
    <|> typeStmt
    <|> dataStmt
    <|> try assignStmt
    <|> baseStmt
    <|> (APMKNI semi >> (return $ NOP ni))

blockStmt = do
    ni <- MKNI
    b <- block
    return $ Block ni b

decStmt = do
    ni <- MKNI
    reserved "dec"
    vs <- decExpr
    semi
    return $ Dec ni vs

decAssign = do
    ni <- MKNI
    reserved "dec"
    s <- do 
        label <- identifier
        reservedOp "<-"
        x <- expr
        return $ DecAssign ni label x
    return s

returnStmt = do
    ni <- MKNI
    reserved "return"
    x <- option (PVoid ni) expr
    semi
    return $ Return ni x

assignStmt = 
        try plainAssignStmt 
    <|> try plusAssignStmt
    <|> try minusAssignStmt
    <|> try divAssignStmt 
    <|> mulAssignStmt 

plainAssignStmt = do
    ni <- MKNI
    left    <- lhsExpr
    reservedOp "<-"
    right   <- expr
    semi
    return $ Assign ni left right

plusAssignStmt = do
    ni <- MKNI
    left    <- lhsExpr
    reservedOp "+<"
    xni <- MKNI
    right   <- expr
    semi
    return $ Assign ni left (Op2 "+" xni left right)

minusAssignStmt = do
    ni <- MKNI
    left    <- lhsExpr
    reservedOp "-<"
    xni <- MKNI
    right   <- expr
    semi
    return $ Assign ni left (Op2 "-" xni left right)

mulAssignStmt = do
    ni <- MKNI
    left    <- lhsExpr
    reservedOp "*<"
    xni <- MKNI
    right   <- expr
    semi
    return $ Assign ni left (Op2 "*" xni left right)

divAssignStmt = do
    ni <- MKNI
    left    <- lhsExpr
    reservedOp "/<"
    xni <- MKNI
    right   <- expr
    semi
    return $ Assign ni left (Op2 "/" xni left right)

fnStmt = do
    ni <- MKNI
    reserved "fn"
    n <- identifier
    params <- many identifier
    b <- block
    return $ Fn ni n params b

fnX = do
    ni <- MKNI
    reserved "fn"
    n <- identifier
    params <- many identifier
    reservedOp "->"
    xni <- MKNI
    x <- expr
    semi
    return $ Fn ni n params (Seq [Return xni x])

typeStmt = do
    ni <- MKNI
    reserved "type"
    label <- identifier
    reservedOp ":="
    tx <- typeExpr
    semi
    return $ TypeAlias ni label tx

dataStmt = do
    ni <- MKNI
    reserved "data"
    label <- identifier
    ts <- many identifier
    reservedOp ":="
    elems <- dataStmtRHSElem `sepBy1` (reservedOp "|")
    semi
    return $ DType ni label ts elems

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
    ni <- MKNI
    reserved "While"
    e       <- expr
    b       <- block
    return $ While ni e b

ifStmt = try ifStmt1 <|> ifStmt2

ifStmt1 = do
    ni <- MKNI
    reserved "If"
    e       <- expr
    b       <- block
    eb     <- elseStmt
    return $ If ni e b eb

ifStmt2 = do
    ni <- MKNI
    reserved "If"
    e       <- expr
    b       <- block
    return $ If ni e b (Seq [])

elseStmt = do
    reserved "Else"
    b <- block
    return $ b

caseStmt = do
    ni <- MKNI
    reserved "Case"
    x <- expr
    elems <- braces (many1 caseStmtElem)
    return $ Case ni x elems

caseStmtElem :: Parser CaseStmtElem
caseStmtElem = do
    px <- patExpr
    reservedOp "->"
    b <- block
    return $ (px,b)

baseStmt = do
    ni <- MKNI
    e <- expr
    semi
    return $ Stmt ni e

expr = buildExpressionParser operators superTerm

ifExpr = do
    ni <- MKNI
    reserved "if"
    e1 <- expr
    reserved "then"
    e2 <- expr
    reserved "else"
    e3 <- expr
    return $ IfX ni e1 e2 e3

lamExpr = do
    ni <- MKNI
    reservedOp "\\"
    as <- many identifier
    b  <- block
    return $ Lambda ni as b

lamX = do
    ni <- MKNI
    reservedOp "\\"
    as <- many identifier
    reservedOp "->"
    xni <- MKNI
    x  <- expr
    return $ Lambda ni as (Seq [Return xni x])

caseExpr = do
    ni <- MKNI
    reserved "case"
    x <- expr
    elems <- braces (many1 caseExprElem)
    return $ CaseX ni x elems

caseExprElem :: Parser CaseExprElem
caseExprElem = do
    px <- patExpr
    reservedOp "->"
    x <- expr
    semi
    return $ (px,x)

arrayLiteral = do
    ni <- MKNI
    reservedOp "["
    xs <- expr `sepBy` (reservedOp ",")
    reservedOp "]"
    return $ PArray ni xs

tupleLiteral = parens $ do
    ni <- MKNI
    -- we must have at least one ","
    x1 <- expr
    reservedOp ","
    xs <- expr `sepBy1` (reservedOp ",")
    return $ PTuple ni (x1:xs)

superTerm = term

term = 
    try tupleLiteral
    <|> parens expr
    <|> ifExpr
    <|> try lamExpr
    <|> lamX
    <|> caseExpr
    <|> stringP
    <|> arrayLiteral
    <|> charP
    <|> try boolP
    <|> try voidP
    <|> try floatP
    <|> var
    <|> intP

intP = do
    ni <- MKNI
    i <- integer
    return $ PInt ni i

floatP = do
    ni <- MKNI
    f <- float
    return $ PFloat ni f

voidP = do
    ni <- MKNI
    reserved "Void"
    return $ PVoid ni

boolP = trueP <|> falseP

trueP = do
    ni <- MKNI
    reserved "True"
    return $ PBool ni True

falseP = do
    ni <- MKNI
    reserved "False"
    return $ PBool ni False


charP = do
    ni <- MKNI
    c <- charLiteral
    return $ PChar ni c

stringP = do
    ni <- MKNI
    s <- stringLiteral
    return $ PString ni s

var = do
    ni <- MKNI
    label <- identifier
    return $ Var ni label
infixVar = do
    symbol "`"
    v <- var
    symbol "`"
    return v
