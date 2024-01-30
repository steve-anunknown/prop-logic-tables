module PropLogic (readProp, evalProp, emitTable) where

import Data.Char (isAlpha, isSpace, digitToInt, intToDigit)
import Data.List (nub, intersperse)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (fromMaybe)
import Numeric (showIntAtBase)

data Token = Tvar Char
            | Tneg      -- '~'
            | Tand      -- '&'
            | Tor       -- '|'
            | Timpl     -- "->"
            | Tequiv    -- "<->"
            | Tlparen
            | Trparen
            deriving (Show, Eq)

data Proposition = Var Char
                 | Neg Proposition
                 | And Proposition Proposition
                 | Or Proposition Proposition
                 | Impl Proposition Proposition
                 | Equiv Proposition Proposition
                 deriving (Eq)

instance Show Proposition where
    show (Var v) = [v]
    show (Neg p) = "~" ++ show p
    show (And p q) = "(" ++ show p ++ " & " ++ show q ++ ")"
    show (Or p q) = "(" ++ show p ++ " | " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
    show (Equiv p q) = "(" ++ show p ++ " <-> " ++ show q ++ ")"


tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
    | isSpace c = tokenize cs
    | isAlpha c = Tvar c : tokenize cs
    | c == '~'  = Tneg : tokenize cs
    | c == '&'  = Tand : tokenize cs
    | c == '|'  = Tor : tokenize cs
    | c == '('  = Tlparen : tokenize cs
    | c == ')'  = Trparen : tokenize cs
    | c == '-'  = case cs of
                    '>':rest -> Timpl : tokenize rest
                    _ -> error "Invalid token"
    | c == '<'  = case cs of
                    '-':rest -> case rest of
                                  '>':rest' -> Tequiv : tokenize rest'
                                  _ -> error "Invalid token"
                    _ -> error "Invalid token"
    | otherwise = error ("Unrecognized character: " ++ [c])

-- Parser function
parse :: [Token] -> Proposition
parse tokens = let (prop, rest) = parseEquiv tokens in
    if null rest then prop
    else error ("Unconsumed tokens" ++ (show rest))

parseEquiv :: [Token] -> (Proposition, [Token])
parseEquiv tokens = 
    let (left, rest) = parseImpl tokens in
    case rest of
        (Tequiv:rest') -> let (right, rest'') = parseEquiv rest' in (Equiv left right, rest'')
        _ -> (left, rest)

parseImpl :: [Token] -> (Proposition, [Token])
parseImpl tokens = 
    let (left, rest) = parseOr tokens in
    case rest of
        (Timpl:rest') -> let (right, rest'') = parseImpl rest' in (Impl left right, rest'')
        _ -> (left, rest)

-- Parse 'Or'
parseOr :: [Token] -> (Proposition, [Token])
parseOr tokens = 
    let (left, rest) = parseAnd tokens in
    case rest of
        (Tor:rest') -> let (right, rest'') = parseOr rest' in (Or left right, rest'')
        _ -> (left, rest)

-- Parse 'And'
parseAnd :: [Token] -> (Proposition, [Token])
parseAnd tokens = 
    let (left, rest) = parseNeg tokens in
    case rest of
        (Tand:rest') -> let (right, rest'') = parseAnd rest' in (And left right, rest'')
        _ -> (left, rest)

-- Parse 'Neg' and 'Var', which have the highest precedence
parseNeg :: [Token] -> (Proposition, [Token])
parseNeg (Tneg:tokens) = let (prop, rest) = parseNeg tokens in (Neg prop, rest)
parseNeg (Tvar v:tokens) = (Var v, tokens)
parseNeg (Tlparen:tokens) = 
    let (prop, rest) = parseImpl tokens in
    case rest of
        (Trparen:rest') -> (prop, rest')
        _ -> error "Missing right parenthesis"
parseNeg _ = error "Invalid token sequence"

readProp :: String -> Proposition
readProp = parse . tokenize

-- create set of variables in a proposition
findVars :: Proposition -> [Char]
findVars = nub . vars
    where
        vars :: Proposition -> [Char]
        vars prop = case prop of
            Var v     -> [v]
            Neg p     -> vars p
            And p q   -> vars p ++ vars q
            Or p q    -> vars p ++ vars q
            Impl p q  -> vars p ++ vars q
            Equiv p q -> vars p ++ vars q

evalProp :: Proposition -> Map Char Bool -> Bool
evalProp (Var p) env = fromMaybe (error "This should not happen") (Data.Map.lookup p env)
evalProp (Neg p) env  = not $ evalProp p env
evalProp (And p q) env = (evalProp p env) && (evalProp q env)
evalProp (Or p q)  env = (evalProp p env) || (evalProp q env)
evalProp (Impl p q) env = (not (evalProp p env) || (evalProp q env))
evalProp (Equiv p q) env = (evalProp p env) == (evalProp q env)


emitTable :: Proposition -> String
emitTable prop =
    let 
        variables = [[c] | c <- findVars prop] :: [String]
        width = length variables
        numRows = 1 + 2 ^ width

        firstRow = concat $ intersperse " || " (variables ++ [show prop]) :: String
        restRows = [showIntAtBase 2 intToDigit n "" | n <- [0..numRows-2]] :: [String]
        
        findBits n = floor (logBase 2 (fromIntegral n)) + 1

        paddings = [width - 1] ++ [width - findBits n | n <- [1..numRows-2]] :: [Int]
        restRowsPadded = [replicate p '0' ++ row | (p, row) <- zip paddings restRows] :: [String]

        charToBool c = toEnum $ digitToInt c :: Bool
        valuesPerRow = map (map charToBool) restRowsPadded :: [[Bool]]

        envs = [fromList $ zip (findVars prop) vals | vals <- valuesPerRow] :: [Map Char Bool]
        results = [evalProp prop env | env <- envs] :: [Bool]
        lastColumn = map (\x -> if x then '1' else '0') results :: [Char]

        completeRows = [row ++ [res] | (row, res) <- zip restRowsPadded lastColumn] :: [String]
        finalRows = [concat $ intersperse " || " (map (\x -> [x]) c) | c <- completeRows] :: [String]

        table = [firstRow] ++ [replicate (length firstRow) '-'] ++ finalRows :: [String]
    in
        unlines table :: String




