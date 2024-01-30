module Main where

import PropLogic (readProp, emitTable)

main :: IO ()
main = do
    putStrLn "Please enter a proposition in propositional logic"
    putStrLn "(& = and, | = or, -> = implies, <-> = equivalence)"
    propString <- getLine
    let proposition = readProp propString
        truthTable = emitTable proposition
    putStrLn truthTable


