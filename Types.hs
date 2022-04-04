-- author: Daniel Pátek (xpatek08)
-- VUT FIT 04/2022

module Types where
import Data.List (intercalate, intersperse)

-- one Symbol of Grammar (eighter Nonterm or Term)
type Symbol = Char
-- representation of Nonterm
type Nonterm = Symbol
-- representation of Term
type Term = Symbol

-- representation of one Rule in Grammar
data Rule = Rule {
    nonTerm :: Nonterm,
    side :: String
} deriving(Eq)

-- reprezentaion of grammar
data Grammar = Grammar {
    nonTerminals :: [Nonterm],
    terminals :: [Term],
    startState :: Nonterm,
    rules :: [Rule]
} deriving(Eq)

instance Show Rule where
    show (Rule nonTerm side) = [nonTerm] ++ "->" ++ side

instance Show Grammar where
    show (Grammar nonTerminals terminals startState rules) = 
        intersperse ',' nonTerminals ++ "\n" 
        ++ intersperse ',' terminals ++ "\n" 
        ++ [startState] ++ "\n" 
        ++ intercalate "\n" (map show rules) ++ "\n"
