-- author: Daniel Pátek (xpatek08)
-- VUT FIT 04/2022

module Simplify where
import Types (Grammar (Grammar, rules, terminals), Nonterm, Rule (Rule, nonTerm), Term)
import Data.List (union, sort, intersect)

-- decompose nonterm from rule
getNonTermFromRule :: Rule -> Nonterm
getNonTermFromRule = nonTerm

-- checks wheather can grammar actually generate something
isGrammarEmpty :: Nonterm -> [Rule] -> Bool
isGrammarEmpty s r
    | null r =
        True
    | all ((`notElem` [s]) . getNonTermFromRule) r =
        True
    | otherwise =
        False


-- performes first step of the algoritm
firstStep :: Grammar -> Grammar
firstStep oldGrammar@(Grammar n t s r) =
    if isGrammarEmpty s (filterRules r filteredNonterms allFilteredSymbols) then
        error "Grammar is not producing any words."
    else
        Grammar (sort (filteredNonterms `union` [s])) t s (filterRules r filteredNonterms allFilteredSymbols)
        where
            filteredNonterms = getRightNonterms oldGrammar []
            allFilteredSymbols = filteredNonterms ++ t

-- return list of filtered nonterms
-- recursively search the grammar rules
-- once it does not change -> return these nonterms
getRightNonterms :: Grammar -> [Nonterm] -> [Nonterm]
getRightNonterms oldGrammar@(Grammar n t s r) previous =
    if previous == getNontermsFromRules r (t++previous)
    then
        getNontermsFromRules r (t++previous)
    else
        getRightNonterms oldGrammar (getNontermsFromRules r (t++previous))

-- check each rule recursively and return list of Nonterms
getNontermsFromRules :: [Rule] -> [Nonterm] -> [Nonterm]
getNontermsFromRules (Rule left right:xs) previousNonterms =
    -- if all terms/nonterms from right side of the rule are in previous list
    if all (`elem` previousNonterms) right
    then
        -- add main nonterm of the rule to the list 
            getNontermsFromRules xs previousNonterms `union` [left]
    else
        -- continue without the rule
        getNontermsFromRules xs previousNonterms
getNontermsFromRules [] _ = []

-- filter those rules that matter and throws away those what dont
-- depends on filtered symbols
filterRules :: [Rule] -> [Nonterm] -> [Nonterm] -> [Rule]
filterRules (currentRule@(Rule left right):xs) filteredNonTerms filteredAllSymbols =
    if left `elem` filteredNonTerms && all (`elem` filteredAllSymbols) right
    then
        currentRule : filterRules xs filteredNonTerms filteredAllSymbols
    else
        filterRules xs filteredNonTerms filteredAllSymbols
filterRules [] _ _ = []


-----------------------------------------------------
-----------------------------------------------------

-- second step of the algoritm - removing unreachable symbols
secondStep :: Grammar -> Grammar
secondStep g@(Grammar n t s r) =
    removeUnreachableSymbols (firstStep g) [s]

removeUnreachableSymbols :: Grammar -> [Nonterm] -> Grammar
removeUnreachableSymbols grammar@(Grammar n t s p) previous =
    if previous == nonterms then
        Grammar 
            (sort nonterms)
            (sort terminals) 
            s 
            (filterRules (rules grammar) nonterms (terminals++nonterms))
    else
        removeUnreachableSymbols grammar nonterms
    where
        filteredRules = filterRules p previous (n++t)
        nonterms = previous `union` getSymbolsFromRulesRight filteredRules n
        terminals = getSymbolsFromRulesRight filteredRules t

-- get eightr nonterms or terms from right sides of rules
-- terms / nonterms specified in symbols attribute
-- doing intersection of these symbols
getSymbolsFromRulesRight :: [Rule] -> [Nonterm] -> [Nonterm]
getSymbolsFromRulesRight (Rule _ right:xs) symbols = 
    (right `intersect` symbols) `union` getSymbolsFromRulesRight xs symbols
getSymbolsFromRulesRight [] _ = []