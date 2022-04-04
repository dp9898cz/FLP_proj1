-- author: Daniel Pátek (xpatek08)
-- VUT FIT 04/2022

module Parser where
import Types (Grammar (..), Nonterm, Term, Rule (..))
import Data.List (sort)

parseGrammar :: String -> Grammar
parseGrammar input = parseCheckedGrammar( checkRulesValid(lines input))

checkRulesValid :: [String] -> [String]
checkRulesValid (n:t:s:rules) = n:t:s:rules
checkRulesValid _ = error "Error when parsing grammar rules."

parseCheckedGrammar :: [String] -> Grammar
parseCheckedGrammar (nterms : terms : start : rules)
    | null rules =
        error "Grammar does not contain any rules."
    | not (isDuplicatesFree (parseTerms terms)) =
        error ("List of terms has duplicates: " ++ terms)
    | not (isDuplicatesFree (parseNterms nterms)) =
        error ("List of nonterms has duplicates: " ++ nterms)
    | (last terms == ',') || (last nterms == ',') =
        error "Last character of terms or nterms is ','."
    | not (isRepeatedFree terms) =
        error "List of terms has repeating values. (most probably \",,\")"
    | not (isRepeatedFree nterms) =
        error "List of nterms has repeating values. (most probably \",,\")"
    | not (isDuplicatesFree (parseRules rules)) = 
        error "List of rules has duplicates."
    | otherwise =
        Grammar (parseNterms nterms) (parseTerms terms) (parseStart start) (parseRules rules)
parseCheckedGrammar _ = error "Grammar has wrong format."

parseTerms :: String -> [Term]
parseTerms terms = sort (map parseTerm (wordsWhen (==',') terms))
    where
        parseTerm term =
            if (length term == 1) && head term `elem` ['a'..'z']
            then
                head term
            else
                error ("Terminal has bad syntax: " ++ term)

parseNterms :: String -> [Nonterm]
parseNterms nterms = sort (map parseNterm (wordsWhen (==',') nterms))
    where
        parseNterm nterm =
            if (length nterm == 1) && head nterm `elem` ['A'..'Z']
            then
                head nterm
            else
                error ("Nonterminal has bad syntax: " ++ nterm)



parseStart :: String -> Nonterm
parseStart nterm =
    if (length nterm == 1) && head nterm `elem` ['A'..'Z'] then
        head nterm
    else
        error ("Start nonterminal has wrong syntax: " ++ nterm)

parseRules :: [String] -> [Rule]
parseRules = map parseRule
    where
        parseRule rule = composeRule (wordsWhen (=='>') rule)
            where
                composeRule [left,right] =
                    if leftSideCheck left && rightSideCheck right then
                        Rule (head left) right
                    else
                        error ("Bad syntax of rule: " ++ rule)
                    where
                        leftSideCheck :: String -> Bool
                        leftSideCheck (n:nx) = n `elem` ['A'..'Z'] && head nx == '-' --todo check actual grammar somehow
                        leftSideCheck _ = False

                        rightSideCheck :: String -> Bool
                        rightSideCheck [] = False
                        rightSideCheck [t] = t `elem` (['a'..'z'] ++ ['#'] ++ ['A'..'Z']) || (t == '#') --todo check actual grammar
                        rightSideCheck (t:tx) = t `elem` (['a'..'z'] ++ ['A'..'Z']) && rightSideCheck tx --todo check actual grammar
                composeRule _ = error ("In composing new rule - Bad syntax of rule: " ++ rule)

-- split String on specific character
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p str = case dropWhile p str of
    "" -> []
    s -> w : wordsWhen p a
        where (w, a) = break p s

-- find duplicates
isDuplicatesFree :: (Eq a) => [a] -> Bool
isDuplicatesFree [] = True
isDuplicatesFree (x:xs) = x `notElem` xs && isDuplicatesFree xs

-- check if the list has any repeating characters
isRepeatedFree :: String -> Bool
isRepeatedFree (x:y:xs) = (x /= y) && isRepeatedFree (y:xs)
isRepeatedFree _ = True