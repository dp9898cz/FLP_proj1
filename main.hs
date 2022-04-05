-- flp21-fun
-- author: Daniel Pátek (xpatek08)
-- VUT FIT 04/2022

module Main where

import Types (Grammar)
import System.Environment ( getArgs )
import System.IO (openFile, getContents, hGetContents, IOMode (ReadMode))
import Parser (parseGrammar)
import Simplify (firstStep, secondStep)

handleArgs :: [String] -> IO String
handleArgs [args] = simplify args <$> getContents
handleArgs [args, file] = do
    contentRef <- openFile file ReadMode
    content <- hGetContents contentRef
    return $ simplify args content
handleArgs _ = error "Wrong number of arguments. Run program with -h to show help.\n"

simplify :: String -> String -> String
simplify "-i" content = show $ parseGrammar content
simplify "-1" content = show $ firstStep $ parseGrammar content
simplify "-2" content = show $ secondStep $ parseGrammar content
simplify "-h" _ = "Simplify BKG usage\n" ++
    "    ./flp21-fun [-i|-1|-2] [filename] \n" ++
    "    [-i] Print grammar after parsing.\n" ++
    "    [-1] Print grammar after first step of algoritm.\n" ++
    "    [-2] Print grammar after completed algoritm.\n"
simplify f _ = error "Unknown argument: \"" ++ f ++ "\". Run program with -h to show help.\n"

main :: IO ()
main = getArgs  >>= handleArgs >>= putStr