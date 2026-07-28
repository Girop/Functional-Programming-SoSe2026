module Main where
import Parser (parseLedger)

main :: IO ()
main = do
    let fileName = ".ledger"
    contents <- readFile fileName
    case  parseLedger fileName contents of
         Left err -> print err
         Right xs -> print xs

