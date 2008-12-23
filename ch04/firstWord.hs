import System.Environment (getArgs)

firstWord :: String -> String
firstWord [] = []
firstWord s = head (words s)

firstWords :: String -> [String]
firstWords s = map firstWord (lines s)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
              [input, output] -> interactWith function input output
              _ -> putStrLn "error: exactly two arguments needed"
          myFunction = unlines . firstWords
