str2message :: String -> String
str2message input = "Data: " ++ input

str2action :: String -> IO ()
str2action = putStrLn . str2message

main = do str2action "Start of the program"
          mapM_ (str2action . show) [1..10]
          str2action "Done!"
