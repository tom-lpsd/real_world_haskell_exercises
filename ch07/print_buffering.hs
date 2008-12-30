import System.IO(stdin, hGetBuffering)

main = do bmode <- hGetBuffering stdin
          (putStrLn . show) bmode
