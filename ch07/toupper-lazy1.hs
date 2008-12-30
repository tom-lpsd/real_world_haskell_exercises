import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
  inh <- openFile "very-large-input" ReadMode
  outh <- openFile "output" WriteMode
  inpStr <- hGetContents inh
  let result = processData inpStr
  hPutStr outh result
--  putStrLn (take 100 inpStr)
  hClose inh
  hClose outh

processData :: String -> String
processData = map toUpper
