import Data.Char(toUpper)

main = do
  inpStr <- readFile "/Users/tom/mnt/APPLESEED.mp4"
  writeFile "output.txt" (map toUpper inpStr)