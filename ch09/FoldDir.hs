import Data.Char (toLower)
import System.FilePath ((</>), takeFileName, takeExtension)
import ControlledVisit (Info(..), getInfo, isDirectory, getUsefulContents)

data Iterate seed = Done     { unwrap :: seed }
                  | Skip     { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving Show

type Iterator seed = seed -> Info -> Iterate seed

foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= walk subpath seed

    walk path seed (name:names) = do
      let path' = path </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed' -> walk path seed' names
        Continue seed'
            | isDirectory info -> do
                next <- fold seed' path'
                case next of
                  done@(Done _) -> return done
                  seed'' -> walk path (unwrap seed'') names
            | otherwise -> walk path seed' names
    walk _ seed _ = return (Continue seed)

atMostThreePictures :: Iterator [FilePath]
atMostThreePictures paths info
    | length paths == 3
        = Done paths
    | isDirectory info && takeFileName path == ".svn"
        = Skip paths
    | extension `elem` [".jpg", ".png"]
        = Continue (path:paths)
    | otherwise
        = Continue paths
    where extension = map toLower (takeExtension path)
          path = infoPath info

countDirectories :: Iterator Integer
countDirectories count info =
    Continue (if isDirectory info
              then count + 1
              else count)
