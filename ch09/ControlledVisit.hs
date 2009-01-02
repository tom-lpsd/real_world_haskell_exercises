module ControlledVisit where

import Control.Monad (liftM, forM, filterM)
import Control.Exception (bracket, handle, IOException(..))

import System.Directory (Permissions(..), getModificationTime,
                         getPermissions, getDirectoryContents)
import System.Time (ClockTime(..))
import System.FilePath (takeExtension, (</>))
import System.IO (IOMode(..), hClose, hFileSize, openFile,)

data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe ClockTime
    } deriving (Eq, Ord, Show)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path: map (path </>) names)
  liftM concat $ forM (order contents) $ \info -> do
                        if isDirectory info && infoPath info /= path
                          then traverse order (infoPath info)
                          else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ((\_ -> return Nothing) :: IOException -> IO (Maybe a))
                     (Just `liftM` act)

getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

sizeP :: Info -> Integer
sizeP i = case infoSize i of
            Just size -> size
            _ -> -1

liftP :: (a -> b -> c) -> (d -> a) -> b -> d -> c
liftP q f k i = f i `q` k

greaterP, lesserP :: (Ord a) => (c -> a) -> a -> c -> Bool
greaterP = liftP (>)
lesserP  = liftP (<)

equalP :: (Eq a) => (c -> a) -> a -> c -> Bool
equalP = liftP (==)

liftP2 :: (a -> b -> c) -> (Info -> a) -> (Info -> b) -> Info -> c
liftP2 q f g i = f i `q` g i

andP = liftP2 (&&)
orP = liftP2 (||)

liftPath :: (FilePath -> a) -> Info -> a
liftPath f i = f $ infoPath i

x ==? y = x `equalP` y
x &&? y = x `andP` y
x >? y = x `greaterP` y

find :: (Info -> Bool) -> ([Info] -> [Info]) -> FilePath -> IO [Info]
find p order path = traverse order path >>= filterM (return . p)
