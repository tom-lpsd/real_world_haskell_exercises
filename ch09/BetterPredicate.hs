import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import System.Time (ClockTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle, IOException(..))
import System.IO (IOMode(..), hClose, hFileSize, openFile,)

import RecursiveContents (getRecursiveContents)

type InfoP a =  FilePath      -- path to directory entry
             -> Permissions   -- permissions
             -> Maybe Integer -- file size (Nothing if not file)
             -> ClockTime     -- last modified
             -> a

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle ((\_ -> return Nothing) ::
                               IOException -> IO (Maybe Integer)) $
                   bracket (openFile path ReadMode) hClose $ \h -> do
                     size <- hFileSize h
                     return (Just size)

betterFind :: InfoP Bool -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ (Nothing) _ = -1

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP  = liftP (<)

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)
orP = liftP2 (||)

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

x ==? y = x `equalP` y
x &&? y = x `andP` y
x >? y = x `greaterP` y

infix 4 ==?
infixr 3 &&?
infix 4 >?
