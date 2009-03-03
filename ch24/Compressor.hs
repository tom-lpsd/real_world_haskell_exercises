import Control.Concurrent (forkIO)
import Control.Exception (handle, IOException)
import Control.Monad (forever, liftM)
import qualified Data.ByteString.Lazy as L
--import System.Console.Readline (readline)

import Codec.Compression.GZip (compress)

main = do
  maybeline <- liftM Just getLine
  case maybeline of
    Nothing -> return ()
    Just "" -> return ()
    Just name -> do
         handle (print :: IOException -> IO ()) $ do
                   content <- L.readFile name
                   forkIO (compressFile name content)
                   return ()
         main
    where compressFile path = L.writeFile (path ++ ".gz") . compress
