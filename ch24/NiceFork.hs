module NiceFork 
    (
      ThreadManager
    , newManager
    , forkManaged
    , getStatus
    , waitFor
    , waitAll
    ) where

import Control.Concurrent
import Control.Monad (join)
import Control.Exception (Exception, try)
import qualified Data.Map as M

data ThreadStatus = Running | Finished | Threw Exception deriving (Eq, Show)

newManager :: IO ThreadManager

forkManaged :: ThreadManager -> IO () -> IO ThreadId

getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)

waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)

waitAll :: ThreadManager -> IO ()

newtype ThreadManager =
    Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
    deriving Eq

newManager = Mgr `fmap` newMVar M.empty

forkManaged (Mgr mgr) body =
    modifyMVar mgr $ \m -> do
      state <- newEmptyMVar
      tid <- forkIO $ do
               result <- try body
               putMVar state (either Threw (const Finished) result)
      return (M.insert tid state m, tid)

getStatus (Mgr mgr) tid =
    modifyMVar mgr $ \m ->
        case M.lookup tid m of
          Nothing -> return (m, Nothing)
          Just st -> tryTakeMVar st >>= \mst -> case mst of
                       Nothing -> return (m, Just Running)
                       Just sth -> return (M.delete tid m, Just sth)

waitFor (Mgr mgr) tid = do
  join . modifyMVar mgr $ \m ->
               return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
                          (Nothing, _) -> (m, return Nothing)
                          (Just st, m') -> (m', Just `fmap` takeMVar st)

waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
    where elems m = return (M.empty, M.elems m)
