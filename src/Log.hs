module Log ( withLogs
           , runQueueLoggingT
           , LogQueue
           ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMQueue
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Logger

type LogQueue = TMQueue (Loc, LogSource, LogLevel, LogStr)

-- | Run logging until the channel is closed and empty
unQueueLogT :: (MonadLogger m, MonadIO m) => LogQueue -> m ()
unQueueLogT chan = do
  item <- liftIO $ atomically $ readTMQueue chan
  case item of
    Nothing -> return ()
    Just (loc, src, lvl, msg) -> do
      monadLoggerLog loc src lvl msg
      unQueueLogT chan

withLogs :: (LogQueue -> IO b) -> IO b
withLogs main = do
  queue <- newTMQueueIO
  withAsync (runStderrLoggingT $ unQueueLogT queue) $ \asyn -> do
    result <- main queue
    atomically $ closeTMQueue queue
    wait asyn
    return result

runQueueLoggingT :: MonadIO m => LogQueue -> LoggingT m a -> m a
runQueueLoggingT queue = (`runLoggingT` sink)
  where sink loc src lvl msg = atomically $ writeTMQueue queue (loc,src,lvl,msg)
