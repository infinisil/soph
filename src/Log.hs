module Log ( withLogs
           , runQueueLoggingT
           , LogQueue
           ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMQueue
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Logger
import           Control.Monad.Trans            (lift)

type LogQueue = TMQueue (Loc, LogSource, LogLevel, LogStr)

-- | Run logging until the channel is closed and empty
unQueueLogT :: LogQueue -> LoggingT IO ()
unQueueLogT chan = do
  item <- lift $ atomically $ readTMQueue chan
  case item of
    Nothing -> return ()
    Just (loc, src, lvl, msg) -> do
      monadLoggerLog loc src lvl msg
      unQueueLogT chan

withLogs :: (LogLevel -> Bool) -> (LogQueue -> IO b) -> IO b
withLogs logFilter main = do
  queue <- newTMQueueIO
  withAsync (unLog queue) $ \asyn -> do
    result <- main queue
    atomically $ closeTMQueue queue
    wait asyn
    return result
  where unLog queue = runStderrLoggingT
          $ filterLogger (const logFilter)
          $ unQueueLogT queue

runQueueLoggingT :: MonadIO m => LogQueue -> LoggingT m a -> m a
runQueueLoggingT queue = (`runLoggingT` sink)
  where sink loc src lvl msg = atomically $ writeTMQueue queue (loc,src,lvl,msg)
