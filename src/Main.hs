module Main where

import           Control.Concurrent.Async.Pool

main :: IO ()
main = do
  pool <- createPool
  group <- createTaskGroup pool 1
  mapConcurrently group return [0]
  return ()
