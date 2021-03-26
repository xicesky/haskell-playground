
{--
while true; do \
oneoffs/.stack-work/dist/*/Cabal*/build/oneoff-concurrent-demo/oneoff-concurrent-demo \
+RTS -N2; \
done
--}

module Main where

import Control.Concurrent

p :: IO Int
p = do
  num <- newEmptyMVar
  _ <- forkIO $ putMVar num 3
  _ <- forkIO $ putMVar num 4
  takeMVar num

main :: IO ()
main = do
  v <- p
  putStr $ show v
