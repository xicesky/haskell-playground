module Main where

-- dummy
-- might wanna run some tests or something...

-- import everything so stack build actually visits all modules
-- Also, maintain somewhat chronological order
import Sky.Lens.SimpleLens
import Sky.ControlLens.TestControlLens
import Sky.ControlLens.TestLensCompat hiding (main)
import Sky.Graph.TestGraph
import Sky.Lambda.DataStructure1
import Sky.Lambda.DataStructure2

main :: IO ()
main = do
  putStrLn "hello world"
