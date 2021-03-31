
{-# OPTIONS_GHC
    -Wno-incomplete-patterns
#-}
module Problems.HasCallStack where

import GHC.Stack (HasCallStack)

erroneous :: HasCallStack => Int -> Int
erroneous x
    | odd x = x
    | otherwise = error "invalid"

blah :: HasCallStack => [Int]
blah = erroneous <$> [1..10]

{-
Source: https://www.parsonsmatt.org/2017/07/29/using_ghc_callstacks.html
If any function in the chain does not have HasCallStack in the signature,
then nothing above that will be represented in the stack trace.
This is a pretty big limitation.
-}
blubb :: HasCallStack => Int -> Int
blubb x = x - maximum blah

runme :: HasCallStack => Int
runme = blubb 5

headNoCallStack :: [a] -> a
headNoCallStack (x:xs) = x
headNoCallStack [] = error "nope"

headWithCallstack :: HasCallStack => [a] -> a
headWithCallstack (x:xs) = x
headWithCallstack [] = error "nope"
