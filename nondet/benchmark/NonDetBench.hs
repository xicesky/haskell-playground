
{-
stack bench --benchmark-arguments '--output=$benchmark.html' && open nondet-benchmarks.html
stack bench --ghc-options=-O1 --benchmark-arguments '--output=$benchmark.html' && open nondet-benchmarks.html


http://www.serpentine.com/criterion/tutorial.html
-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Criterion.Main

import Control.Monad (join)
import NonDet.Class
import NonDetSearch.SearchImpl
import NonDetSearch.MTL (searchMTL)
import NonDetSearch.HaskellWiki (searchHWiki)
import qualified NonDetSearch.SearchImplCustomEff as OLD
import qualified NonDetSearch.ATPS09 as ATPS

searchFuns :: [(String, Int, SFun)]
searchFuns =
    [   ("searchList",      8, searchList)
    ,   ("searchMTL",       8, searchMTL)
    ,   ("searchHWiki",     8, searchHWiki)
    ,   ("searchND",        7, searchND)
    ,   ("searchNDOld",     8, SFun OLD.searchND)
    ,   ("searchATPS09",    7, ATPS.search)
    ]

-- Awkward!!
run :: forall a. SFun -> (forall m. NonDet m => Int -> m a) -> Int -> [a]
run search problem size = getFun search (problem size)

benchPG :: [Benchmark]
benchPG = [ bgroup ("pidgeonHole' " ++ show n)
        [   bench fname $ nf (run f pidgeonHole') n
        | (fname, nmax, f) <- searchFuns
        , n <= nmax
        ]
    | n <- [7]  -- [7,8] -- 8 is already too slow
    ]

benchPT :: [Benchmark]
benchPT = [ bgroup ("pytriple' " ++ show n)
        [   bench fname $ nf (run f pytriple') n
        | (fname, _, f) <- searchFuns
        --, n <= nmax
        ]
    | n <- [160]
    ]

benchmarkSearch :: [Benchmark]
benchmarkSearch = join
    [   benchPT
    ,   benchPG
    ]

benchmarks :: [Benchmark]
benchmarks = benchmarkSearch
    -- ++ [bench "const" (whnf const ())]

main :: IO ()
main = defaultMain benchmarks
