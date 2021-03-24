
{-
stack bench --benchmark-arguments '--output=$benchmark.html' && open nondet-benchmarks.html
stack bench --ghc-options=-O1 --benchmark-arguments '--output=$benchmark.html' && open nondet-benchmarks.html


http://www.serpentine.com/criterion/tutorial.html
-}

{-# OPTIONS_GHC
    -Wno-unused-top-binds
    -Wno-unused-imports
#-}

import Criterion.Main

import Control.Monad (join)
import NonDet.Class
import NonDetSearch.SearchImpl
import NonDetSearch.MTL (searchMTL)
import NonDetSearch.HaskellWiki (searchHWiki)
import NonDetSearch.LogicT (searchLogicT, searchLogicBFS)
import NonDetSearch.Amb (searchAmb)
import NonDetSearch.FusedEffects (searchFE)
import NonDetSearch.SearchImplCustomEff (searchNDOld)
import NonDetSearch.ATPS09 (searchATPS09dfs, searchATPS09bfs)

searchFuns :: [(String, Int, SFun)]
searchFuns =
    [   ("searchList",      8, searchList)
    ,   ("searchMTL",       8, searchMTL)
    ,   ("searchHWiki",     8, searchHWiki)
    ,   ("searchLogicT",    8, searchLogicT)
    --,   ("searchLogicBFS",  7, searchLogicBFS)    -- too slow (> 3s)
    ,   ("searchAmb",       8, searchAmb)
    ,   ("searchFE",        8, searchFE)
    ,   ("searchND",        7, searchND)
    ,   ("searchNDOld",     8, searchNDOld)
    ,   ("searchATPS09dfs", 7, searchATPS09dfs)
    ,   ("searchATPS09bfs", 7, searchATPS09bfs)
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
