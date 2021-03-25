
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
import NonDetSearch.MTL (searchMTL)
import NonDetSearch.HaskellWiki (searchHWiki)
import NonDetSearch.LogicT (searchLogicT, searchLogicBFS)
import NonDetSearch.Amb (searchAmb)
import NonDetSearch.FusedEffects (searchFE)
import NonDetSearch.Freer (searchFreer)
import NonDetSearch.Polysemy (searchPolysemy)
import NonDetSearch.CustomEff (searchCustomEff)
import NonDetSearch.ATPS09 (searchATPS09dfs, searchATPS09bfs)

searchFuns :: [(String, Int, SFun)]
searchFuns =
    [   ("searchList",      99, searchList)
    ,   ("searchMTL",       99, searchMTL)
    ,   ("searchHWiki",     99, searchHWiki)
    ,   ("searchLogicT",    99, searchLogicT)
    -- ,   ("searchLogicBFS",  99, searchLogicBFS)    -- too slow (> 3s)
    ,   ("searchAmb",       99, searchAmb)
    ,   ("searchFE",        99, searchFE)
    ,   ("searchFreer",     99, searchFreer)
    ,   ("searchPolysemy",  99, searchPolysemy)
    ,   ("searchCustomEff", 99, searchCustomEff)
    ,   ("searchATPS09dfs", 99, searchATPS09dfs)
    ,   ("searchATPS09bfs", 99, searchATPS09bfs)
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
    | n <- [9]  -- [7,8] -- 8 is already too slow
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
