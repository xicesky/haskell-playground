
{-
stack bench --benchmark-arguments '--output=$benchmark.html' && open nondet-benchmarks.html
stack bench --ghc-options=-O1 --benchmark-arguments '--output=$benchmark.html' && open nondet-benchmarks.html


http://www.serpentine.com/criterion/tutorial.html
-}
import Criterion.Main

import NonDet.Class
import NonDetSearch.SearchImpl
import NonDetSearch.MTL (searchMTL)
import qualified NonDetSearch.SearchImplCustomEff as OLD
import qualified NonDetSearch.ATPS09 as ATPS

searchFuns :: [(String, Int, SFun)]
searchFuns =
    [   ("searchList",      8, searchList)
    ,   ("searchMTL",       8, searchMTL)
    ,   ("searchND",        7, searchND)
    ,   ("searchNDOld",     8, SFun OLD.searchND)
    ,   ("searchATPS09",    7, ATPS.search)
    ]

-- Awkward!!
pg :: SFun -> Int -> [[Int]]
pg f n = getFun f (pidgeonHole' n)

benchmarkSearch :: [Benchmark]
benchmarkSearch = 
    [ bgroup ("pidgeonHole' " ++ show n)
        [   bench fname $ nf (pg f) n
        | (fname, nmax, f) <- searchFuns
        , n <= nmax
        ]
    | n <- [7]  -- [7,8] -- 8 is already too slow
    ]

benchmarks :: [Benchmark]
benchmarks = benchmarkSearch
    -- ++ [bench "const" (whnf const ())]

main :: IO ()
main = defaultMain benchmarks
