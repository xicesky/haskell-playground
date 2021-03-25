
{-# OPTIONS_GHC
    -Wno-unused-imports
    -Wno-unused-matches
#-}

-- http://overtond.blogspot.com/2008/07/pre.html
module OneOff.FDConstraint where

import Prelude hiding (null)
import Data.List (transpose)
import Control.Monad.State

-- containers
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- lens
import Control.Lens
import Data.Map.Lens

{-----------------------------------------------------------------------------}

data Domain
    = DUndetermined
    | DSet IntSet
    deriving (Show, Eq, Ord)

-- FD variables
newtype FDVar = FDVar { _unwrapFDVar :: Int } deriving (Ord, Eq)

type VarSupply = FDVar

data VarInfo = VarInfo { _delayedConstraints :: !FDConstraint, _domain :: !Domain }

type VarMap = Map FDVar VarInfo

data FDState = FDState { _varSupply :: !VarSupply, _varMap :: !VarMap }

{-----------------------------------------------------------------------------}

-- The FD monad
type FD a = StateT FDState [] a

type FDConstraint = FD ()

makeLenses ''FDVar

makeLenses ''FDState

makeLenses ''VarInfo

{-----------------------------------------------------------------------------}
-- Helpers / Domain

toDomain :: [Int] -> Domain
toDomain = DSet . IntSet.fromList

instance Semigroup Domain where
    DUndetermined <> a = a
    a <> DUndetermined = a
    DSet a <> DSet b   = DSet $ IntSet.intersection a b

instance Monoid Domain where
    mempty = DUndetermined

member :: Int -> Domain -> Bool
member _ DUndetermined = True
member a (DSet d) = IntSet.member a d

singleton :: Int -> Domain
singleton = DSet . IntSet.singleton

isSingleton :: Domain -> Bool
isSingleton (DSet a) = IntSet.size a == 1
isSingleton _ = False

null :: Domain -> Bool
null (DSet a) = IntSet.null a
null _ = False

isSubsetOf :: Domain -> Domain -> Bool
isSubsetOf _ DUndetermined = True
isSubsetOf DUndetermined _ = False
isSubsetOf (DSet a) (DSet b) = IntSet.isSubsetOf a b

elems :: Domain -> [Int]
elems (DSet xs) = IntSet.elems xs
elems _ = error "NYI"

difference :: Domain -> Domain -> Domain
difference (DSet a) (DSet b) = DSet $ IntSet.difference a b
difference _ _ = error "NYI"

filterLessThan :: Int -> Domain -> Domain
filterLessThan n (DSet xs) = DSet $ IntSet.filter (< n) xs
filterLessThan _ _ = error "NYI"

filterGreaterThan :: Int -> Domain -> Domain
filterGreaterThan n (DSet xs) = DSet $ IntSet.filter (> n) xs
filterGreaterThan _ _ = error "NYI"

findMax :: Domain -> Int
findMax (DSet xs) = IntSet.findMax xs
findMax _ = error "NYI"

findMin :: Domain -> Int
findMin (DSet xs) = IntSet.findMin xs
findMin _ = error "NYI"

delete :: Int -> Domain -> Domain
delete n (DSet xs) = DSet $ IntSet.delete n xs
delete _ _ = error "NYI"

{-----------------------------------------------------------------------------}

-- Run the FD monad and produce a lazy list of possible solutions.
runFD :: FD a -> [a]
runFD fd = evalStateT fd initState

initState :: FDState
initState = FDState { _varSupply = FDVar 0, _varMap = Map.empty }

instance Semigroup VarInfo where
    (<>) vi0 vi = vi0 & delayedConstraints %~ (>> vi ^. delayedConstraints)
                         & domain <>~ (vi ^. domain)

initVarInfo :: VarInfo
initVarInfo = VarInfo { _delayedConstraints = return (), _domain = DUndetermined }

instance Monoid VarInfo where
    mempty = initVarInfo

-- Get a new FDVar
{-
newVar :: ToDomain a => a -> FD FDVar
newVar d = do
    v <- use varSupply
    varSupply . unwrapFDVar += 1
    let vi = initVarInfo & domain .~ toDomain d
    varMap . at v ?= vi
    return v
-}

-- Get a new FDVar
newVar :: [Int] -> FD FDVar
newVar d = do
    v <- use varSupply
    varSupply . unwrapFDVar += 1
    let vi = initVarInfo & domain .~ toDomain d
    varMap . at v ?= vi
    return v

newVars :: Int -> [Int] -> FD [FDVar]
newVars n d = replicateM n (newVar d)

-- Look up the current domain of a variable.
lookupDom :: FDVar -> FD Domain
lookupDom x =
    use $ varMap . ix x . domain

-- Update the domain of a variable and fire all delayed constraints
-- associated with that variable.
update :: FDVar -> Domain -> FDConstraint
update x i = do
    vi <- use $ varMap . ix x
    -- use :: MonadState s m => Getting a s a -> m a
    -- varMap :: Lens' FDState VarMap
    -- ix x ::
    varMap . ix x . domain .= i
    vi ^. delayedConstraints

-- Add a new constraint for a variable to the constraint store.
addConstraint :: FDVar -> FDConstraint -> FDConstraint
addConstraint x constraint = do
    varMap . ix x . delayedConstraints %= (>> constraint)

-- Useful helper function for adding binary constraints between FDVars.
type BinaryConstraint = FDVar -> FDVar -> FDConstraint
addBinaryConstraint :: BinaryConstraint -> BinaryConstraint
addBinaryConstraint f x y = do
    let constraint  = f x y
    constraint
    -- watches?
    addConstraint x constraint
    addConstraint y constraint

-- Constrain a variable to a particular value.
hasValue :: FDVar -> Int -> FDConstraint
var `hasValue` val = do
    vals <- lookupDom var
    guard $ val `member` vals
    let i = singleton val
    when (i /= vals) $ update var i

-- Constrain a variable to be different from a particular value.
hasNotValue :: FDVar -> Int -> FDConstraint
var `hasNotValue` val = do
    vals <- lookupDom var
    let vals' = delete val vals
    when (vals /= vals') $ update var vals'

-- Constrain two variables to have the same value.
same :: FDVar -> FDVar -> FDConstraint
same = addBinaryConstraint $ \x y -> do
    xv <- lookupDom x
    yv <- lookupDom y
    let i = xv <> yv
    guard $ not $ null i
    when (i /= xv) $ update x i
    when (i /= yv) $ update y i

-- Constrain two variables to have different values.
different :: FDVar -> FDVar -> FDConstraint
different = addBinaryConstraint $ \x y -> do
    xv <- lookupDom x
    yv <- lookupDom y
    guard $ not (isSingleton xv) || not (isSingleton yv) || xv /= yv
    when (isSingleton xv && xv `isSubsetOf` yv) $
        update y (yv `difference` xv)
    when (isSingleton yv && yv `isSubsetOf` xv) $
        update x (xv `difference` yv)

-- Constrain a list of variables to all have different values.
varsAllDifferent :: [FDVar] -> FDConstraint
varsAllDifferent (x:xs) = do
    mapM_ (different x) xs
    varsAllDifferent xs
varsAllDifferent _ = return ()

-- Constrain one variable to have a value less than the value of another
-- variable.
lessThan :: FDVar -> FDVar -> FDConstraint
lessThan = addBinaryConstraint $ \x y -> do
    xv <- lookupDom x
    yv <- lookupDom y
    let xv' = filterLessThan (findMax yv) xv
    let yv' = filterGreaterThan (findMin xv) yv
    guard $ not $ null xv'
    guard $ not $ null yv'
    when (xv /= xv') $ update x xv'
    when (yv /= yv') $ update y yv'

{-----------------------------------------------------------------------------}

-- Get all solutions for a constraint without actually updating the
-- constraint store.
solutions :: FD a -> FD [a]
solutions constraint = get <&> evalStateT constraint

-- Label variables using a depth-first left-to-right search.
varsLabelling :: [FDVar] -> FD [Int]
varsLabelling = mapM label where
    label var = do
        vals <- lookupDom var
        val <- lift $ elems vals
        var `hasValue` val
        return val

dump :: [FDVar] -> FD [Domain]
dump = mapM lookupDom

{-----------------------------------------------------------------------------}
-- sudoku

type Sudoku = [Int]

test :: Sudoku
test = join [
    [ 0, 0, 0, 0, 8, 0, 0, 0, 0],
    [ 0, 0, 0, 1, 0, 6, 5, 0, 7],
    [ 4, 0, 2, 7, 0, 0, 0, 0, 0],
    [ 0, 8, 0, 3, 0, 0, 1, 0, 0],
    [ 0, 0, 3, 0, 0, 0, 8, 0, 0],
    [ 0, 0, 5, 0, 0, 9, 0, 7, 0],
    [ 0, 5, 0, 0, 0, 8, 0, 0, 6],
    [ 3, 0, 1, 2, 0, 4, 0, 0, 0],
    [ 0, 0, 6, 0, 1, 0, 0, 0, 0]
    ]

testSoln :: Sudoku
testSoln = join [
    [5,6,7,4,8,3,2,9,1],
    [9,3,8,1,2,6,5,4,7],
    [4,1,2,7,9,5,3,6,8],
    [6,8,9,3,7,2,1,5,4],
    [7,4,3,6,5,1,8,2,9],
    [1,2,5,8,4,9,6,7,3],
    [2,5,4,9,3,8,7,1,6],
    [3,7,1,2,6,4,9,8,5],
    [8,9,6,5,1,7,4,3,2]
    ]

displayPuzzle :: Sudoku -> String
displayPuzzle = unlines . map show . chunk 9

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = ys : chunk n zs where
    (ys, zs) = splitAt n xs

sudoku :: Sudoku -> [Sudoku]
sudoku puzzle = runFD $ do
    vars <- newVars 81 [1..9]
    zipWithM_ (\x n -> when (n > 0) (x `hasValue` n)) vars puzzle
    mapM_ varsAllDifferent (rows vars)
    mapM_ varsAllDifferent (columns vars)
    mapM_ varsAllDifferent (boxes vars)
    varsLabelling vars

rows, columns, boxes :: [a] -> [[a]]
rows = chunk 9
columns = transpose . rows
boxes = concatMap (map concat . transpose) . chunk 3 . chunk 3 . chunk 3

printSudoku :: Sudoku -> IO ()
printSudoku = putStr . unlines . map displayPuzzle . sudoku

-- >>> printSudoku test

checkSudoku :: Sudoku -> Sudoku -> IO ()
checkSudoku puzzle solution = case sudoku puzzle of
    []  -> putStrLn "No solution!"
    [n] -> do
        putStr $ displayPuzzle n
        putStrLn $ "Correct: " ++ show (n == solution)
    ns  -> do
        putStrLn "Multiple solutions!"
        putStr $ unlines $ map displayPuzzle ns

{-----------------------------------------------------------------------------}
-- n Queens

nQueens :: Int -> FD [FDVar]
nQueens n = do
    qs <- newVars n [1..n]
    safeQueens qs
    return qs

safeQueens :: [FDVar] -> FDConstraint
safeQueens [] = return ()
safeQueens (q : qs) = do
    safeQueen qs q 1
    safeQueens qs

safeQueen :: [FDVar] -> FDVar -> Int -> FDConstraint
safeQueen [] _ _ = return ()
safeQueen (q : qs) q0 d0 = do
    q0 `hasNotValue` d0
    -- FIXME
    -- abs (q0 - q) `hasNotValue` d0
    safeQueen qs q0 (d0 + 1)

