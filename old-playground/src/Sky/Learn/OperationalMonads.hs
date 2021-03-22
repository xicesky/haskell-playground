
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}

module Sky.Learn.OperationalMonads where

-- https://themonadreader.files.wordpress.com/2010/01/issue15.pdf

--data StackInstruction a where
--    Pop     ::          StackInstruction Int
--    Push    :: Int ->   StackInstruction ()

data Program {- instr -} a where
    --Return  :: a ->     Program instr a
    --Then    :: instr a -> (a -> Program instr b) -> Program instr b
    Return  :: a ->     Program a
    Pop     :: (Int -> Program b) -> Program b
    Push    :: Int -> Program b -> Program b

interpret :: Program a -> ([Int] -> a)
interpret (Return c)                stack       = c
interpret (Pop      rest)   (x:stack)   = interpret (rest x)  stack
interpret (Push a   rest)   stack       = interpret rest (a : stack)

example =   Pop $ \a ->
            Pop $ \b ->
            Push (a+b) $
            Pop $ \x ->
            Return x

--singleton :: instr a -> Program instr a
--singleton i = i `Then` Return

--pop :: Program Int
pop = Pop Return

push :: Int -> Program ()
push x = Push x $ Return ()

--concatProg :: Program StackInstruction a -> (a -> Program StackInstruction b) -> Program StackInstruction b
--concatProg (Return a)          program = program a
--concatProg (instr `Then` rest) program = instr `Then` (\a -> concatProg (rest a) program)

concatProg :: Program a -> (a -> Program b) -> Program b
concatProg (Return a)          program = program a
--concatProg (instr `Then` rest) program = instr `Then` (\a -> concatProg (rest a) program)
concatProg (Pop rest) program = Pop $ (\a -> concatProg (rest a) program)
concatProg (Push a rest) program = Push a $ concatProg rest program

instance Functor (Program) where
    fmap f prog = concatProg prog $ Return . f

instance Applicative (Program) where
    pure = Return
    pf <*> pv = concatProg pf $ \f ->
                concatProg pv $ \v ->
                Return (f v)

instance Monad (Program) where
    return :: a -> Program a
    return  = Return
    (>>=) :: Program a -> (a -> Program b) -> Program b
    (>>=) = concatProg

example2 = do
    a <- pop
    b <- pop
    return (a * b)
