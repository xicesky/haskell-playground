
-- | Encode deterministic pushdown automata as functions

module DPDA where

-- We want the stack to be our "evaluation stack" (even if the
-- language does not use one per se).

{-
  We will be using a simple parser as an example. Our language shall
    be formula with variables "a" and "b", binary operators "+" and "*" and
    matching parentheses, e.g. ((a + b) * (b + a))
    We use '$' as a symbol for the end of input.

  We will be using the following grammar (which is LL(1)):
    Expr   = Sum '$'
    Sum    = Prod ( '+''  Prod )*
    Prod   = Basic ( '*' Basic )*
    Basic  = 'a' | 'b' | '(' Sum ')'

  Let's just encode the PDA as functions directly, it's pretty easy
  to read:
-}

-- NOOB TYPES!!
type StackElem = String
type State = Int
type Input = String

pda1 :: StackElem -> (State, Input) -> (State, Input)

-- "Putting something on the stack" = Function call
-- "Consuming something from the stack" = Being called (to consume 1, just return)

pda1 "Expr"       (0,       xs) = (pda1 "$")          $ -- push "$"
                                  (pda1 "Sum")        $ -- push "Sum"
                                  (0, xs)               -- Don't alter state or input

pda1 "Sum"        (0,       xs) = (pda1 "(+Prod)*")   $
                                  (pda1 "Prod")       $
                                  (0, xs)

pda1 "Prod"       (0,       xs) = (pda1 "(*Basic)*")  $
                                  (pda1 "Basic")      $
                                  (0, xs)

pda1 "Basic"      (0, 'a' : xs) = (0, xs)               -- Pop stack element and input
pda1 "Basic"      (0, 'b' : xs) = (0, xs)
pda1 "Basic"      (0, '(' : xs) = (pda1 ")")          $
                                  (pda1 "Prod")       $
                                  (0, xs)

pda1 ")"          (0, ')' : xs) = (0, xs)

pda1 "(*Basic)*"  (0, '*' : xs) = (pda1 "Basic")      $
                                  (0, xs)
pda1 "(*Basic)*"  (0,       xs) = (0, xs)               -- No consume, just pop

pda1 "(+Prod)*"   (0, '+' : xs) = (pda1 "Basic")      $
                                  (0, xs)
pda1 "(+Prod)*"   (0,       xs) = (0, xs)               -- No consume, just pop

pda1 "$"          (0, '$' : xs) = (0, xs)

-- Error reporting
pda1 stacktop     (0,       []) = error $ "Unexpected end of input, we were expecting " ++ stacktop
pda1 stacktop     (0,   x : xs) = error $ "Unexpected '" ++ [x] ++ "', we were expecting " ++ stacktop ++ " at: " ++ (x:xs)
pda1 stacktop     (i,    input) = error $ "Something went horribly wrong (" ++ show i ++ ") expecting " ++ stacktop ++ " at: " ++ input

-- Shortcut
parse s = pda1 "Expr" (0, s ++ "$")


