module Simplified where

import Ansi (cyan, red, reset, yellow)
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Free
import Parser hiding (int, integer)
import Sum
import System.Exit

type Stack = [Int]

type Cont = Stack -> Stack

data CodeF a
  = PUSH Int a
  | ADD a
  deriving (Show)

instance Functor CodeF where
  fmap g (PUSH n a) = PUSH n (g a)
  fmap g (ADD a) = ADD (g a)

type Code = Free CodeF

push :: Int -> Code Cont
push n = liftF $ PUSH n id

add :: Code Cont
add = liftF $ ADD id

-- compiler

compile :: Expr -> Code Cont
compile (Val n) = push n
compile (Add n m) = do
  compile n
  compile m
  add

exec :: Code a -> Stack
exec c = exec' c []

exec' :: Code a -> Cont
exec' (Pure _) s = s
exec' (Free c) s = case c of
  PUSH n a -> exec' a (n : s)
  ADD a -> case s of
    (n : m : s) -> exec' a (n + m : s)
    _ -> error "ADD error"

run :: String -> Int
run = head . exec . compile . parse1

-- Algebra

foldFree :: (Functor f) => (a -> b) -> (f b -> b) -> Free f a -> b
foldFree pure free (Pure x) = pure x
foldFree pure free (Free t) = free (fmap (foldFree pure free) t)

alg :: CodeF Cont -> Cont
alg (PUSH n k) s = k (n : s)
alg (ADD k) (n : m : s) = k (n + m : s)

eval' :: Code Cont -> Cont
eval' = foldFree id alg

eval :: Code Cont -> Stack
eval c = eval' c []

run1 :: String -> Int
run1 = head . eval . compile . parse1

-- Typeclass

-- abstracting an algebra to a typeclass
class (Functor f) => StackEval f where
  runStack :: f Cont -> Cont

instance StackEval CodeF where
  runStack :: CodeF Cont -> Cont
  runStack (PUSH n k) s = k (n : s)
  runStack (ADD k) (n : m : s) = k (n + m : s)

instance (StackEval f, StackEval g) => StackEval (f :+: g) where
  runStack (InL r) = runStack r
  runStack (InR r) = runStack r

eval1' :: (StackEval f) => Free f Cont -> Cont
eval1' = foldFree id runStack

eval1 :: (StackEval f) => Free f Cont -> Stack
eval1 c = eval1' c []

run2 :: String -> Int
run2 = head . eval1 . compile . parse1

-- Shit

{--
alg :: CodeF a -> State Stack a
alg (PUSH n k) = do
  s <- get
  put (n : s)
  return k
alg (ADD k) = do
  s <- get
  case s of
    (n : m : s) -> do
      put (n + m : s)
      return k
    _ -> error "ADD error"

eval :: Code a -> Stack
eval c = execState (foldFree alg c) []

run1 :: String -> Stack
run1 = eval . compile . parse1

-- | The very definition of a free monad is that given a natural transformation
-- you get a monad homomorphism.
foldFree :: (Monad m) => (forall x. f x -> m x) -> Free f a -> m a
foldFree _ (Pure a) = return a
foldFree f (Free as) = f as >>= foldFree f

type Alg f a = f a -> a
--}

-- Parser

data Expr
  = Val Int -- value
  | Add Expr Expr -- addition
  | Sub Expr Expr -- subtraction
  | Mul Expr Expr -- multiplication
  | Div Expr Expr -- division
  | Pow Expr Expr -- exponentiation
  | Par Expr -- parenthesized expression
  deriving (Show)

-- parse an expression
expr :: Parser Expr
expr =
  do
    t <- term
    do
      symbol "+"
      Add t <$> expr
      <|> do
        symbol "-"
        Sub t <$> expr
      <|> do
        return t

-- parse a term
term :: Parser Expr
term =
  do
    p <- power
    do
      symbol "*"
      Mul p <$> term
      <|> do
        symbol "/"
        Div p <$> term
      <|> do
        return p

-- parse a power
power :: Parser Expr
power =
  do
    f <- factor
    do
      symbol "^"
      Pow f <$> power
      <|> return f

-- parse a factor
factor :: Parser Expr
factor =
  do
    symbol "("
    e <- expr
    symbol ")"
    return (Par e)
    <|> integer

-- parser for integers
int :: Parser Expr
int =
  do
    char '-'
    Val . negate <$> nat
    <|> Val <$> nat

-- parse integer
integer :: Parser Expr
integer = token int

-- parse syntax
parse1 :: String -> Expr
parse1 xs = case parse expr xs of
  [(e, [])] -> e
  [(_, out)] -> error ("Unused input " ++ out)
  [] -> error "invalid input"
