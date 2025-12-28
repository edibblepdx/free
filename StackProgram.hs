module StackProgram where

import Ansi (cyan, red, reset, yellow)
import Control.Applicative
import Control.Monad
import Free
import Parser hiding (int, integer)
import Sum
import System.Exit

type Stack = [Int]

data CodeF a
  = PUSH Int a
  | ADD a
  | SUB a
  | MUL a
  deriving (Show)

instance Functor CodeF where
  fmap g (PUSH n a) = PUSH n (g a)
  fmap g (ADD a) = ADD (g a)
  fmap g (SUB a) = SUB (g a)
  fmap g (MUL a) = MUL (g a)

type Code = Free CodeF

push :: Int -> Code ()
push n = liftF $ PUSH n ()

add :: Code ()
add = liftF $ ADD ()

sub :: Code ()
sub = liftF $ ADD ()

mul :: Code ()
mul = liftF $ ADD ()

-- compiler

compile :: Expr -> Code ()
compile (Val n) = push n
compile (Add n m) = do
  compile n
  compile m
  add
compile (Sub n m) = do
  compile n
  compile m
  sub
compile (Mul n m) = do
  compile n
  compile m
  mul

exec :: Code a -> Stack
exec c = exec' c []

exec' :: Code a -> Stack -> Stack
exec' (Pure _) s = s
exec' (Free c) s = case c of
  PUSH n a -> exec' a (n : s)
  ADD a -> case s of
    (n : m : s) -> exec' a (n + m : s)
    _ -> error "ADD error"
  SUB a -> case s of
    (n : m : s) -> exec' a (n - m : s)
    _ -> error "SUB error"
  MUL a -> case s of
    (n : m : s) -> exec' a (n * m : s)
    _ -> error "MUL error"

{-
step :: Code a -> IO ()
step (Pure _) s = putStrLn $ show s
step (Free c) s = print c
-}

showc :: Code a -> IO ()
showc c = showC c []

showC :: Code a -> Stack -> IO ()
showC (Pure _) _ = return ()
showC (Free c) s = case c of
  PUSH n a -> showC a (n : s)
  ADD a -> case s of
    (n : m : s) -> do
      print $ show n ++ "+" ++ show m ++ "=" ++ show r
      showC a (r : s)
      where
        r = n + m
    _ -> error "ADD error"
  SUB a -> case s of
    (n : m : s) -> do
      print $ show n ++ "-" ++ show m ++ "=" ++ show r
      showC a (r : s)
      where
        r = n * m
    _ -> error "SUB error"
  MUL a -> case s of
    (n : m : s) -> do
      print $ show n ++ "*" ++ show m ++ "=" ++ show r
      showC a (r : s)
      where
        r = n * m
    _ -> error "MUL error"

run :: String -> Int
run = head . exec . compile . parse1

runp :: String -> IO ()
runp = showc . compile . parse1

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
