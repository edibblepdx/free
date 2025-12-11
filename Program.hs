module Program where

import Ansi (cyan, red, reset, yellow)
import Control.Applicative
import Control.Monad
import Free
import Parser hiding (int, integer)
import Sum
import System.Exit

{-
language grammar

expr ::= term (+ expr | - expr | Є)
term ::= power (* term | / term | Є)
power ::= factor (^ power | Є)
factor ::= ( expr ) | int
int ::= ... | -1 | 0 | 1 | ...
-}

-- Logging language
-- =============================================================================

{-# NOINLINE fatal #-}

{-# RULES "exit" forall s m. fatal s >> m = fatal s #-}

data LogF a b
  = Debug a b
  | Info String b
  | Fatal String

instance Functor (LogF a) where
  fmap g (Debug a b) = Debug a (g b)
  fmap g (Info s b) = Info s (g b)
  fmap g (Fatal s) = Fatal s

type Log b = Free (LogF b)

debug :: Int -> Log Int ()
debug a = liftF $ Debug a ()

info :: String -> Log Int ()
info s = liftF $ Info s ()

fatal :: String -> Log Int ()
fatal s = liftF $ Fatal s

-- Arithmetic expressions language
-- =============================================================================
data ExprF a
  = Val1 Int (Int -> a)
  | Add1 Int Int (Int -> a)
  | Sub1 Int Int (Int -> a)
  | Mul1 Int Int (Int -> a)
  | Div1 Int Int (Int -> a)
  | Pow1 Int Int (Int -> a)

instance Functor ExprF where
  fmap g (Val1 n k) = Val1 n (g . k)
  fmap g (Add1 n m k) = Add1 n m (g . k)
  fmap g (Sub1 n m k) = Sub1 n m (g . k)
  fmap g (Mul1 n m k) = Mul1 n m (g . k)
  fmap g (Div1 n m k) = Div1 n m (g . k)
  fmap g (Pow1 n m k) = Pow1 n m (g . k)

type Expr = Free ExprF

val :: Int -> Expr Int
val n = liftF $ Val1 n id

add :: Int -> Int -> Expr Int
add n m = liftF $ Add1 n m id

sub :: Int -> Int -> Expr Int
sub n m = liftF $ Sub1 n m id

mul :: Int -> Int -> Expr Int
mul n m = liftF $ Mul1 n m id

div1 :: Int -> Int -> Expr Int
div1 n m = liftF $ Div1 n m id

pow :: Int -> Int -> Expr Int
pow n m = liftF $ Pow1 n m id

-- Combined language
type Program = Free (LogF Int :+: ExprF)

-- Abstract syntax tree
-- =============================================================================
data Tree
  = Val Int -- value
  | Add Tree Tree -- addition
  | Sub Tree Tree -- subtraction
  | Mul Tree Tree -- multiplication
  | Div Tree Tree -- division
  | Pow Tree Tree -- exponentiation
  | Par Tree -- parenthesized expression
  deriving (Show)

-- parse an expression
expr :: Parser Tree
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
term :: Parser Tree
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
power :: Parser Tree
power =
  do
    f <- factor
    do
      symbol "^"
      Pow f <$> power
      <|> return f

-- parse a factor
factor :: Parser Tree
factor =
  do
    symbol "("
    e <- expr
    symbol ")"
    return (Par e)
    <|> integer

-- parser for integers
int :: Parser Tree
int =
  do
    char '-'
    Val . negate <$> nat
    <|> Val <$> nat

-- parse integer
integer :: Parser Tree
integer = token int

-- parse syntax
parse1 :: String -> Tree
parse1 xs = case parse expr xs of
  [(e, [])] -> e
  [(_, out)] -> error ("Unused input " ++ out)
  [] -> error "invalid input"

-- compile an abstract syntax tree into a free monad
-- =============================================================================

data CompileFlags = CompileFlags
  { logDebug :: Bool,
    logInfo :: Bool,
    logFatal :: Bool
  }

-- compile program
compile :: CompileFlags -> Tree -> Program Int
compile o (Val n) = do
  inj $ val n
compile o (Add e1 e2) = do
  x <- compile o e1
  y <- compile o e2
  when (logInfo o) $
    inj (info $ "Add " ++ show x ++ " " ++ show y)
  when (logDebug o) $
    inj (debug $ x + y)
  inj $ x `add` y
compile o (Sub e1 e2) = do
  x <- compile o e1
  y <- compile o e2
  when (logInfo o) $
    inj (info $ "Sub " ++ show x ++ " " ++ show y)
  when (logDebug o) $
    inj (debug $ x - y)
  inj $ x `sub` y
compile o (Mul e1 e2) = do
  x <- compile o e1
  y <- compile o e2
  when (logInfo o) $
    inj (info $ "Mul " ++ show x ++ " " ++ show y)
  when (logDebug o) $
    inj (debug $ x * y)
  inj $ x `mul` y
compile o (Div e1 e2) = do
  x <- compile o e1
  y <- compile o e2
  when (logInfo o) $
    inj (info $ "Div " ++ show x ++ " " ++ show y)
  when (y == 0 && logFatal o) $
    inj (fatal "divide by zero")
  when (y /= 0 && logDebug o) $
    inj (debug $ x `div` y)
  inj $ x `div1` y
compile o (Pow e1 e2) = do
  x <- compile o e1
  y <- compile o e2
  when (logInfo o) $
    inj (info $ "Pow " ++ show x ++ " " ++ show y)
  when (logDebug o) $
    inj (debug $ x ^ y)
  inj $ x `pow` y
compile o (Par e) = do
  compile o e

-- decompile program
-- Still need to figure this out
decompile :: Program Int -> Tree
decompile (Pure n) = Val n
decompile (Free (InR x)) = case x of
  Val1 n k -> decompile (k n)
  Add1 n m k -> Add (decompile (k n)) (Val m)
  Sub1 n m k -> Sub (decompile (k n)) (Val m)
  Mul1 n m k -> Mul (decompile (k n)) (Val m)
  Div1 n m k -> Div (decompile (k n)) (Val m)
  Pow1 n m k -> Pow (decompile (k n)) (Val m)

-- interpreter that evaluates the program as an expression
eval :: Program Int -> IO Int
eval (Pure n) = return n
eval (Free (InL x)) = case x of
  Debug a k -> putStrLn (yellow ++ "DEBUG: " ++ show a ++ reset) >> eval k
  Info s k -> putStrLn (cyan ++ "INFO: " ++ s ++ reset) >> eval k
  Fatal s -> putStrLn (red ++ "FATAL: " ++ s ++ reset) >> exitFailure
eval (Free (InR x)) = case x of
  Val1 n k -> eval . k $ n
  Add1 n m k -> eval . k $ n + m
  Sub1 n m k -> eval . k $ n - m
  Mul1 n m k -> eval . k $ n * m
  Div1 n m k -> eval . k $ n `div` m
  Pow1 n m k -> eval . k $ n ^ m

-- interpreter that pretty prints the program
-- The program is sequential so pretty printing is still not figure out yet
-- Probably need to decompile the program back into an abstract syntax tree
showProgram :: Program Int -> String
showProgram (Pure n) = show n
showProgram (Free (InR x)) = case x of
  (Val1 n k) -> showProgram . k $ n
  (Add1 n m k) ->
    let g = showProgram . k
     in wrap $ g n ++ "+" ++ show m
  (Sub1 n m k) ->
    let g = showProgram . k
     in wrap $ g n ++ "-" ++ show m
  (Mul1 n m k) ->
    let g = showProgram . k
     in wrap $ g n ++ "*" ++ show m

{-
  Val1 n k -> showProgram . k $ n
  Add1 n m k -> showProgram (k n) ++ "+" ++ show m
  Sub1 n m k -> showProgram (k n) ++ "-" ++ show m
  Mul1 n m k -> showProgram (k n) ++ "*" ++ show m
  Div1 n m k -> showProgram (k n) ++ "/" ++ show m
  Pow1 n m k -> showProgram (k n) ++ "^" ++ show m
-}

-- interpreter that steps through the program
stepProgram :: Program Int -> String
stepProgram (Pure n) = show n
stepProgram (Free (InL x)) = case x of
  Debug a k -> stepProgram k
  Info s k -> stepProgram k
  Fatal s -> ""
stepProgram (Free (InR x)) = case x of
  Val1 n k -> stepProgram . k $ n
  Add1 n m k ->
    show n ++ "+" ++ show m ++ "=" ++ show r ++ "\n" ++ stepProgram (k r)
    where
      r = n + m
  Sub1 n m k ->
    show n ++ "-" ++ show m ++ "=" ++ show r ++ "\n" ++ stepProgram (k r)
    where
      r = n - m
  Mul1 n m k ->
    show n ++ "*" ++ show m ++ "=" ++ show r ++ "\n" ++ stepProgram (k r)
    where
      r = n * m
  Div1 n m k ->
    show n ++ "/" ++ show m ++ "=" ++ show r ++ "\n" ++ stepProgram (k r)
    where
      r = n `div` m
  Pow1 n m k ->
    show n ++ "^" ++ show m ++ "=" ++ show r ++ "\n" ++ stepProgram (k r)
    where
      r = n ^ m

wrap :: String -> String
wrap s = "(" ++ s ++ ")"

-- default compile flags
dflags :: CompileFlags
dflags = CompileFlags True True True

run :: CompileFlags -> String -> IO Int
run o = eval . compile o . parse1

pretty :: String -> IO ()
pretty = putStrLn . showProgram . compile dflags . parse1

step :: String -> IO ()
step = putStrLn . stepProgram . compile dflags . parse1

decomp :: String -> Tree
decomp = decompile . compile dflags . parse1
