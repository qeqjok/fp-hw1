module HW2.T4
  ( State (..),
    Prim (..),
    Expr (..),
    mapState,
    wrapState,
    joinState,
    modifyState,
    eval,
  )
where

import Control.Monad (ap)
import HW2.T1 (Annotated (..), mapAnnotated)

data State s a = S {runS :: s -> Annotated s a}

mapState :: (a -> b) -> State s a -> State s b
mapState f = g
  where
    g (S run) = S (mapAnnotated f . run)

wrapState :: a -> State s a
wrapState x = S (x :#)

joinState :: State s (State s a) -> State s a
joinState (S run) = S (g . run)
  where
    g ((S x) :# s) = x s

modifyState :: (s -> s) -> State s ()
modifyState f = S (\s -> () :# f s)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a
  = Add a a -- (+)
  | Sub a a -- (-)
  | Mul a a -- (*)
  | Div a a -- (/)
  | Abs a -- abs
  | Sgn a -- signum
  deriving (Show)

data Expr = Val Double | Op (Prim Expr) deriving (Show)

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  x - y = Op (Sub x y)
  abs = Op . Abs
  signum = Op . Sgn

  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  fromRational = Val . fromRational
  x / y = Op (Div x y)

op ::
  Expr ->
  Expr ->
  (Double -> Double -> Double) ->
  (Double -> Double -> Prim Double) ->
  State [Prim Double] Double
op x y f trace =
  do
    a <- eval x
    b <- eval y
    modifyState (trace a b :)
    return (f a b)

opUnary ::
  Expr ->
  (Double -> Double) ->
  (Double -> Prim Double) ->
  State [Prim Double] Double
opUnary x f trace =
  do
    a <- eval x
    modifyState (trace a :)
    return (f a)

eval :: Expr -> State [Prim Double] Double
eval (Val x) = return x
eval (Op (Add x y)) = op x y (+) Add
eval (Op (Sub x y)) = op x y (-) Sub
eval (Op (Mul x y)) = op x y (*) Mul
eval (Op (Div x y)) = op x y (/) Div
eval (Op (Abs x)) = opUnary x abs Abs
eval (Op (Sgn x)) = opUnary x signum Sgn
