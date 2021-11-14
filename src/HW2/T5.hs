module HW2.T5
  ( ExceptState (..),
    mapExceptState,
    wrapExceptState,
    joinExceptState,
    modifyExceptState,
    throwExceptState,
    eval,
  )
where

import Control.Monad (ap)
import HW2.T1 (Annotated (..), Except (..), mapAnnotated, mapExcept)
import HW2.T4 (Expr (..), Prim (..))

data ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f = g
  where
    g (ES run) = ES ((mapExcept . mapAnnotated) f . run)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState x = ES (Success . (x :#))

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES run) = ES (g . run)
  where
    g (Error e) = Error e
    g (Success ((ES x) :# s)) = x s

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (() :# f s))

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES (\_ -> Error e)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero deriving (Show)

op ::
  Expr ->
  Expr ->
  (Double -> Double -> Double) ->
  (Double -> Double -> Prim Double) ->
  ExceptState EvaluationError [Prim Double] Double
op x y f trace =
  do
    a <- eval x
    b <- eval y
    modifyExceptState (trace a b :)
    return (f a b)

opUnary ::
  Expr ->
  (Double -> Double) ->
  (Double -> Prim Double) ->
  ExceptState EvaluationError [Prim Double] Double
opUnary x f trace =
  do
    a <- eval x
    modifyExceptState (trace a :)
    return (f a)

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = return x
eval (Op (Add x y)) = op x y (+) Add
eval (Op (Sub x y)) = op x y (-) Sub
eval (Op (Mul x y)) = op x y (*) Mul
eval (Op (Abs x)) = opUnary x abs Abs
eval (Op (Sgn x)) = opUnary x signum Sgn
eval (Op (Div x y)) =
  do
    a <- eval x
    b <- eval y
    if b == 0
      then throwExceptState DivideByZero
      else do
        modifyExceptState (Div a b :)
        return (a / b)
