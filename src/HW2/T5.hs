module HW2.T5
  ( -- * The `ExceptState` type
    ExceptState (..),

    -- * The `EvaluationError` type
    EvaluationError (..),

    -- * Functions for `State`
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

-- | State data type with Error.
data ExceptState e s a = ES
  { -- ^ Run evaluation in the `ExceptState`.
    runES :: s -> Except e (Annotated s a)
  }

-- | fmap for `ExceptState`.
mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f = g
  where
    g (ES run) = ES ((mapExcept . mapAnnotated) f . run)

-- | Wrap value into `ExceptState`.
wrapExceptState :: a -> ExceptState e s a
wrapExceptState x = ES (Success . (x :#))

-- | Join for `ExceptState`.
joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES run) = ES (g . run)
  where
    g (Error e)               = Error e
    g (Success ((ES x) :# s)) = x s

-- | Modifying current state.
modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (() :# f s))

-- | Fail current state.
throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES (\_ -> Error e)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero

-- | Create a binary operation in the context of `ExceptState`.
op ::
  Expr -> -- ^ Left expression
  Expr -> -- ^ Right expression
  (Double -> Double -> Double) -> -- ^ Binary operation
  (Double -> Double -> Prim Double) -> -- ^ Binary log operation
  ExceptState EvaluationError [Prim Double] Double -- ^ Resulting `ExceptState`
op x y f trace =
  do
    a <- eval x
    b <- eval y
    modifyExceptState (trace a b :)
    return (f a b)

-- | Create a unary operation in the context of `ExceptState`.
opUnary ::
  Expr -> -- ^ Expression
  (Double -> Double) -> -- ^ Unary operation
  (Double -> Prim Double) -> -- ^ Unary log operation
  ExceptState EvaluationError [Prim Double] Double -- ^ Resulting `ExceptState`
opUnary x f trace =
  do
    a <- eval x
    modifyExceptState (trace a :)
    return (f a)

-- | Creates a state describing the calculation based on the `Expr`.
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
