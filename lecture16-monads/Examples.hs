module Examples where

-- import System.Random
import Prelude hiding (Either (..), Maybe (..), map, mapM)

-- Defined as in Prelude
data Maybe a
  = Nothing
  | Just a
  deriving (Eq, Ord, Show, Read)

-- Defined as in Prelude
data Either a b
  = Left a
  | Right b
  deriving (Eq, Ord, Show, Read)

-- We can't hide the definition of the list data type, so we use an isomorphic
-- definition.
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show, Read)

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Eq, Ord, Show, Read)

map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

--
-- Functors
--
instance Functor List where
  -- fmap :: (a -> b) -> List a -> List b
  fmap = map

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f (Just x) = Just (f x)
  fmap _ Nothing = Nothing

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node left right) = Node (fmap f left) (fmap f right)

instance Functor (Either a) where
  -- fmap :: (b -> c) -> Either a b -> Either a c
  fmap _ (Left x) = Left x
  fmap f (Right y) = Right (f y)

--
-- Applicatives
--

instance Applicative Maybe where
  -- pure :: a -> Maybe a
  pure = Just

  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> _ = Nothing
  Just f <*> mx = fmap f mx

instance Applicative (Either a) where
  -- pure :: b -> Either a b
  pure = Right

  -- (<*>) :: Either a (b -> c) -> Either a b -> Either a c
  Left x <*> _ = Left x
  Right f <*> mx = fmap f mx

--
-- Abstracting over computation
--

--
-- Total interpreter
--
data Exp = Const Int | Div Exp Exp

eval :: Exp -> Int
eval (Const n) = n
eval (Div x y) = eval x `div` eval y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval' :: Exp -> Maybe Int
eval' (Const n) = Just n
eval' (Div x y) = case eval' x of
  Nothing -> Nothing
  Just n -> case eval' y of
    Nothing -> Nothing
    Just m -> safediv n m

--
-- Total interpreter in monadic style
--

instance Monad Maybe where
  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= _ = Nothing
  Just x >>= f = f x

eval'' :: Exp -> Maybe Int
eval'' (Const n) = Just n
eval'' (Div x y) = do
  n <- eval'' x
  m <- eval'' y
  safediv n m

--
-- Total interpreter using do notation
--

eval''' :: Exp -> Maybe Int
eval''' = eval''

--
-- State Monad
--

newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) = f

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)

  State mf <*> State mx = State $ \s ->
    let (f, s') = mf s
        (x, s'') = mx s'
     in (f x, s'')

instance Monad (State s) where
  State mx >>= f = State $ \s ->
    let (x, s') = mx s
        (State my) = f x
     in my s'

--
-- Generic functions
--

mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM _ [] = pure []
mapM f (x : xs) = do
  y <- f x
  ys <- mapM f xs
  pure (y : ys)

filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = pure []
filterM p (x : xs) = do
  keep <- p x
  ys <- filterM p xs
  pure (if keep then x : ys else ys)
