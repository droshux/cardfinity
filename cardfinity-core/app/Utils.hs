{-# LANGUAGE LambdaCase #-}

module Utils where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either (isRight)
import Data.Foldable (Foldable (toList))
import Data.Functor ((<&>))
import Data.Map (assocs, fromListWith)
import GHC.Natural (Natural, naturalToInteger)
import GHC.Num (integerToInt)

natToInt :: Natural -> Int
natToInt = integerToInt . naturalToInteger

without :: [a] -> Int -> [a]
xs `without` i = take i xs ++ drop (i + 1) xs

delimFoldMap :: (Foldable t, Monoid m) => (a -> m) -> m -> t a -> m
delimFoldMap fun delim = helper fun delim . Data.Foldable.toList
  where
    helper _ _ [] = mempty
    helper f _ [x] = f x
    helper f d (x : xs) = f x <> d <> helper f d xs

showFold :: (Show a, Foldable t) => String -> t a -> String
showFold = delimFoldMap show

whenJust :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
whenJust _ Nothing = return ()
whenJust op (Just x) = op x

try :: (Monad m) => ExceptT () m () -> m Bool
try = (<&> isRight) . runExceptT

ifEmpty :: (Monad m) => m [a] -> m (a, [a]) -> m (a, [a])
ifEmpty m a =
  m >>= \case
    [] -> a
    (x : xs) -> return (x, xs)

ifNone :: (Monad m) => m (Maybe a) -> m a -> m a
ifNone m a =
  m >>= \case
    Nothing -> a
    Just x -> return x

addInteger :: Integer -> Natural -> Natural
addInteger y x
  | y < 0 && x < fromIntegral (abs y) = 0
  | y < 0 = x - fromIntegral y
  | otherwise = x + fromIntegral y

collapse :: (Ord a) => [a] -> [(a, Int)]
collapse = assocs . fromListWith (+) . map (,1)
