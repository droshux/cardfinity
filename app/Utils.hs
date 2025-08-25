{-# LANGUAGE LambdaCase #-}

module Utils where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either (isRight)
import Data.Foldable (Foldable (toList))
import Data.Functor ((<&>))
import GHC.Natural (Natural, naturalToInteger)
import GHC.Num (integerToInt)

natToInt :: Natural -> Int
natToInt = integerToInt . naturalToInteger

without :: [a] -> Int -> [a]
xs `without` i = take i xs ++ drop (i + 1) xs

showFold :: (Show a, Foldable t) => String -> t a -> String
showFold connector as = helper connector $ toList as
  where
    helper _ [] = ""
    helper c (x : xs) = foldr (\x' a -> a ++ c ++ show x') (show x) $ reverse xs

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
