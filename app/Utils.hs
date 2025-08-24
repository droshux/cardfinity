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
