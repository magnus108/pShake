{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.ListZipper
    ( ListZipper(..)
    , zipperL
    , fromList
    , toN
    , findFirst
    , find
    , toNonEmpty
    , appendr
    , focus
    , isRight
    , isLeft
    , rights
    , lefts
    , mapFocus
    , backward
    , forward
    , toList
    , iextend
    , sorted
    , insert
    ) where


import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , over
                                                , (%~)
                                                , lens
                                                , view
                                                , Lens'
                                                )
import qualified Control.Lens                  as Lens

import Prelude ((-))
import Data.Semigroup
import Data.Function ((.), ($))
import Data.List (reverse, (++), length)
import Data.Functor
import Data.Ord
import Data.Eq
import Data.Foldable (Foldable, foldMap, foldr)
import Data.Traversable
import Control.Applicative
import Text.Show
import Data.Maybe
import Data.Int
import GHC.Generics
import Data.List.NonEmpty (NonEmpty(..), (<|), tail, cons)
import Data.Aeson
import Data.Bool

import Utils.Comonad

data ListZipper a = ListZipper [a] a [a]
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


first :: ListZipper a -> ListZipper a
first (ListZipper [] a rs) = ListZipper [] a rs
first (ListZipper ls a rs) = ListZipper [] y (ys ++ (a : rs))
    where
        y:ys = reverse ls

toN :: ListZipper a -> Int -> ListZipper a
toN xs n = toN' zipper n
    where
        zipper =  first xs
        toN' xs' 0 = xs'
        toN' xs' n' = toN' (forward xs') (n'-1)

isRight :: ListZipper a -> Bool
isRight (ListZipper _ _ []) = True
isRight _ = False

isLeft :: ListZipper a -> Bool
isLeft (ListZipper [] _ _) = True
isLeft _ = False

insert :: ListZipper a -> a -> ListZipper a
insert (ListZipper ls a rs) b = ListZipper (a:ls) b rs


backward :: ListZipper a -> ListZipper a
backward a = fromMaybe a (backward' a)


backward' :: ListZipper a -> Maybe (ListZipper a)
backward' (ListZipper (l:ls) a rs) = Just (ListZipper ls l (a:rs))
backward' (ListZipper [] _ _) = Nothing


forward :: ListZipper a -> ListZipper a
forward a = fromMaybe a (forward' a)


forward' :: ListZipper a -> Maybe (ListZipper a)
forward' (ListZipper ls a (r:rs)) = Just (ListZipper (a:ls) r rs)
forward' (ListZipper _ _ []) = Nothing


lefts :: ListZipper a -> [a]
lefts (ListZipper ls _ _) = reverse ls


rights :: ListZipper a -> [a]
rights (ListZipper _ _ rs) = rs


focus :: ListZipper a -> a
focus zipper = extract zipper


mapFocus :: (a -> a) -> ListZipper a -> ListZipper a
mapFocus f (ListZipper ls a xs) = ListZipper ls (f a) xs


iterate' :: (a -> Maybe a) -> a -> NonEmpty a
iterate' f x = 
    case f x of
            Just x' -> x <| (iterate' f x')
            Nothing -> x :| []


toList :: ListZipper a -> [a]
toList (ListZipper ls x rs) = (reverse ls) ++ (x : rs)


-- > appendr [1,2,3] (4 :| [5]) == 1 :| [2,3,4,5]
appendr :: [a] -> NonEmpty a -> NonEmpty a
appendr l nel = foldr cons nel l

toNonEmpty :: ListZipper a -> NonEmpty a
toNonEmpty (ListZipper ls x rs) = appendr (reverse ls) (x :| rs)


iextend :: (Int -> ListZipper a -> b) -> ListZipper a -> ListZipper b 
iextend f = fmap (\xs@(ListZipper ls _ _) -> f (length ls) xs) . duplicate

--move me
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys) | x < y     = x:y:ys
                 | otherwise = y:(insert' x ys)

insert'' :: Ord a => a -> [a] -> [a]
insert'' x [] = [x]
insert'' x (y:ys) | x > y     = x:y:ys
                  | otherwise = y:(insert'' x ys)


sorted :: Ord a => [a] -> ListZipper a -> ListZipper a
sorted [] z = z
sorted (y:ys) (ListZipper ls a rs) | y < a = sorted ys $ ListZipper (insert'' y ls) a rs
                                   | otherwise = sorted ys $ ListZipper ls a (insert' y rs) 


fromList :: [a] -> Maybe (ListZipper a)
fromList [] = Nothing
fromList (x:xs) = Just $ ListZipper [] x xs


findFirst :: (a -> Bool) -> ListZipper a -> Maybe (ListZipper a)
findFirst predicate = find predicate . first


find :: (a -> Bool) -> ListZipper a -> Maybe (ListZipper a)
find predicate zipper@(ListZipper _ x _) =
    if predicate x then
        Just zipper
    else
        case forward' zipper of
            Just nextZipper ->
                find predicate nextZipper
            Nothing ->
                Nothing


instance Functor ListZipper where
    fmap f (ListZipper ls a rs) = ListZipper (fmap f ls) (f a) (fmap f rs)


instance Comonad ListZipper where
    extract (ListZipper _ a _) = a
    duplicate a = ListZipper (shift backward') a (shift forward')
        where shift move = tail $ iterate' move a

instance Foldable ListZipper where
    foldMap f (ListZipper l x r) =
        foldMap f (reverse l) <> f x <> foldMap f r

instance Traversable ListZipper where
    traverse f (ListZipper l x r) =
        ListZipper <$> traverse f l <*> f x <*> traverse f r



zipperL :: Lens' (ListZipper a) a
zipperL = lens getter setter
  where
    getter :: ListZipper a -> a
    getter = extract

    setter :: (ListZipper a) -> a -> (ListZipper a)
    setter (ListZipper ls x rs) new =
        ListZipper ls new rs
