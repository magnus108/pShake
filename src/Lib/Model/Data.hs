{-# LANGUAGE DeriveFoldable #-}
module Lib.Model.Data
    ( Data(..)
    , toJust
    , data'
    )
where


data Data e s
    = NotAsked
    | Loading
    | Failure e
    | Data s
        deriving Show
        deriving Functor
        deriving Foldable

instance Bifunctor Data where
    bimap _ _ NotAsked = NotAsked
    bimap _ _ Loading = Loading
    bimap f _ (Failure e) = Failure (f e)
    bimap _ g (Data s) = Data (g s)

instance Monad (Data e) where
    return = Data
    (Data x)  >>= f = f x
    Failure e >>= _ = Failure e
    Loading   >>= _ = Loading
    NotAsked  >>= _ = NotAsked


instance Applicative (Data e) where
    pure = Data
    (Data    f) <*> (Data value) = Data (f value)
    (Failure e) <*> _            = Failure e
    _           <*> Failure e    = Failure e
    Loading     <*> _            = Loading
    _           <*> Loading      = Loading
    NotAsked    <*> _            = NotAsked
    _           <*> NotAsked     = NotAsked


toJust :: Data e s -> Maybe s
toJust (Data s) = Just s
toJust _        = Nothing


data' :: d -> d -> (e -> d) -> (s -> d) -> (Data e s) -> d
data' _ _ _ g (Data s) = g s
data' _ _ f _ (Failure e) = f e
data' _ d _ _ Loading = d
data' d _ _ _ NotAsked = d
