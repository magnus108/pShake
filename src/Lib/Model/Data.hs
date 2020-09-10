module Lib.Model.Data
    ( Data(..)
    , toJust
    )
where


data Data e s
    = NotAsked
    | Loading
    | Failure e
    | Data s
        deriving Show
        deriving Functor

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
