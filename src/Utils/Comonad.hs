module Utils.Comonad where

class Functor w => Comonad w where
    extract :: w a -> a

    duplicate :: w a -> w (w a)
    duplicate = extend id

    extend :: (w a -> b) -> w a -> w b
    extend f = fmap f . duplicate

(<<=) :: Comonad w => (w a -> b) -> w a -> w b
(<<=) = extend

(=>>) :: Comonad w => w a -> (w a -> b) -> w b
(=>>) = flip extend
