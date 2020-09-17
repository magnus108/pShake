{-# LANGUAGE DeriveAnyClass #-}
module Lib.App.Error
    ( AppError(..)
    , AppException(..)
    , WithError
    , IError(..)
    )
where

import           Control.Monad.Except           ( MonadError )
import qualified Control.Exception             as Exception

type WithError m = MonadError AppError m

newtype AppException = AppException
    { unAppException :: AppError
    } deriving (Show)
      deriving anyclass (Exception)

newtype AppError = InternalError IError
    deriving (Show, Eq)

data IError
    = WTF
    | ServerError String
    deriving (Show, Eq)
