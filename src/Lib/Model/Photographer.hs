{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Photographer
    ( Photographer
    , Photographers
    , getPhotographers
    , name
    , tid
    )
where

import           Utils.ListZipper
import           Control.Lens

import           Control.Monad.Catch            ( MonadThrow )

type Name = String
type Tid = String


data Photographer = Photographer
    { _name :: Name
    , _tid :: Tid
    }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


makeLenses ''Photographer


newtype Photographers = Photographers { unPhotographers :: ListZipper Photographer }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


getPhotographers :: (MonadIO m, MonadThrow m) => FilePath -> m Photographers
getPhotographers = readJSONFile
