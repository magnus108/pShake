{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Photographer
    ( Photographer(..)
    , Photographers(..)
    , getPhotographers
    , writePhotographers
    , name
    , unPhotographers
    , tid
    , initalState
    )
where

import           Lib.Model.Data
import           Utils.ListZipper
import           Control.Lens

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )

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


newtype Photographers = Photographers { _unPhotographers :: ListZipper Photographer }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


makeLenses ''Photographers


getPhotographers
    :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> m Photographers
getPhotographers = readJSONFile

writePhotographers
    :: (MonadIO m, MonadThrow m) => FilePath -> Photographers -> m ()
writePhotographers = writeJSONFile

initalState :: Data String Photographers
initalState = NotAsked
