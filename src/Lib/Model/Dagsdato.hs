{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Dagsdato
    ( Dagsdato(..)
    , getDagsdato
    , writeDagsdato
    , unDagsdato
    , initalState
    )
where

import           Lib.Model.Data
import           Control.Lens

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )


newtype Dagsdato = Dagsdato { _unDagsdato :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''Dagsdato

getDagsdato :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> m Dagsdato
getDagsdato = readJSONFile

writeDagsdato :: (MonadIO m, MonadThrow m) => FilePath -> Dagsdato -> m ()
writeDagsdato = writeJSONFile

initalState :: Data String Dagsdato
initalState = NotAsked
