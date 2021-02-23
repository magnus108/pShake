{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Translation
    ( Translations(..)
    , getTranslations
    , writeTranslations
    , initalState
    , unTranslation
    )
where

import           Control.Lens

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )

import qualified Data.HashMap.Strict           as HashMap


data Translations = Translations { _unTranslation :: HashMap String String }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''Translations

getTranslations
    :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> m Translations
getTranslations = readJSONFile

writeTranslations
    :: (MonadIO m, MonadThrow m) => FilePath -> Translations -> m ()
writeTranslations = writeJSONFile

initalState :: Translations
initalState = Translations HashMap.empty
