{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Session
    ( Session(..)
    , Sessions(..)
    , getSessions
    , writeSessions
    , initalState
    )
where

import           Lib.Model.Data
import           Utils.ListZipper
import           Control.Lens

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )

data Session
    = KindergartenGroup
    | KindergartenSingle
    | School
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''Session


newtype Sessions = Sessions { unSessions :: ListZipper Session }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


getSessions
    :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> m Sessions
getSessions = readJSONFile

writeSessions
    :: (MonadIO m, MonadThrow m) => FilePath -> Sessions -> m ()
writeSessions = writeJSONFile

initalState :: Data String Sessions
initalState = NotAsked
