{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Session
    ( Session(..)
    , Sessions(..)
    , unSessions
    , getSessions
    , writeSessions
    , initalState
    , toInteger
    )
where

import Prelude hiding (toInteger)
import           Lib.Model.Data
import           Utils.ListZipper
import           Control.Lens

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )


toInteger :: Session -> Int
toInteger KindergartenSingle = 12
toInteger KindergartenGroup = 12
toInteger School = 11


data Session
    = KindergartenGroup
    | KindergartenSingle
    | School
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''Session


newtype Sessions = Sessions { _unSessions :: ListZipper Session }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''Sessions


getSessions
    :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> m Sessions
getSessions = readJSONFile

writeSessions
    :: (MonadIO m, MonadThrow m) => FilePath -> Sessions -> m ()
writeSessions = writeJSONFile

initalState :: Data String Sessions
initalState = NotAsked
