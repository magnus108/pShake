{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Grade
    ( Photographee(..)
    , Photographees(..)
    , Grade(..)
    , Grades(..)
    , unPhotographees
    , photographees
    , gradeId
    , getGrades
    , writeGrades
    , unGrades
    , sys
    , name
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

type GradeId = String
type Name = String
type Tid = String
type Sys = String

data Photographee = Photographee
    { _name :: Name
    , _tid :: Tid
    , _sys :: Sys
    }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


makeLenses ''Photographee


newtype Photographees = Photographees { _unPhotographees :: ListZipper Photographee }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''Photographees

data Grade = Grade
    { _gradeId :: GradeId
    , _photographees :: Photographees
    }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


makeLenses ''Grade

newtype Grades = Grades { _unGrades :: ListZipper Grade }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''Grades


getGrades :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> m Grades
getGrades = readJSONFile

writeGrades :: (MonadIO m, MonadThrow m) => FilePath -> Grades -> m ()
writeGrades = writeJSONFile

initalState :: Data String Grades
initalState = NotAsked
