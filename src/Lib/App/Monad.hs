module Lib.App.Monad
    ( App
    , AppEnv
    , unApp
    , runApp
    , runAppAsIO
    )
where

import           Control.Exception              ( throwIO
                                                , try
                                                )
import           Control.Monad.Except           ( MonadError(..) )

import           Relude.Extra.Bifunctor         ( firstF )

import           Lib.App.Env                    ( Env )
import           Lib.App.Error                  ( AppError
                                                , AppException(..)
                                                )
import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                , catch
                                                )


type AppEnv = Env App

newtype App a = App
    { unApp :: ReaderT AppEnv IO a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader AppEnv
               , MonadCatch
               , MonadThrow
               )


instance MonadError AppError App where
    throwError :: AppError -> App a
    throwError = liftIO . throwIO . AppException
    {-# INLINE throwError #-}

    catchError :: App a -> (AppError -> App a) -> App a
    catchError action handler = App $ ReaderT $ \env -> do
        let ioAction = runApp env action
        ioAction `catch` \e -> runApp env $ handler $ unAppException e
    {-# INLINE catchError #-}


runAppAsIO :: AppEnv -> App a -> IO (Either AppError a)
runAppAsIO env = firstF unAppException . try . runApp env


runApp :: AppEnv -> App a -> IO a
runApp env = usingReaderT env . unApp
