module Lib.App.Monad
    ( AppT
    , unAppT
    , runAppT
    , runAppAsIO
    )
where

import           Control.Exception              ( throwIO
                                                , try
                                                )
import           Control.Monad.Except           ( MonadError(..) )

import           Relude.Extra.Bifunctor         ( firstF )

import           Lib.App.Env                    ( Env )
import           Lib.App.Error                  ( Error
                                                , Exception(..)
                                                )
import           Control.Monad.Catch            ( MonadThrow, MonadCatch, catch )


type AppEnv m msg = Env (AppT m msg) msg

newtype AppT m msg a = AppT
    { unAppT :: ReaderT (AppEnv m msg) m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader (AppEnv m msg)
               , MonadCatch
               , MonadThrow
               )

instance (MonadCatch m, MonadIO m) => MonadError Error (AppT m msg) where
    throwError :: Error -> AppT m msg a
    throwError = liftIO . throwIO . Exception
    {-# INLINE throwError #-}

    catchError :: AppT m msg a -> (Error -> AppT m msg a) -> AppT m msg a
    catchError action handler = AppT $ ReaderT $ \env -> do
        let ioAction = runAppT env action
        ioAction `catch` \e -> runAppT env $ handler $ unException e
    {-# INLINE catchError #-}


runAppAsIO :: AppEnv IO msg -> AppT IO msg a -> IO (Either Error a)
runAppAsIO env = firstF unException . try . runAppT env


runAppT :: AppEnv m msg -> AppT m msg a -> m a
runAppT env = usingReaderT env . unAppT
