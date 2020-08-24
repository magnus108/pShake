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

newtype AppT msg m a = AppT
    { unAppT :: ReaderT (Env msg) m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader (Env msg)
               , MonadCatch
               , MonadThrow
               )

instance (MonadCatch m, MonadIO m) => MonadError Error (AppT msg m) where
    throwError :: Error -> AppT msg m a
    throwError = liftIO . throwIO . Exception
    {-# INLINE throwError #-}

    catchError :: AppT msg m a -> (Error -> AppT msg m a) -> AppT msg m a
    catchError action handler = AppT $ ReaderT $ \env -> do
        let ioAction = runAppT env action
        ioAction `catch` \e -> runAppT env $ handler $ unException e
    {-# INLINE catchError #-}


runAppAsIO :: Env msg -> AppT msg IO a -> IO (Either Error a)
runAppAsIO env = firstF unException . try . runAppT env


runAppT :: Env msg -> AppT msg m a -> m a
runAppT env = usingReaderT env . unAppT
