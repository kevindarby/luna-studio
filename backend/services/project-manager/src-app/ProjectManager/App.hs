{-# LANGUAGE GADTs #-}

module ProjectManager.App where

import Prologue

import qualified Control.Exception.Lifted as Exception
import qualified Control.Monad.State.Layered as State
import qualified Data.Aeson as Aeson
import qualified Snap.Core  as Snap

import Snap.Core (MonadSnap)

instance MonadSnap m => MonadSnap (State.StateT s m) where
    liftSnap = lift . Snap.liftSnap

type MonadApp m = (MonadIO m, MonadSnap m)

reply :: (MonadApp m, Aeson.ToJSON a) => a -> m ()
reply = Snap.writeLBS . Aeson.encode

handleHttpExceptions :: MonadApp m => m a -> m a
handleHttpExceptions action =
    Exception.catch action $ \(HttpException e) -> do
        let response = Snap.setResponseStatus
                (httpCode e)
                (convert $ displayException e)
                Snap.emptyResponse
        Snap.finishWith response

handleWith :: (MonadApp m, Aeson.ToJSON a) => m a -> m ()
handleWith action = handleHttpExceptions $ action >>= reply

throwHttp :: (MonadApp m, HandlerException e) => e -> m a
throwHttp = Exception.throwIO . HttpException

class Exception e => HandlerException e where
    httpCode :: e -> Int

data HttpException = forall e. HandlerException e => HttpException e

deriving instance Show HttpException

instance Exception HttpException where
    displayException (HttpException e) = displayException e

instance HandlerException HttpException where
    httpCode (HttpException e) = httpCode e
