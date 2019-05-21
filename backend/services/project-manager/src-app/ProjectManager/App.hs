module ProjectManager.App where

import Prologue

import qualified Data.Aeson as Aeson
import qualified Snap.Core  as Snap

import Snap.Core (MonadSnap)

type MonadApp m = (MonadIO m, MonadSnap m)

reply :: (MonadApp m, Aeson.ToJSON a) => a -> m ()
reply = Snap.writeLBS . Aeson.encode

handleWith :: (MonadApp m, Aeson.ToJSON a) => m a -> m ()
handleWith = (>>= reply)

