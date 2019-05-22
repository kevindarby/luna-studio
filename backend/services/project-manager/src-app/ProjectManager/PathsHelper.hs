module ProjectManager.PathsHelper where

import Prologue

import qualified Snap.Core as Snap
import qualified ProjectManager.Model.Project as Project
import qualified Data.UUID as UUID

import ProjectManager.App (MonadApp)
import Data.ByteString (ByteString)

(</>) :: ByteString -> ByteString -> ByteString
p </> q = p <> "/" <> q

currentHost :: MonadApp m => m ByteString
currentHost = do
    rq <- Snap.getRequest
    let prefix = if Snap.rqIsSecure rq then "https://" else "http://"
        host   = Snap.rqHostName rq
    pure $ prefix <> host

projectsDirectory :: ByteString
projectsDirectory = "projects"

projectsUri :: MonadApp m => m ByteString
projectsUri = (</> projectsDirectory) <$> currentHost

projectUri :: MonadApp m => Project.Id -> m ByteString
projectUri id = (</> UUID.toASCIIBytes id) <$> projectsUri

thumbDirectory :: ByteString
thumbDirectory = "thumb"

projectThumbUri :: MonadApp m => Project.Id -> m ByteString
projectThumbUri = fmap (</> thumbDirectory) . projectUri
