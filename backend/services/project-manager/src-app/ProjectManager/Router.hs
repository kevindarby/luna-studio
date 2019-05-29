module ProjectManager.Router where

import Prologue

import qualified ProjectManager.App                 as App
import qualified ProjectManager.Controller.Project as ProjectsController
import qualified ProjectManager.Repository.Project as ProjectsRepo
import qualified Snap.Core                          as Snap

import ProjectManager.Repository.Project (MonadRepo)

import ProjectManager.App (MonadApp)

serve :: (MonadApp m, MonadRepo m) => m ()
serve = Snap.dir "projects" $
    Snap.ifTop (App.handleWith ProjectsController.index) <|>
    Snap.route [(":id", serveProject)]

serveProject :: (MonadApp m, MonadRepo m) => m ()
serveProject = App.handleHttpExceptions $ do
    maybeId <- Snap.getParam "id"
    id <- maybe (App.throwHttp ProjectsRepo.InvalidId) pure
        (ProjectsRepo.parseId =<< maybeId)
    Snap.route [("", App.handleWith $ ProjectsController.get id),
                ("thumb", ProjectsController.getThumb id),
                ("tree", App.handleWith $ ProjectsController.getTree id)]
