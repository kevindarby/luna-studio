module ProjectManager.Router where

import Prologue

import qualified ProjectManager.App                 as App
import qualified ProjectManager.Controller.Project as ProjectsController
import qualified Snap.Core                          as Snap

import ProjectManager.App (MonadApp)

serve :: MonadApp m => m ()
serve = Snap.dir "projects" $
    Snap.ifTop $ App.handleWith ProjectsController.index
