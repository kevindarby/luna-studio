module ProjectManager.Controller.Project where

import Prologue

import qualified ProjectManager.Api.Project as ApiProject
import qualified ProjectManager.App         as App
import qualified ProjectManager.Repository.Project as Repo
import qualified ProjectManager.Model.Project as Project

import ProjectManager.App (MonadApp)
import qualified Path
import qualified Data.Map as Map

index :: MonadApp m => m [ApiProject.Item]
index = do
    projs <- Repo.run $ Map.elems <$> Repo.getProjects
    for projs $ \proj -> do
        pure $ ApiProject.Item (proj ^. Project.name) (proj ^. Project.path) Nothing Nothing (proj ^. Project.lastOpen) ""

{-pure [-}
    {-ApiProject.Item "HelloProject" (Just "Dummy project") (Just 98765432) Nothing "/projects/HelloProject/open",-}
    {-ApiProject.Item "LelProject" (Just "Another project") (Just 908765432) Nothing "/projects/LelProject/open"-}
    {-]-}
