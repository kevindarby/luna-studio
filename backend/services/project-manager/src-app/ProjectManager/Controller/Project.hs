module ProjectManager.Controller.Project where

import Prologue

import qualified ProjectManager.Api.Project as ApiProject
import qualified ProjectManager.App         as App
import qualified ProjectManager.Service.Project as ProjectsService
import qualified ProjectManager.Model.Project as Project

import ProjectManager.App (MonadApp)
import qualified Path

index :: MonadApp m => m [ApiProject.Item]
index = do
    projs <- ProjectsService.listProjectsInDirectory $ unsafeFromJust $ Path.parseAbsDir "/Users/marcinkostrzewa/luna/projects/"
    for projs $ \proj -> do
        pure $ ApiProject.Item (proj ^. Project.name) Nothing Nothing Nothing ""

{-pure [-}
    {-ApiProject.Item "HelloProject" (Just "Dummy project") (Just 98765432) Nothing "/projects/HelloProject/open",-}
    {-ApiProject.Item "LelProject" (Just "Another project") (Just 908765432) Nothing "/projects/LelProject/open"-}
    {-]-}
