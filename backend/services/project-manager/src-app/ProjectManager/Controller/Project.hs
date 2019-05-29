module ProjectManager.Controller.Project where

import Prologue

import qualified ProjectManager.Api.Project as ApiProject
import qualified ProjectManager.Api.File as ApiFile
import qualified ProjectManager.App         as App
import qualified ProjectManager.Repository.Project as Repo
import qualified ProjectManager.Model.Project as Project
import qualified Snap.Core as Snap

import ProjectManager.App (MonadApp)
import ProjectManager.Repository.Project (MonadRepo)
import qualified Path
import qualified Data.Map as Map
import qualified ProjectManager.PathsHelper as Paths

import Path (Path, Abs, Dir, (</>))

import qualified Path.IO as PathIO

toApiItem :: MonadApp m => Project.Id -> Project.Project -> m ApiProject.Item
toApiItem id project = do
    thumb <- if project ^. Project.hasThumb then Just <$> Paths.projectThumbUri id else pure Nothing
    pure $ ApiProject.Item id (project ^. Project.name) (project ^. Project.path) (convert thumb) (project ^. Project.lastOpen)

index :: (MonadApp m, MonadRepo m) => m [ApiProject.Item]
index = do
    projs <- Map.toList <$> Repo.getProjects
    for projs $ uncurry toApiItem

get :: (MonadApp m, MonadRepo m) => Project.Id -> m ApiProject.Item
get id = do
    proj <- Repo.getProject id
    toApiItem id proj

getThumb :: (MonadApp m, MonadRepo m) => Project.Id -> m ()
getThumb id = do
    proj <- Repo.getProject id
    let p = (proj ^. Project.path) </> $(Path.mkRelFile "thumb.png")
    Snap.sendFile $ Path.toFilePath p

buildTree :: MonadIO m => Path Abs Dir -> m ApiFile.File
buildTree path = do
    (dirs, files) <- PathIO.listDir path
    subDirs <- traverse buildTree dirs
    let subFiles = (\fPath -> ApiFile.File (convert $ Path.toFilePath $ Path.filename fPath) (convert $ Path.fileExtension fPath) []) <$> files
    let rootNode = ApiFile.File (convert $ Path.toFilePath $ Path.dirname path) "directory" (subDirs <> subFiles)
    pure rootNode


getTree :: (MonadApp m, MonadRepo m) => Project.Id -> m ApiFile.File
getTree id = do
    project <- Repo.getProject id
    let path = project ^. Project.path
    buildTree path

{-pure [-}
    {-ApiProject.Item "HelloProject" (Just "Dummy project") (Just 98765432) Nothing "/projects/HelloProject/open",-}
    {-ApiProject.Item "LelProject" (Just "Another project") (Just 908765432) Nothing "/projects/LelProject/open"-}
    {-]-}
