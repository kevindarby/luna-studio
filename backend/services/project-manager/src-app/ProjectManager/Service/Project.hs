module ProjectManager.Service.Project where

import Prologue

import qualified Luna.Package as Package
import qualified Luna.Package.Configuration.Local as Config
import qualified Path
import qualified Path.IO            as PathIO
import qualified ProjectManager.App as App

import ProjectManager.Model.Project (Project (..))

import ProjectManager.App (MonadApp)
import Path (Path, Abs, Rel, Dir, File)


listProjectsInDirectory :: MonadApp m => Path Abs Dir -> m [Project]
listProjectsInDirectory container = do
    subdirs <- fst <$> PathIO.listDir container
    configs <- fmap catMaybes $ for subdirs $ \path ->
        (path,) <<$>> Package.tryGetConfigFile path
    for configs $ \(path, cfg) ->
        pure $ Project (cfg ^. Config.name) cfg path Nothing


    {-projectPaths <- liftIO $ Package.getLunaPackagesFromDir container-}
    {-for projectPaths $ \configPath -> do-}
        {-Just projectPath <- liftIO $ Package.findPackageRootForFile configPath-}
        {-pure $ Project (convert $ Package.getPackageName projectPath) container (unsafeFromJust $ Path.stripProperPrefix container projectPath) Nothing-}
