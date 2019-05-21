module ProjectManager.Model.Project where

import Prologue

import qualified Luna.Package.Configuration.Local as Package

import Path (Path, Abs, Rel, Dir, File)

data Project = Project
    { _name         :: Text
    , _config       :: Package.Config
    , _path         :: Path Abs Dir
    , _thumbnail    :: Maybe (Path Rel File)
    } deriving (Show, Eq)

makeLenses ''Project
