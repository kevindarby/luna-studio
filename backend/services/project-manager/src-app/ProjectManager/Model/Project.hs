module ProjectManager.Model.Project where

import Prologue

import qualified Luna.Package.Configuration.Local as Package

import Path (Path, Abs, Rel, Dir, File)

data Project = Project
    { _path   :: Path Abs Dir
    , _config :: Package.Config
    , _lastOpen :: Maybe Int64
    , _hasThumb :: Bool
    } deriving (Show, Eq)

makeLenses ''Project

name :: Lens' Project Text
name = config . Package.name
