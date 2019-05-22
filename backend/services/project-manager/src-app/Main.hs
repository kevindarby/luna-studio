module Main where

import Prologue

import qualified Snap.Http.Server as Snap
import qualified ProjectManager.Router as Router
import qualified ProjectManager.Repository.Project as Repo

main :: IO ()
main = Snap.httpServe (Snap.setPort 50505 mempty) . Repo.run $ Router.serve
