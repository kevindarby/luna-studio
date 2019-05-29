module ProjectManager.Api.File where

import Prologue

import qualified Control.Lens.Aeson as LensAeson
import qualified Data.Aeson         as Aeson

data File = File
    { _name :: Text
    , _kind :: Text
    , _children :: [File]
    } deriving (Eq, Generic, Show)
makeLenses ''File

instance Aeson.ToJSON File where
    toJSON     = LensAeson.toJSON
    toEncoding = LensAeson.toEncoding

instance Aeson.FromJSON File where
    parseJSON = LensAeson.parse

