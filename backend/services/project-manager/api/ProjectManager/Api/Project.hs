module ProjectManager.Api.Project where

import Prologue hiding (Item)

import qualified Control.Lens.Aeson as LensAeson
import qualified Data.Aeson         as Aeson

import Path (Path, Abs, Dir)

data Item = Item
    { _name         :: Text
    , _path         :: Path Abs Dir
    , _thumb        :: Maybe Text
    , _description  :: Maybe Text
    , _lastOpenTime :: Maybe Int64
    , _openUri      :: Text
    } deriving (Eq, Generic, Show)

makeLenses ''Item

instance Aeson.ToJSON Item where
    toJSON     = LensAeson.toJSON
    toEncoding = LensAeson.toEncoding

instance Aeson.FromJSON Item where
    parseJSON = LensAeson.parse

