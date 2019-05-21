module ProjectManager.Api.Project where

import Prologue hiding (Item)

import qualified Control.Lens.Aeson as LensAeson
import qualified Data.Aeson         as Aeson

import Data.ByteString (ByteString)

data Item = Item
    { _name         :: Text
    , _description  :: Maybe Text
    , _lastOpenTime :: Maybe Int64
    , _thumbnailUri :: Maybe Text
    , _openUri      :: Text
    } deriving (Eq, Generic, Show)

makeLenses ''Item

instance Aeson.ToJSON Item where
    toJSON     = LensAeson.toJSON
    toEncoding = LensAeson.toEncoding

instance Aeson.FromJSON Item where
    parseJSON = LensAeson.parse

