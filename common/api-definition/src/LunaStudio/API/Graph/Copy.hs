module LunaStudio.API.Graph.Copy where

import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.NodeLoc       (NodeLoc)
import           Prologue


data Request = Request
    { _location :: GraphLocation
    , _selected :: [NodeLoc]
    } deriving (Eq, Generic, Show)

data Result = Result
    { _clipboardPlain :: String
    , _clipboardMeta  :: String
    } deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance NFData Request
instance ToJSON Request
instance Binary Result
instance NFData Result
instance ToJSON Result
instance G.GraphRequest Request where location = location

instance T.MessageTopic Request where
    topic = "empire.graph.copy"
