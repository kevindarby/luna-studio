module Empire.API.Graph.GetProgram where

import           Data.Binary                   (Binary)
import           Data.Text.Lazy                (Text)
import           Prologue

import           Empire.API.Data.Graph         (Graph)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.NodeSearcher  (Items)
import qualified Empire.API.Response           as Response
import qualified Empire.API.Graph.Request      as G

data Request = Request { _location :: GraphLocation
                       } deriving (Generic, Show, Eq)

data Result  = Result  { _graph            :: Graph
                       , _code             :: Text
                       , _nodeSearcherData :: Items
                       } deriving (Generic, Show, Eq)

type Response = Response.Response Request Result

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance Binary Result
instance G.GraphRequest Request where location = location
