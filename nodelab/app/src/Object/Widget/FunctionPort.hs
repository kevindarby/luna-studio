{-# LANGUAGE OverloadedStrings #-}
module Object.Widget.FunctionPort where

import           Data.Aeson                          (ToJSON)
import           Empire.API.Data.Input               (Input)
import qualified Empire.API.Data.Input               as Input
import           Empire.API.Data.Output              (Output)
import qualified Empire.API.Data.Output              as Output
import qualified Empire.API.Data.ValueType           as ValueType
import           Object.Widget
import qualified Reactive.Commands.Node.Ports.Colors as Colors
import           Utils.PreludePlus
import           Utils.Vector



data InputOutput = Input | Output
        deriving (Eq, Show, Typeable, Generic)


data FunctionPort = FunctionPort
                    { _position          :: Vector2 Double
                    , _hovered           :: Bool
                    , _size              :: Vector2 Double
                    , _inputOutput       :: InputOutput
                    , _color             :: Int
                    , _unhoverLabelValue :: Text
                    , _hoverLabelValue   :: Text
                    } deriving (Eq, Show, Typeable, Generic)

makeLenses ''FunctionPort

instance ToJSON InputOutput
instance ToJSON FunctionPort
instance IsDisplayObject FunctionPort where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True

labelValue :: Getter FunctionPort Text
labelValue = to $ \fp -> if fp ^. hovered
    then fp ^. hoverLabelValue
    else fp ^. unhoverLabelValue

create :: InputOutput -> Int -> Text -> Text -> FunctionPort
create = FunctionPort def False (Vector2 50 50)

fromInput :: Input -> FunctionPort
fromInput input = create Input (Colors.vtToColor vt) label labelWithType where
    vt    = input ^. Input.valueType
    label = input ^. Input.name
    labelWithType = label <> " :: " <> vt ^. ValueType.toText

fromOutput :: Output -> FunctionPort
fromOutput output = create Output (Colors.vtToColor vt) label label where
    vt    = output ^. Output.valueType
    label = "-> " <> vt ^. ValueType.toText