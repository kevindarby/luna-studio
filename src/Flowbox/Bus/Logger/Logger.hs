---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Bus.Logger.Logger where

import           Control.Monad                 (forever)
import qualified Data.List                     as List
import           Flowbox.Bus.Bus               (Bus)
import qualified Flowbox.Bus.Bus               as Bus
import qualified Flowbox.Bus.Data.Message      as Message
import           Flowbox.Bus.Data.MessageFrame (MessageFrame (MessageFrame))
import           Flowbox.Bus.Data.Topic        (Topic)
import           Flowbox.Bus.EndPoint          (BusEndPoints)
import           Flowbox.Prelude               hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.Logger.Logger"


run :: BusEndPoints -> [Topic] -> IO (Either Bus.Error ())
run ep topics = Bus.runBus ep $ do logger info $ "Subscribing to topics: " ++ show topics
                                   mapM_ Bus.subscribe topics
                                   forever $ logMessage


logMessage :: Bus ()
logMessage = do msgFrame <- Bus.receive
                case msgFrame of
                    Left err -> logger error $ "Unparseable message: " ++ err
                    Right (MessageFrame msg crlID senderID lastFrame) -> do
                        let topic  = Message.topic msg
                            logMsg = show senderID
                                   ++ " -> "
                                   ++ show crlID
                                   ++ " (last = "
                                   ++ show lastFrame
                                   ++ ")"
                                   ++ "\t:: "
                                   ++ topic
                        if List.isSuffixOf "error" topic
                            then logger error logMsg
                            else logger info  logMsg


