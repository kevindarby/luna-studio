---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Graph.Edge(
Edge(..),
noEdges,
) where

import qualified Data.Graph.Inductive    as DG
import qualified Data.Serialize          as Serialize
import           Data.Serialize            (Serialize)
import           Data.List                 (sortBy)
import           Control.Monad             (liftM)


data Edge = Standard deriving (Show, Read, Ord, Eq)
    
noEdges :: [Edge]
noEdges = [] 




------------------------- INSTANCES -------------------------

--instance Serialize EdgeCls where
--  put i = Serialize.put $ show i
--  get   = liftM (read :: String -> EdgeCls) (Serialize.get :: Serialize.Get String)


--instance Serialize Edge where
--  put i = Serialize.put (src i, dst i, cls i)
--  get   = do
--            (src', dst', cls') <- Serialize.get
--            return $ Edge src' dst' cls'
