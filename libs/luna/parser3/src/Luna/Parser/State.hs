---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Luna.Parser.State where

import           Flowbox.Prelude
import qualified Luna.Data.ASTInfo    as ASTInfo
import           Luna.Data.ASTInfo    (ASTInfo)
import           Luna.Data.SourceMap  (SourceMap)
import qualified Luna.Data.SourceMap  as SourceMap
import qualified Luna.Data.Config     as Config
import           Luna.Data.Config     (Config)
import           Luna.Parser.Operator (OperatorMap)
import qualified Luna.Data.Namespace  as Namespace
import           Luna.Data.Namespace  (Namespace)
import qualified Data.List            as List
import qualified Luna.AST.Common      as AST
import           Luna.AST.IDMap       (IDMap)
import qualified Luna.AST.IDMap       as IDMap
import qualified Data.Maps            as Map
import           Luna.AST.Comment     (Comment(..))
import           Flowbox.Control.Monad.State (mapStateVal, get, put)
import qualified Luna.Data.AliasInfo          as Alias
import qualified Luna.AST.AST                 as AST



data State conf = State { _conf          :: Config conf
                     , _info          :: ASTInfo
                     , _opFixity      :: OperatorMap
                     , _sourceMap     :: SourceMap
                     , _namespace     :: Namespace
                     , _adhocReserved :: [String]
                     , _comments      :: IDMap [Comment]
                     } deriving (Show)

makeLenses ''State


------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------

mk :: ASTInfo -> State ()
mk i = def & info .~ i

addReserved words = adhocReserved %~ (++words)
delReserved words = adhocReserved %~ (flip (foldl (flip List.delete)) words)

lastID            = view (info . ASTInfo.lastID)
addComment cmt s  = s & comments %~ Map.insertWith (++) (lastID s) [cmt]

registerComment = mapStateVal . addComment . Comment

regParent id pid = mapStateVal $ namespace %~ Namespace.regParent id pid

pushNewScope id = mapStateVal $ namespace %~ Namespace.pushNewScope id
pushScope    id = mapStateVal $ namespace %~ Namespace.pushScope id
popScope        = mapStateVal $ namespace %~ Namespace.popScope

regVarName id name = do
    pid <- getPid
    withAlias $ Alias.regVarName pid id name

regTypeName id name = do
    pid <- getPid
    withAlias $ Alias.regTypeName pid id name

withAlias f = mapStateVal (namespace . Namespace.info %~ f)

withReserved words p = do
    s <- get
    let reserved = view adhocReserved s
    put $ (addReserved words s)
    ret <- p
    s   <- get
    put (s & adhocReserved .~ reserved)
    return ret


withNewScope id p = do
    pushNewScope id
    ret <- p
    popScope
    return ret

    
withScope id p = do
    pushScope id
    ret <- p
    popScope
    return ret



getPid = do
    mpid <- Namespace.head . view namespace <$> get
    case mpid of
        Nothing  -> fail "Internal parser error. Cannot optain pid."
        Just pid -> return pid

getScope  = view (namespace . Namespace.info . Alias.scope) <$> get
--getASTMap = view (namespace . Namespace.info . Alias.ast) <$> get



registerID id = do
    pid <- getPid
    regParent id pid

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance conf~() => Default (State conf) where
        def = State def def def def def def def



