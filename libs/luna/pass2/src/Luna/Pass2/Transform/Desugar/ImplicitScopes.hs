---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Luna.Pass2.Transform.Desugar.ImplicitScopes where

import           Flowbox.Prelude              hiding (Traversal)
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
import qualified Luna.ASTNew.Traversals       as AST
import qualified Luna.ASTNew.Enum             as Enum
import           Luna.ASTNew.Enum             (Enumerated, IDTag(IDTag))
import qualified Luna.ASTNew.Decl             as Decl
import           Luna.ASTNew.Decl             (Decl, LDecl, Field(Field))
import qualified Luna.ASTNew.Module           as Module
import           Luna.ASTNew.Module           (Module(Module), LModule)
import           Luna.ASTNew.Unit             (Unit(Unit))
import qualified Luna.ASTNew.Label            as Label
import           Luna.ASTNew.Label            (Label(Label))
import qualified Luna.ASTNew.Type             as Type
import           Luna.ASTNew.Type             (Type)
import qualified Luna.ASTNew.Pat              as Pat
import           Luna.ASTNew.Pat              (LPat, Pat)
import           Luna.ASTNew.Expr             (LExpr, Expr)
import qualified Luna.ASTNew.Expr             as Expr
import qualified Luna.ASTNew.Lit              as Lit
import qualified Luna.ASTNew.Native           as Native
import qualified Luna.ASTNew.Name             as Name
import           Luna.ASTNew.Name             (TName(TName), TVName(TVName))
import           Luna.Pass                    (Pass(Pass), PassMonad, PassCtx)
import qualified Luna.Pass                    as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.ASTInfo            (ASTInfo, genID)

import qualified Luna.Data.Namespace.State    as State 
import qualified Luna.Parser.Parser           as Parser
import qualified Luna.Parser.State            as ParserState
import           Luna.ASTNew.Arg              (Arg(Arg))
import           Luna.ASTNew.Name.Pattern     (NamePat(NamePat), Segment(Segment))
import           Luna.Data.StructInfo         (StructInfo)
import qualified Luna.Data.StructInfo         as StructInfo
import Control.Monad (join)

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data PassState = PassState { _astInfo    :: ASTInfo 
                           , _structInfo :: StructInfo 
                           } deriving (Show)


makeLenses ''PassState

data ImplScopes = ImplScopes

type ISPass                 m     = PassMonad PassState m
type ISCtx              lab m a   = (Monad m, Enumerated lab, ISTraversal m a, Num lab)
type ISTraversal            m a   = (PassCtx m, AST.Traversal        ImplScopes (ISPass m) a a)
type ISDefaultTraversal     m a   = (PassCtx m, AST.DefaultTraversal ImplScopes (ISPass m) a a)


------------------------------------------------------------------------
---- Utils functions
------------------------------------------------------------------------

traverseM :: (ISTraversal m a) => a -> ISPass m a
traverseM = AST.traverseM ImplScopes

defaultTraverseM :: (ISDefaultTraversal m a) => a -> ISPass m a
defaultTraverseM = AST.defaultTraverseM ImplScopes


------------------------------------------------------------------------
---- Pass functions
------------------------------------------------------------------------

pass :: ISDefaultTraversal m a => Pass PassState (ASTInfo -> StructInfo -> a -> ISPass m (a, ASTInfo))
pass = Pass "Implicit self" "Desugars AST by adding implicit self function parameters" undefined passRunner

passRunner ai si ast = do
    put $ PassState ai si
    (,) <$> defaultTraverseM ast <*> (view astInfo <$> get)

exprScopes :: ISCtx lab m a => LExpr lab a -> ISPass m (LExpr lab a)
exprScopes ast@(Label lab e) = case e of
	Expr.Var (Expr.Variable name v) -> do
		si <- view structInfo <$> get
		let parentMap = view StructInfo.parent si
		    aliasMap  = view StructInfo.alias si
		    pid       = view (at id) parentMap
		    tgt       = fmap (view StructInfo.target) $ view (at id) aliasMap
		    tgtPid    = join $ fmap (\tid -> view (at tid) parentMap) tgt

		if pid == tgtPid then return ast
                         else return $ Label (-888) $ Expr.Accessor (convert name) (Label lab $ Expr.Var $ Expr.Variable "self" v)
	_                               -> continue
	where continue = defaultTraverseM ast
	      id       = Enum.id lab

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance ISCtx lab m a => AST.Traversal ImplScopes (ISPass m) (LExpr lab a) (LExpr lab a) where
    traverseM _ = exprScopes

