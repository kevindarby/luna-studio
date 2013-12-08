---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Transform.HAST.HASTGen.Utils where

import qualified Flowbox.Luna.Data.HAST.Expr as HExpr
import qualified Flowbox.Luna.Data.HAST.Lit  as HLit
import           Flowbox.Prelude             hiding (error, id)


mkCFName :: String -> String
mkCFName     = ("CF_" ++)

mkCFLName :: String -> String
mkCFLName    = ("CF_" ++)

mkCCName :: String -> String
mkCCName     = ("CC_" ++)

mkGetName :: String -> String
mkGetName    = ("get" ++)

mkTHVarName :: String -> String
mkTHVarName  = ("'" ++)

mkTHTypeName :: String -> String
mkTHTypeName = ("''" ++)

mkFieldName :: String -> String
mkFieldName  = (++"_T")

mkFuncName :: String -> String
mkFuncName   = ("f_" ++)

mkVarName :: String -> String
mkVarName    = ("_" ++)

mkConsName :: String -> String
mkConsName   = ("_" ++)

mkLamName :: String -> String
mkLamName    = ("Lambda_" ++)

mangleName cname fname = cname ++ fname

--mkTName      = (++ "_T")
mkTName i name = mkGetNName i ++ "_" ++ name ++ "_T"

mkCGetCName i = "Get" ++ show i
mkGetNName  i = "get" ++ show i
mkCGetName  i = mkGetNName i

--genTH f a b c = HExpr.AppE (HExpr.Var f)
--              $ HExpr.AppE (HExpr.Var $ mkTHTypeName a)
--              $ HExpr.AppE (HExpr.Var $ mkTHVarName  b)
--              $ HExpr.Var $ mkTHVarName c

genTH f a b c = foldl (HExpr.AppE) (HExpr.Var f) vars where
                            vars = map HExpr.Var [mkTHTypeName a, mkTHVarName b, mkTHVarName c]
genTHInst  = genTH "mkInst"


genTHInstMem name func = foldl (HExpr.AppE) (HExpr.Var "mkInstMem") vars where
                            vars = [ HExpr.Lit $ HLit.String name
                                   , HExpr.Var $ mkTHVarName func
                                   ]


genCFDec cname cfname = foldl HExpr.AppE (HExpr.Var "mkNTWrapper") [ HExpr.Lit $ HLit.String cfname
                                                                   , HExpr.Var $ mkTHTypeName cname
                                                                   ]


genCCDec name = HExpr.DataD name [] [HExpr.Con name []] []

genDTGet0 name params = HExpr.InstanceD (foldl (HExpr.AppE) (HExpr.ConT "Get0") [baseType, baseType])
                      $ [HExpr.Function "get0" [] $ HExpr.VarE "id"]
                                                where baseType = mkPure $ foldl (HExpr.AppE) (HExpr.ConT name) $ map HExpr.VarE params

--genCon name fnum = HExpr.Function ("con" ++ mkConsName name) []
--                 $ HExpr.AppE (HExpr.Var $ "mkPure" ++ show fnum)
--                 $ HExpr.Var name

genCon name ccname = HExpr.Function ("con" ++ mkConsName name) []
                   $ (HExpr.Var ccname) -- mkPure


mkPure   = HExpr.AppE (HExpr.Var "Pure")
mkPureIO = HExpr.AppE (HExpr.Var "pureIO")
mkGetIO  = HExpr.AppE (HExpr.Var "getIO")
mkIO     = HExpr.AppE (HExpr.ConE ["IO"])


emptyHExpr = mkPureIO (HExpr.Var "()")


mkMemberGetter name = HExpr.AppE (HExpr.VarE "member") (HExpr.TypedE (HExpr.AppT (HExpr.ConT "Proxy") (HExpr.LitT $ HLit.String name)) (HExpr.ConE ["Proxy"]) )
