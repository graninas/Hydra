module Language.Haskell.TH.MakeFunctor where

import qualified Data.List                    as L
import           Hydra.Prelude
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype

makeFunctorInstance :: Name -> Q [Dec]
makeFunctorInstance name =
  (: []) <$> instanceD (cxt []) (appT (conT $ mkName "Functor") (conT name)) [makeFmap name]

makeFmap :: Name -> Q Dec
makeFmap name = do
  constructors <- datatypeCons <$> reifyDatatype name
  funD (mkName "fmap") (map makeFmapBody constructors)

makeFmapBody :: ConstructorInfo -> Q Clause
makeFmapBody info = do
  let argsCount = length $ constructorFields info
  let varNames  = [ mkName $ "a" <> show a | a <- [1 .. argsCount] ]
  let consName  = constructorName info
  let vars      = map VarE $ L.init varNames
  let expr      = UInfixE (VarE $ mkName "g") (VarE $ mkName ".") (VarE $ last varNames)
  clause
    [ varP $ mkName "g"
    , conP consName (map varP varNames)
    ]
    (normalB $ foldApp $ ConE consName : vars ++ [expr])
    []

foldApp :: [Exp] -> Q Exp
foldApp = pure . foldl1 AppE
