module Language.Haskell.TH.MakeFunctor where

import           Hydra.Prelude
import qualified Data.List as L
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype

makeFunctorInstance :: Name -> Q [Dec]
makeFunctorInstance name =
    forM [1 :: Int] $ \_ -> instanceD (cxt []) (appT (conT $ mkName "Functor") (conT name)) [makeFmap name]

makeFmap :: Name -> Q Dec
makeFmap name = do
    constructors <- datatypeCons <$> reifyDatatype name
    funD (mkName "fmap") (makeFmapBody <$> constructors)

makeFmapBody :: ConstructorInfo -> Q Clause
makeFmapBody info = clause
    [varP $ mkName "g", conP consName (varP <$> varNames)]
    (normalB
        (  foldApp
        $  ConE consName
        :  (VarE <$> L.init varNames)
        ++ [UInfixE (VarE $ mkName "g") (VarE $ mkName ".") (VarE lastArg)]
        )
    )
    []
  where
    lastArg  = last varNames
    varNames = (\a -> mkName $ "a" <> show a) <$> [1 .. argNum]
    consName = constructorName info
    argNum   = length $ constructorFields info

foldApp :: [Exp] -> Q Exp
foldApp = pure . foldl1 AppE
