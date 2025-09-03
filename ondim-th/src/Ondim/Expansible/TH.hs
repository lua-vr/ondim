{-# LANGUAGE TemplateHaskellQuotes #-}

module Ondim.Expansible.TH where

import Data.Char (toLower)
import Data.List qualified as L
import Language.Haskell.TH
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Optics
import Unsafe.Coerce (unsafeCoerce)
import Optics.Core

makeHoles :: Name -> [Name] -> DecsQ
makeHoles datatype holes = do
  reify datatype >>= \case
    TyConI dec -> do
      scopedVars <- forM holes \hole -> (hole,) <$> newName "a"
      let newDName = setName datatype
          (decNamed, ks) =
            usingState [] $
              dec
                & adjoin (_DataD % _2) (_NewtypeD % _2) .~ newDName
                & adjoin (_DataD % _3) (_NewtypeD % _3) %~ setBinds scopedVars
                & adjoin (_DataD % _5 % traversed) (_NewtypeD % _5)
                `traverseOf` setCon scopedVars
      let isoName = holeIsoName datatype
          origVars = adjoin (_DataD % _3) (_NewtypeD % _3) % traversed % name `toListOf` dec
      sequence
        [ pure decNamed,
          mkHoleIsoSig isoName origVars holes datatype newDName,
          mkHoleIsoFn isoName ks
        ]
    _ -> fail $ show datatype <> " is not a data type."
  where
    setCon :: [(Name, Name)] -> Con -> State [(Name, Name, [Int])] Con
    setCon vars con =
      con
        & name
        .~ newCName
        & types
        `traverseOf` st0
        & eval
      where
        eval x = do
          let (c, is) = usingState [] x
          modify' (++ [(oldCName, newCName, is)]) $> c
        oldCName = con ^. name
        newCName = setName oldCName
        st0 x = do
          let (t, i) = usingState 0 (setType vars x)
          modify' (++ [i]) $> t
    setType :: [(Name, Name)] -> TH.Type -> State Int TH.Type
    setType vars = fix \go -> \case
      ConT n | n == datatype -> put 1 $> applied
      ConT n | Just s <- L.lookup n vars -> pure $ VarT s
      ForallT x y z -> ForallT x y <$> go z
      ForallVisT x y -> ForallVisT x <$> go y
      AppT x y -> AppT <$> (go x <* put 0) <*> (go y <* modify' increment)
      AppKindT x y -> AppKindT <$> go x ?? y
      SigT x y -> SigT <$> go x ?? y
      InfixT x y z -> InfixT <$> go x ?? y <*> go z
      UInfixT x y z -> UInfixT <$> go x ?? y <*> go z
      ParensT x -> ParensT <$> go x
      x -> pure x
      where
        increment n
          | n > 0 = n + 1
          | otherwise = n
        applied = foldl' AppT (ConT datatypeNewName) (VarT . snd <$> vars)
    setName :: Name -> Name
    setName = mkName . (<> "'") . nameBase
    setBinds vars b = foldr ((:) . plainTV . snd) b vars
    datatypeNewName = setName datatype

holeIsoName :: Name -> Name
holeIsoName original = nameBase original & mapHead toLower & (++ "HoleIso") & mkName
  where
    mapHead f (x : xs) = f x : xs
    mapHead _ [] = []

mkHoleIsoSig :: Name -> [Name] -> [Name] -> Name -> Name -> Q Dec
mkHoleIsoSig decName (fmap varT -> originalVars) (fmap conT -> holes) original new =
  sigD decName [t|Iso' $lhs $rhs|]
  where
    concretize t = foldl' appT (conT t)
    lhs = concretize original originalVars
    rhs = concretize new (holes ++ originalVars)

mkHoleIsoFn :: Name -> [(Name, Name, [Int])] -> Q Dec
mkHoleIsoFn decName conPairs =
  funD
    decName
    [ clause
        []
        (normalB [e|iso $(mkIso conPairs) $(mkIso revConPairs)|])
        []
    ]
  where
    revConPairs = map (\(x, y, z) -> (y, x, z)) conPairs
    mkIso pairs = do
      -- go <- newName "go"
      -- let fixName = 'fix
      --     fmapName = 'fmap
      -- appE (varE fixName) $
      --   lam1E (varP go) $
      --     lamCaseE $
      --       pairs <&> \(oldCName, newCName, arities) -> do
      --         vars <- forM arities ((,) <$> newName "x" ??)
      --         let pat = conP oldCName (varP . fst <$> vars)
      --             conv n 0 = varE n
      --             conv n k = appE (fn k) (varE n)
      --               where
      --                 fn 1 = varE go
      --                 fn i = appE (varE fmapName) (fn (i - 1))
      --             expr = foldl' appE (conE newCName) (uncurry conv <$> vars)
      --         match pat (normalB expr) []
      let ucName = 'unsafeCoerce
      varE ucName
