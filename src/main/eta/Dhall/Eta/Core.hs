{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Dhall.Eta.Core
  ( -- * Syntax
    JConst(..)
  , JDirectory(..)
  , JFile(..)
  , JFilePrefix(..)
  , JImport(..)
  , JImportHashed(..)
  , JImportMode(..)
  , JImportType(..)
  , JURL(..)
  , JScheme(..)
  , JVar(..)
  , JChunks(..)
  , JExpr(..)
  
    -- * Normalization
  , alphaNormalize
  , alphaNormalizeResolved
  , normalize
  , normalizeResolved
--  , normalizeWith TODO
--  , JNormalizer
--  , JReifiedNormalizer (..)
  , judgmentallyEqual
  , subst
  , shift
  , isNormalized
--  , isNormalizedWith TODO
  , denote
  , freeIn
  
  -- * Pretty-printing
  , pretty

  ) where

import qualified Dhall.Core      as Dhall
import qualified Dhall.Parser    as Dhall
import qualified Dhall.TypeCheck as Dhall

import Data.Proxy
import Data.Text.Prettyprint.Doc (Pretty)
import Dhall.Eta.Core.Java
import Dhall.Eta.Parser.Java
import Java

_Src :: Proxy Dhall.Src
_Src = Proxy

_Import :: Proxy Dhall.Import
_Import = Proxy

_X :: Proxy Dhall.X
_X = Proxy

shift :: forall proxy s a js ja.
         (Class js, Class ja, JavaConverter s js, JavaConverter a ja)
      => proxy s -> proxy a -> JInteger -> JVar -> JExpr js ja -> JExpr js ja
shift _ _ ji jvar jexpr = toJava $ Dhall.shift i var src
  where i  = fromJava ji
        var = fromJava jvar
        src :: Dhall.Expr s a = fromJava jexpr

jshift :: JInteger -> JVar -> JExpr JSrc JImport -> JExpr JSrc JImport
jshift = shift _Src _Import

foreign export java "@static org.dhall.eta.Core.shift"
  jshift :: JInteger -> JVar -> JExpr JSrc JImport
         -> JExpr JSrc JImport

subst :: forall proxy s a js ja.
         (Class js, Class ja, JavaConverter s js, JavaConverter a ja)
      => proxy s -> proxy a -> JVar -> JExpr js ja -> JExpr js ja -> JExpr js ja
subst _ _ jvar jexprToSubst jexprSrc =
  toJava $ Dhall.subst (fromJava jvar) exprToSubst exprSrc
  where exprToSubst :: Dhall.Expr s a = fromJava jexprToSubst
        exprSrc :: Dhall.Expr s a = fromJava jexprSrc

jsubst :: JVar -> JExpr JSrc JImport -> JExpr JSrc JImport
       -> JExpr JSrc JImport
jsubst = subst _Src _Import

foreign export java "@static org.dhall.eta.Core.subst"
  jsubst :: JVar -> JExpr JSrc JImport -> JExpr JSrc JImport
         -> JExpr JSrc JImport

alphaNormalize :: forall proxy s a js ja.
                  (Class js, Class ja, JavaConverter s js, JavaConverter a ja)
               => proxy s -> proxy a -> JExpr js ja -> JExpr js ja
alphaNormalize _ _ jexpr = toJava $ Dhall.alphaNormalize expr
  where expr :: Dhall.Expr s a = fromJava jexpr

alphaNormalizeUnresolved :: JExpr JX JImport -> JExpr JX JImport
alphaNormalizeUnresolved = alphaNormalize _X _Import

alphaNormalizeResolved :: JExpr JX JX -> JExpr JX JX
alphaNormalizeResolved = alphaNormalize _X _X

foreign export java "@static org.dhall.eta.Core.alphaNormalizeUnresolved"
  alphaNormalizeUnresolved :: JExpr JX JImport -> JExpr JX JImport

foreign export java "@static org.dhall.eta.Core.alphaNormalizeResolved"
  alphaNormalizeResolved :: JExpr JX JX -> JExpr JX JX

normalize :: forall proxy s a t js ja jt.
             (Eq a, Eq ja, Class js, Class ja, Class jt,
              JavaConverter s js, JavaConverter a ja, JavaConverter t jt)
          => proxy s -> proxy a -> proxy t -> JExpr js ja -> JExpr jt ja
normalize _ _ _ jexpr = toJava $ norm
  where expr :: Dhall.Expr s a = fromJava jexpr
        norm :: Dhall.Expr t a = Dhall.normalize expr

normalizeUnresolved :: JExpr JSrc JImport -> JExpr JX JImport
normalizeUnresolved = normalize _Src _Import _X

normalizeResolved :: JExpr JSrc JX -> JExpr JX JX
normalizeResolved = normalize _Src _X _X

foreign export java "@static org.dhall.eta.Core.normalizeUnresolved"
  normalizeUnresolved :: JExpr JSrc JImport -> JExpr JX JImport

foreign export java "@static org.dhall.eta.Core.normalizeResolved"
  normalizeResolved :: JExpr JSrc JX -> JExpr JX JX

denote :: forall proxy s a t js ja jt.
         (Eq a, Eq ja, Class js, Class ja, Class jt,
          JavaConverter s js, JavaConverter a ja, JavaConverter t jt) =>
         proxy s -> proxy a -> proxy t -> JExpr js ja -> JExpr jt ja
denote _ _ _ jexpr = toJava $ denot
  where expr :: Dhall.Expr s a = fromJava jexpr
        denot :: Dhall.Expr t a = Dhall.denote expr

jdenote :: JExpr JSrc JImport -> JExpr JX JImport
jdenote = denote _Src _Import _X 

foreign export java "@static org.dhall.eta.Core.denote"
  jdenote :: JExpr JSrc JImport -> JExpr JX JImport

judgmentallyEqual :: forall proxy s a t js ja jt.
                     (Eq a, Eq ja, Class js, Class ja, Class jt,
                      JavaConverter s js, JavaConverter a ja, JavaConverter t jt)
                  => proxy s -> proxy a -> proxy t
                  -> JExpr js ja -> JExpr jt ja -> Bool
judgmentallyEqual _ _ _ jexpr1 jexpr2 = Dhall.judgmentallyEqual expr1 expr2
  where expr1 :: Dhall.Expr s a = fromJava jexpr1
        expr2 :: Dhall.Expr t a = fromJava jexpr2

jjudgmentallyEqual :: JExpr JSrc JImport -> JExpr JX JImport -> Bool
jjudgmentallyEqual = judgmentallyEqual _Src _Import _X 

foreign export java "@static org.dhall.eta.Core.judgmentallyEqual"
  jjudgmentallyEqual :: JExpr JSrc JImport -> JExpr JX JImport -> Bool

isNormalized :: forall proxy s a js ja.
                (Eq a, Eq ja, Class js, Class ja,
                 JavaConverter s js, JavaConverter a ja)
              => proxy s -> proxy a -> JExpr js ja -> Bool
isNormalized _ _ jexpr = Dhall.isNormalized expr
  where expr :: Dhall.Expr s a = fromJava jexpr

jisNormalized :: JExpr JSrc JImport -> Bool
jisNormalized = isNormalized _Src _Import

jisNormalizedResolved :: JExpr JSrc JX -> Bool
jisNormalizedResolved = isNormalized _Src _X

foreign export java "@static org.dhall.eta.Core.isNormalized"
  jisNormalized :: JExpr JSrc JImport -> Bool

foreign export java "@static org.dhall.eta.Core.isNormalizedResolved"
  jisNormalizedResolved :: JExpr JSrc JX -> Bool

freeIn :: forall proxy s a js ja.
         (Eq a, Class js, Class ja, JavaConverter s js, JavaConverter a ja)
       => proxy s -> proxy a -> JVar -> JExpr js ja -> Bool
freeIn _ _ jvar jexpr = Dhall.freeIn (fromJava jvar) expr
  where expr :: Dhall.Expr s a = fromJava jexpr

jfreeIn :: JVar -> JExpr JSrc JImport -> Bool
jfreeIn = freeIn _Src _Import

foreign export java "@static org.dhall.eta.Core.freeIn"
  jfreeIn :: JVar -> JExpr JSrc JImport -> Bool

pretty :: forall proxy s a js ja.
          (Pretty a, Pretty s, Class js, Class ja,
          JavaConverter s js, JavaConverter a ja)
       => proxy s -> proxy a -> JExpr js ja -> JString
pretty _ _ jexpr = toJava $ Dhall.pretty expr
  where expr :: Dhall.Expr s a = fromJava jexpr      

jpretty :: JExpr JSrc JImport -> JString
jpretty = pretty _Src _Import

foreign export java "@static org.dhall.eta.Core.pretty"
  jpretty :: JExpr JSrc JImport -> JString
