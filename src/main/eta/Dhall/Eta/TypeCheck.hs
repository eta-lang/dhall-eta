{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Dhall.Eta.TypeCheck (
    -- * Type-checking
      typeWith
    , typeOf
    , typeOfResolved
--    , typeWithA
    , checkContext

    -- * Types
    -- , Typer
    , JX(..)
    , JTypeError(..)
    , JDetailedTypeError(..)
    , JTypeMessage(..)
    ) where

import qualified Dhall.Context   as Dhall
import qualified Dhall.Core      as Dhall
import qualified Dhall.Parser    as Dhall
import qualified Dhall.TypeCheck as Dhall

import Data.Proxy

import Dhall.Eta.Context
import Dhall.Eta.Core
import Dhall.Eta.Core.Java
import Dhall.Eta.Parser.Java
import Dhall.Eta.TypeCheck.Java

import Eta.Types

import Java

_Src :: Proxy Dhall.Src
_Src = Proxy

typeWith :: forall proxy s js. (Class js, JavaConverter s js)
         => proxy s -> JContext (JExpr js JX) -> JExpr js JX
         -> JEither (JTypeError js JX) (JExpr js JX)
typeWith _ jctx jexpr = deepToJava $ Dhall.typeWith ctx expr
  where ctx :: Dhall.Context (Dhall.Expr s Dhall.X) = fromJava jctx
        expr :: Dhall.Expr s Dhall.X = fromJava jexpr

jtypeWith :: JContext (JExpr JSrc JX) -> JExpr JSrc JX
         -> JEither (JTypeError JSrc JX) (JExpr JSrc JX)
jtypeWith = typeWith _Src

foreign export java "@static org.dhall.eta.TypeCheck.typeWith"
  jtypeWith :: JContext (JExpr JSrc JX) -> JExpr JSrc JX
            -> JEither (JTypeError JSrc JX) (JExpr JSrc JX)

typeOf :: forall proxy s js. (Class js, JavaConverter s js)
       => proxy s -> JExpr js JX -> JEither (JTypeError js JX) (JExpr js JX)
typeOf _ jexpr = deepToJava $ Dhall.typeOf expr
  where expr :: Dhall.Expr s Dhall.X = fromJava jexpr

typeOfResolved :: JExpr JSrc JX -> JEither (JTypeError JSrc JX) (JExpr JSrc JX)
typeOfResolved = typeOf _Src

foreign export java "@static org.dhall.eta.TypeCheck.typeOfResolved"
  typeOfResolved :: JExpr JSrc JX
                 -> JEither (JTypeError JSrc JX) (JExpr JSrc JX)
  
checkContext :: forall proxy s js. (Class js, JavaConverter s js)
             => proxy s -> JContext (JExpr js JX)
             -> JEither (JTypeError js JX) JUnit
checkContext _ jctx = deepToJava $ Dhall.checkContext ctx
  where ctx :: Dhall.Context (Dhall.Expr s Dhall.X) = fromJava jctx
 
jcheckContext :: JContext (JExpr JSrc JX)
              -> JEither (JTypeError JSrc JX) JUnit
jcheckContext = checkContext _Src

foreign export java "@static org.dhall.eta.TypeCheck.checkContext"
  jcheckContext :: JContext (JExpr JSrc JX)
                -> JEither (JTypeError JSrc JX) JUnit
