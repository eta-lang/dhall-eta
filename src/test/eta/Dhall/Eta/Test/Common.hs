{-# LANGUAGE ScopedTypeVariables #-}
module Dhall.Eta.Test.Common where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Dhall.Core as Dhall
import qualified Dhall.Import as Dhall
import qualified Dhall.Parser as Dhall
import qualified Dhall.TypeCheck as Dhall

import qualified Dhall.Eta.Parser as Dhall.Eta
import qualified Dhall.Eta.Import as Dhall.Eta

import Control.Monad (liftM2)
import Control.Exception
import Data.Text ( Text )

import Dhall.Eta.Core.Java
import Dhall.Eta.Parser.Java

import Eta.Types
import Java

dhallCasesBasePath :: FilePath
dhallCasesBasePath = "../dhall-lang/tests"

parseOrThrow :: Text
             -> IO ( Dhall.Expr Dhall.Src Dhall.Import, JExpr JSrc JImport )
parseOrThrow txt = do
  expr <- getOrThrow $ Dhall.exprFromText mempty txt
  let ejExpr = Dhall.Eta.exprFromText "" ( toJava txt )
      jexpr = jrightValue $ ( ( unsafeCast ejExpr )
                              :: JRight JParseError (JExpr JSrc JImport ) )
  return ( expr, jexpr )
  where getOrThrow = either throwIO return
  
resolve :: ( Dhall.Expr Dhall.Src Dhall.Import, JExpr JSrc JImport )
        -> IO ( Either SomeException ( Dhall.Expr Dhall.Src Dhall.X )
              , Either SomeException ( JExpr JSrc JX )
              )
resolve ( expr, jexpr ) =
  liftM2 (,)
    ( ( try $ Dhall.load expr )
      :: IO ( Either SomeException ( Dhall.Expr Dhall.Src Dhall.X ) ) )
    ( ( try $ Dhall.Eta.load jexpr )
      :: IO ( Either SomeException ( JExpr JSrc JX ) ) )

resolveOrThrow :: ( Dhall.Expr Dhall.Src Dhall.Import , JExpr JSrc JImport )
               -> IO ( Dhall.Expr Dhall.Src Dhall.X  , JExpr JSrc JX )
resolveOrThrow exprs = do
  ( eResExpr, jeResExpr ) <- resolve exprs
  liftM2 (,) ( getOrThrow eResExpr ) ( getOrThrow jeResExpr )
  where getOrThrow = either throwIO return 

resolveRelative :: FilePath
                -> ( Dhall.Expr Dhall.Src Dhall.Import, JExpr JSrc JImport )
                -> IO ( Either SomeException ( Dhall.Expr Dhall.Src Dhall.X )
                      , Either SomeException ( JExpr JSrc JX )
                      )
resolveRelative dir ( expr, jexpr ) = do
  let status = Dhall.emptyStatus dir

  eResolvedExpr :: Either SomeException
                   ( Dhall.Expr Dhall.Src Dhall.X ) <-
    try $ State.evalStateT ( Dhall.loadWith expr ) status
  jeResolvedExpr :: Either SomeException ( JExpr JSrc JX ) <-
    try $ fmap pairSnd ( Dhall.Eta.loadWith ( toJava status ) jexpr )
  return ( eResolvedExpr, jeResolvedExpr )

resolveRelativeOrThrow :: FilePath
                       -> ( Dhall.Expr Dhall.Src Dhall.Import
                          , JExpr JSrc JImport )
                       -> IO ( Dhall.Expr Dhall.Src Dhall.X
                             , JExpr JSrc JX )
resolveRelativeOrThrow dir exprs = do
  ( eResExpr, jeResExpr ) <- resolveRelative dir exprs
  liftM2 (,) ( getOrThrow eResExpr ) ( getOrThrow jeResExpr )
  where getOrThrow = either throwIO return 
