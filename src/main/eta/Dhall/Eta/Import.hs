{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Dhall.Eta.Import
  ( exprFromImport
  , exprToImport
  , load
  , loadWith
--  , hashExpression
--  , hashExpressionToCode
  , JStatus
  , emptyStatus
  ) where
import qualified Dhall.Binary    as Dhall
import qualified Dhall.Core      as Dhall
import qualified Dhall.Import    as Dhall
import qualified Dhall.Parser    as Dhall
import qualified Dhall.TypeCheck as Dhall

import qualified Control.Monad.Trans.State.Strict as State
import qualified Text.Dot as Dot

import Dhall.Eta.Binary
import Dhall.Eta.Context
import Dhall.Eta.Core.Java
import Dhall.Eta.Parser.Java

import Eta.Types

import Java

import Lens.Family (view, (&), (.~))

data JStatus = JStatus @org.dhall.imports.Status
  deriving (Class,Show,Eq)

foreign import java unsafe "@new" newStatus  :: JNonEmptyArrayList JImport
                                             -> Map JImport (JExpr JSrc JX)
                                             -> JStandardVersion
                                             -> JContext (JExpr JSrc JX)
                                             -> JStatus

foreign import java unsafe "getStack"
  jstatusStack  :: JStatus -> JNonEmptyArrayList JImport

foreign import java unsafe "getCache"
  jstatusCache  :: JStatus -> Map JImport (JExpr JSrc JX)

foreign import java unsafe "getStandardVersion"
  jstatusStdVer  :: JStatus -> JStandardVersion

foreign import java unsafe "getStartingContext"
  jstatusStartCtx :: JStatus -> JContext (JExpr JSrc JX)

instance JavaConverter (Dhall.Status IO) JStatus where
  toJava status = newStatus jstack jcache jstdVer jstartCtx
    where jstack :: JNonEmptyArrayList JImport =
            deepToJava $ view Dhall.stack status
          jcache :: Map JImport (JExpr JSrc JX)=
            deepToJava $ fmap snd $ view Dhall.cache status
          jstdVer :: JStandardVersion =
            toJava $ view Dhall.standardVersion status
          jstartCtx :: JContext (JExpr JSrc JX) =
            toJava $ view Dhall.startingContext status
  fromJava jstatus = Dhall.emptyStatus "."
                     & Dhall.stack .~ (deepFromJava $ jstatusStack jstatus)
                     & Dhall.cache .~
                         (fmap ((,) ((error "Not Dot.NodeId available") :: Dot.NodeId))
                               (deepFromJava $ jstatusCache jstatus))
                     & Dhall.standardVersion .~ (fromJava $ jstatusStdVer jstatus)
                     & Dhall.startingContext .~ (fromJava $ jstatusStartCtx jstatus)

emptyStatus :: JString -> JStatus
emptyStatus = toJava . Dhall.emptyStatus . fromJava

exprFromImport :: JStatus -> JImport
               -> IO (Pair JStatus (JExpr JSrc JImport))
exprFromImport js ji = do
    sexpr <- State.runStateT (Dhall.exprFromImport i) s
    let jexpr :: (JExpr JSrc JImport) = toJava $ fst sexpr
        jstate :: JStatus = toJava $ snd sexpr
    return $ newPair jstate jexpr
  where s :: Dhall.Status IO = fromJava js 
        i :: Dhall.Import = fromJava ji

foreign export java "@static org.dhall.eta.Import.exprFromImport"
  exprFromImport :: JStatus -> JImport
                 -> IO (Pair JStatus (JExpr JSrc JImport))

exprToImport :: JStatus -> JImport -> JExpr JSrc JX -> IO JStatus
exprToImport js ji jexpr = do
  svoid <- State.runStateT (Dhall.exprToImport i expr) s
  return $ toJava $ snd svoid
  where s :: Dhall.Status IO = fromJava js 
        i :: Dhall.Import = fromJava ji
        expr :: Dhall.Expr Dhall.Src Dhall.X = fromJava jexpr

foreign export java "@static org.dhall.eta.Import.exprToImport"
  exprToImport :: JStatus -> JImport -> JExpr JSrc JX -> IO JStatus

load :: JExpr JSrc JImport -> IO (JExpr JSrc JX)
load jexpr = do
  loaded <- Dhall.load expr 
  let jloaded :: JExpr JSrc JX = toJava loaded
  return jloaded
  where expr :: Dhall.Expr Dhall.Src Dhall.Import = fromJava jexpr

foreign export java "@static org.dhall.eta.Import.load"
  load :: JExpr JSrc JImport -> IO (JExpr JSrc JX)

loadWith :: JStatus -> JExpr JSrc JImport -> IO (Pair JStatus (JExpr JSrc JX))
loadWith js ji = do
  sexpr <- State.runStateT (Dhall.loadWith i) s
  let jexpr :: (JExpr JSrc JX) = toJava $ fst sexpr
      jstate :: JStatus = toJava $ snd sexpr
  return $ newPair jstate jexpr
  where s :: Dhall.Status IO = fromJava js 
        i :: Dhall.Expr Dhall.Src Dhall.Import = fromJava ji
  
foreign export java "@static org.dhall.eta.Import.loadWith"
  loadWith :: JStatus -> JExpr JSrc JImport
           -> IO (Pair JStatus (JExpr JSrc JX))

hashExpression :: JStandardVersion -> JExpr JUnit JX -> JDigest JSHA256
hashExpression jstdVer jexpr = toJava $ Dhall.hashExpression stdVer expr
  where stdVer :: Dhall.StandardVersion = fromJava jstdVer
        expr   :: Dhall.Expr () Dhall.X = fromJava jexpr

foreign export java "@static org.dhall.eta.Import.hashExpression"
  hashExpression :: JStandardVersion -> JExpr JUnit JX -> JDigest JSHA256

hashExpressionToCode :: JStandardVersion -> JExpr JUnit JX -> JString
hashExpressionToCode jstdVer jexpr = toJava $ Dhall.hashExpressionToCode stdVer expr
  where stdVer :: Dhall.StandardVersion = fromJava jstdVer
        expr   :: Dhall.Expr () Dhall.X = fromJava jexpr

foreign export java "@static org.dhall.eta.Import.hashExpressionToCode"
  hashExpressionToCode :: JStandardVersion -> JExpr JUnit JX -> JString
