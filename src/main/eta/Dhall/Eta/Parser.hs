module Dhall.Eta.Parser (
  -- * Utilities
    exprFromText
  , exprAndHeaderFromText
  
  -- * Parsers
  -- , expr, exprA

  -- * Types
  , JSrc(..)
  , JParseError(..)
  -- , Parser(..)
  ) where

import qualified Dhall.Parser as Dhall 

import Dhall.Eta.Core.Java
import Dhall.Eta.Parser.Java
import Eta.Types
import Java


exprFromText :: JString -> JString -> JEither JParseError (JExpr JSrc JImport)
exprFromText source input =
  deepToJava $ Dhall.exprFromText (fromJava source) (fromJava input)

foreign export java "@static org.dhall.eta.Parser.exprFromText"
  exprFromText :: JString -> JString
               -> JEither JParseError (JExpr JSrc JImport)

exprAndHeaderFromText :: JString -> JString
                      -> JEither JParseError (Pair JString (JExpr JSrc JImport))
exprAndHeaderFromText delta text =
  deepToJava $ Dhall.exprAndHeaderFromText (fromJava delta) (fromJava text)

foreign export java "@static org.dhall.eta.Parser.exprAndHeaderFromText"
  exprAndHeaderFromText :: JString -> JString
                        -> JEither JParseError (Pair JString (JExpr JSrc JImport))
