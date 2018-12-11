{-# LANGUAGE ScopedTypeVariables #-}
module Dhall.Eta.Context where

import Data.Text (Text)
import Dhall.Context as Dhall
import Eta.Types
import Java

data JContext a = JContext (@org.dhall.Context a)
  deriving (Class,Show,Eq)

foreign import java unsafe "@new"
  newContext :: (a <: Object) => List (Pair JString a) -> JContext a

foreign import java unsafe "getBindings"
  jcontextBindings ::  JContext a -> List (Pair JString a)

instance (Class ja, JavaConverter a ja) => JavaConverter (Dhall.Context a) (JContext ja) where
  toJava = newContext . deepToJava . Dhall.toList
  fromJava = mkContext . deepFromJava . jcontextBindings
  
mkContext :: [(Text, a)] -> Dhall.Context a
mkContext = foldr (uncurry Dhall.insert) Dhall.empty

