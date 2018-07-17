module Dhall.Eta where

import qualified Dhall
import qualified Data.Text as Text
import Java


input :: Dhall.Type a ->  JString -> IO a
input ty = ( Dhall.input ty ) . Text.pack . fromJava

foreign export java "@static org.dhall.eta.Input.bool" bool
  :: JString -> IO Bool
bool = input Dhall.bool

foreign export java "@static org.dhall.eta.Input.str" str
  :: JString -> IO JString
str = fmap toJava . input Dhall.string

foreign export java "@static org.dhall.eta.Input.integer" integer
  :: JString -> IO JInteger
integer = fmap toJava . input Dhall.integer

foreign export java "@static org.dhall.eta.Input.natural" natural
  :: JString -> IO JInteger
natural = fmap ( toJava . toInteger ) . input Dhall.natural

