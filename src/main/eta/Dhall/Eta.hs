module Dhall.Eta where

import qualified Dhall
import qualified Data.Text as Text
import Java


input :: Dhall.Type a ->  JString -> IO a
input ty = ( Dhall.input ty ) . Text.pack . fromJava

autoInput :: Dhall.Interpret a => JString -> IO a
autoInput = input Dhall.auto

foreign export java "@static org.dhall.eta.Input.bool" autoInput
  :: JString -> IO Bool

foreign export java "@static org.dhall.eta.Input.str" autoInput
  :: JString -> IO String

foreign export java "@static org.dhall.eta.Input.integral" autoInput
  :: JString -> IO Integer

foreign export java "@static org.dhall.eta.Input.natural" natural
  :: JString -> IO Integer
natural = ( fmap toInteger ) . ( input Dhall.natural )

