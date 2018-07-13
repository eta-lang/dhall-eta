module Dhall.Eta where

import qualified Dhall
import qualified Data.Text as Text
import Java

foreign export java "@static org.dhall.eta.Input.bool" bool
  :: JString -> IO Bool
bool = ( Dhall.input Dhall.bool ) . Text.pack . fromJava
