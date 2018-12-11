{-# LANGUAGE ScopedTypeVariables #-}
module Dhall.Eta.Binary
  ( JStandardVersion
  , defaultStandardVersion
--  , parseStandardVersion

  -- * Encoding and decoding
  , encodeWithVersion
  , decodeWithVersion
  ) where

import qualified Codec.Serialise as Serialise

import qualified Dhall.Binary as Dhall
import qualified Dhall.Core   as Dhall
import qualified Java.Utils as Java (Enum)

import Data.ByteString.Lazy (ByteString)

import Dhall.Eta.Core.Java

import Eta.Types
import Java

data JStandardVersion = JStandardVersion @org.dhall.StandardVersion
  deriving (Class,Show,Eq)

type instance Inherits JStandardVersion = '[Java.Enum JStandardVersion]

foreign import java unsafe
  "@static @field org.dhall.StandardVersion.V_4_0_0" jV_4_0_0 :: JStandardVersion

instance JavaConverter Dhall.StandardVersion JStandardVersion where
  toJava Dhall.V_4_0_0 = jV_4_0_0
  fromJava jstdVer | jstdVer == jV_4_0_0 = Dhall.V_4_0_0
                   | otherwise           =
                       error $ "Unknown StandardVersion:" ++ show jstdVer

defaultStandardVersion :: JStandardVersion
defaultStandardVersion = toJava Dhall.defaultStandardVersion

foreign export java "@static org.dhall.eta.StandarVersion.defaultStandardVersion"
  defaultStandardVersion :: JStandardVersion

data JDecodingFailure = JDecodingFailure @org.dhall.binary.DecodingFailure
  deriving (Class,Show,Eq)

data JCannotDecodeVersionString = JCannotDecodeVersionString  @org.dhall.binary.CannotDecodeVersionString 
  deriving (Class,Show,Eq)

type instance Inherits JCannotDecodeVersionString = '[JDecodingFailure]

foreign import java unsafe "@new"
  newCannotDecodeVersionString  :: JByteArray -> JCannotDecodeVersionString

foreign import java unsafe "getTerm"
  jcannotDecodeVersionStringTerm :: JCannotDecodeVersionString
                                 -> JByteArray

data JUnsupportedVersionString = JUnsupportedVersionString  @org.dhall.binary.UnsupportedVersionString 
  deriving (Class,Show,Eq)

type instance Inherits JUnsupportedVersionString = '[JDecodingFailure]

foreign import java unsafe "@new"
  newUnsupportedVersionString  :: JString -> JUnsupportedVersionString

foreign import java unsafe "getVersionString"
  junsupportedVersionString :: JUnsupportedVersionString
                            -> JString

data JCBORIsNotDhall = JCBORIsNotDhall  @org.dhall.binary.CBORIsNotDhall 
  deriving (Class,Show,Eq)

type instance Inherits JCBORIsNotDhall = '[JDecodingFailure]

foreign import java unsafe "@new"
  newCBORIsNotDhall  :: JByteArray -> JCBORIsNotDhall

foreign import java unsafe "getTerm"
  jcBORIsNotDhallTerm :: JCBORIsNotDhall -> JByteArray

instance JavaConverter Dhall.DecodingFailure JDecodingFailure where
  toJava (Dhall.CannotDecodeVersionString term) =
    superCast $ newCannotDecodeVersionString (toJava $ Serialise.serialise term)
  toJava (Dhall.UnsupportedVersionString verStr) =
    superCast $ newUnsupportedVersionString (toJava verStr)
  toJava (Dhall.CBORIsNotDhall term) =
    superCast $ newCBORIsNotDhall (toJava $ Serialise.serialise term)

  fromJava jdf
    | jdf `instanceOf` getClass (Proxy :: Proxy JCannotDecodeVersionString) =
        Dhall.CannotDecodeVersionString
        (Serialise.deserialise
          $ fromJava $ jcannotDecodeVersionStringTerm
          $ ((unsafeCast jdf) :: JCannotDecodeVersionString))
    | jdf `instanceOf` getClass (Proxy :: Proxy JUnsupportedVersionString) =
        Dhall.UnsupportedVersionString
        (fromJava $ junsupportedVersionString
                  $ ((unsafeCast jdf) :: JUnsupportedVersionString))
    | jdf `instanceOf` getClass (Proxy :: Proxy JCBORIsNotDhall) =
        Dhall.CBORIsNotDhall
        (Serialise.deserialise
          $ fromJava $ jcBORIsNotDhallTerm
          $ ((unsafeCast jdf) :: JCBORIsNotDhall))
    | otherwise = error $ "Unknown DecodingFailure subclass: " ++ show jdf

encodeWithVersion :: JStandardVersion -> JExpr JUnit JImport -> List JByte
encodeWithVersion jver jexpr = byteArrayToList $ toJava bs
  where ver :: Dhall.StandardVersion = fromJava jver
        expr :: Dhall.Expr () Dhall.Import = fromJava jexpr
        term = Dhall.encodeWithVersion ver expr
        bs :: ByteString = Serialise.serialise term
        
foreign export java "@static org.dhall.eta.Binary.encodeWithVersion"
  encodeWithVersion :: JStandardVersion -> JExpr JUnit JImport -> List JByte

decodeWithVersion :: List JByte
                  -> JEither JDecodingFailure (JExpr JUnit JImport)
decodeWithVersion jbs = deepToJava res
  where bs :: ByteString = fromJava $ byteArrayFromList jbs
        term = Serialise.deserialise bs
        res :: Either Dhall.DecodingFailure (Dhall.Expr () Dhall.Import) =
          Dhall.decodeWithVersion term

foreign export java "@static org.dhall.eta.Binary.decodeWithVersion"
  decodeWithVersion :: List JByte
                    -> JEither JDecodingFailure (JExpr JUnit JImport)
