{-# LANGUAGE RecordWildCards, ScopedTypeVariables, FlexibleInstances,
             UndecidableInstances, AllowAmbiguousTypes #-}
module Dhall.Eta
  (
    -- * Input
    input
--  , inputWithSettings
  , inputFile
--  , inputFileWithSettings
  , inputExpr
--   , inputExprWithSettings
--   , rootDirectory
--   , sourceName
--   , startingContext
-- --  , normalizer
--   , standardVersion
--   , defaultInputSettings
--   , InputSettings
--   , defaultEvaluateSettings
--   , EvaluateSettings
--   , HasEvaluateSettings
--   , detailed

  -- * Types
  , JType(..)
  , bool
  , string
  , natural
  , bigInteger
  , bigDecimal
  , list
  , unit
  , pair
  , optional
  , homMap
  , objMap
  , record

--  , inputType
  ) where

import qualified Dhall
import qualified Dhall.Core as Dhall hiding (Type)
import qualified Dhall.Parser as Dhall (Src)
import qualified Dhall.TypeCheck as Dhall  (X)
import qualified Dhall.Map as Dhall (Map)
import qualified Dhall.Map
import qualified Data.Text as Text
import qualified Eta.Types as Types (optional)
import Control.Monad (forM_)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Dhall.Eta.Core.Java hiding (JType)
import Dhall.Eta.Parser.Java
import Java
import Java.Math
import Eta.Types hiding (optional, unit)



data JType ja = JType (@org.dhall.eta.Type ja)
  deriving (Class,Show,Eq)

foreign import java unsafe "@interface extract"
  jextract :: (ja <: Object) => JExpr JSrc JX -> Java (JType ja) (Optional ja)

foreign import java unsafe "@interface expected"
  jexpected :: (ja <: Object) => Java (JType ja) (JExpr JSrc JX)

foreign import java unsafe "@wrapper extract,expected"
  mkJType :: (ja <: Object)
          => (JExpr JSrc JX -> Java (JType ja) (Optional ja))
          -> Java (JType ja) (JExpr JSrc JX)
          -> JType ja

instance forall ja. Class ja
      => JavaConverter (Dhall.Type ja) (JType ja) where
  toJava  (Dhall.Type {..}) = mkJType jextract jexpected
    where jextract :: JExpr JSrc JX
                   -> Java (JType ja) (Optional ja)
          jextract jexpr = return $ toJava (extract (fromJava jexpr))
          jexpected :: Java (JType ja) (JExpr JSrc JX) =
            return $ toJava expected
  fromJava jt = Dhall.Type {..}
    where extract expr = fromJava $ unsafePerformJavaWith jt
                                  $ jextract (toJava expr)
          expected = fromJava $ unsafePerformJavaWith jt
                              $ jexpected

instance forall a ja. (Class ja, DeepToJava a ja)
      => DeepToJava (Dhall.Type a) (JType ja) where
  deepToJava ty = toJava tyj
    where tyj :: Dhall.Type ja = fmap deepToJava ty
    
instance forall a ja. (Class ja, DeepFromJava ja a)
      => DeepFromJava (JType ja) (Dhall.Type a) where
  deepFromJava jty = fmap deepFromJava tyj
    where tyj :: Dhall.Type ja = fromJava jty
    
dhallInput :: Dhall.Type a -> JString -> IO a
dhallInput ty = ( Dhall.input ty ) . Text.pack . fromJava

input :: forall ja. (Class ja) => JType ja -> JString -> IO ja
input jty = dhallInput (fromJava jty)

foreign export java "@static org.dhall.eta.Input.type" input
  :: (ja <: Object) => JType ja -> JString -> IO ja

inputFile :: (Class ja) => JType ja -> JString -> IO ja
inputFile jty path = Dhall.inputFile (fromJava jty) (fromJava path)

foreign export java "@static org.dhall.eta.Input.typeFile" inputFile
  :: (ja <: Object) => JType ja -> JString -> IO ja

inputExpr :: JString -> IO (JExpr JSrc JX)
inputExpr = fmap toJava . Dhall.inputExpr . fromJava

foreign export java "@static org.dhall.eta.Input.expr" inputExpr
  :: JString -> IO (JExpr JSrc JX)

-- Boolean

dhallBool :: Dhall.Type JBoolean
dhallBool = fmap toJava Dhall.bool

foreign export java "@static org.dhall.eta.Types.bool" bool
  :: JType JBoolean

bool :: JType JBoolean
bool = toJava dhallBool

foreign export java "@static org.dhall.eta.Input.bool" inputBool
  :: JString -> IO JBoolean

inputBool :: JString -> IO JBoolean
inputBool = dhallInput dhallBool

-- String

dhallString :: Dhall.Type JString
dhallString = fmap toJava Dhall.string

foreign export java "@static org.dhall.eta.Types.str" string
  :: JType JString

string :: JType JString
string = toJava dhallString

foreign export java "@static org.dhall.eta.Input.str" inputString
  :: JString -> IO JString

inputString :: JString -> IO JString
inputString = dhallInput dhallString

-- BigInteger

dhallBigInteger :: Dhall.Type BigInteger
dhallBigInteger = fmap toJava Dhall.integer

foreign export java "@static org.dhall.eta.Types.bigInt" bigInteger
  :: JType BigInteger

bigInteger :: JType BigInteger
bigInteger = toJava dhallBigInteger

foreign export java "@static org.dhall.eta.Input.bigInt" inputBigInteger
  :: JString -> IO BigInteger

inputBigInteger :: JString -> IO BigInteger
inputBigInteger = dhallInput dhallBigInteger

-- Natural

dhallNatural :: Dhall.Type JNatural
dhallNatural = fmap toJava Dhall.natural

foreign export java "@static org.dhall.eta.Types.natural" natural
  :: JType JNatural

natural :: JType JNatural 
natural = toJava dhallNatural

foreign export java "@static org.dhall.eta.Input.natural" inputNatural
  :: JString -> IO JNatural

inputNatural :: JString -> IO JNatural  
inputNatural = dhallInput dhallNatural

-- BigDecimal

dhallBigDecimal :: Dhall.Type BigDecimal
dhallBigDecimal = fmap toJava Dhall.scientific

foreign export java "@static org.dhall.eta.Types.bigDecimal" bigDecimal
  :: JType BigDecimal

bigDecimal :: JType BigDecimal
bigDecimal = toJava dhallBigDecimal

foreign export java "@static org.dhall.eta.Input.bigDecimal" inputBigDecimal
  :: JString -> IO BigDecimal

inputBigDecimal :: JString -> IO BigDecimal
inputBigDecimal = dhallInput dhallBigDecimal

-- Double

dhallDouble :: Dhall.Type JDouble
dhallDouble = fmap toJava Dhall.double

foreign export java "@static org.dhall.eta.Types.doubleDecimal" doubleDecimal
  :: JType JDouble

doubleDecimal :: JType JDouble
doubleDecimal = toJava dhallDouble

foreign export java "@static org.dhall.eta.Input.doubleDecimal" inputDoubleDecimal
  :: JString -> IO JDouble

inputDoubleDecimal :: JString -> IO JDouble
inputDoubleDecimal = dhallInput dhallDouble


optional :: forall a. (Class a) => JType a -> JType (Optional a)
optional jty = toJava optty
  where ty :: Dhall.Type a = fromJava jty
        mbty :: Dhall.Type (Maybe a) = Dhall.maybe ty
        optty :: Dhall.Type (Optional a) = fmap toJava mbty

foreign export java "@static org.dhall.eta.Types.optional" optional
  :: (a <: Object) => JType a -> JType (Optional a)

list :: forall a. (Class a) => JType a -> JType (List a)
list jty = toJava listty
  where ty :: Dhall.Type a = fromJava jty
        xsty :: Dhall.Type [a] = Dhall.list ty
        listty :: Dhall.Type (List a) = fmap toJava xsty

foreign export java "@static org.dhall.eta.Types.list" list
  :: (a <: Object) => JType a -> JType (List a)
 
unit :: JType JUnit
unit = deepToJava Dhall.unit

foreign export java "@static org.dhall.eta.Types.unit" unit
  :: JType JUnit

pair :: forall a b. (Class a, Class b) => JType a -> JType b -> JType (Pair a b)
pair jtya jtyb = toJava jp
  where tya :: Dhall.Type a = fromJava jtya
        tyb :: Dhall.Type b = fromJava jtyb
        p :: Dhall.Type (a,b) = Dhall.pair tya tyb
        jp :: Dhall.Type (Pair a b) = fmap toJava p

foreign export java "@static org.dhall.eta.Types.pair" pair
  :: (a <: Object, b <: Object) => JType a -> JType b -> JType (Pair a b)

type DhallExpr =  Dhall.Expr Dhall.Src Dhall.X

objMap :: forall v. Class v
       => Map JString (JType v) -> JType (Map JString v)
objMap jfieldsMap = toJava $ Dhall.Type {..}
  where fields :: [(Text,Dhall.Type v)] =
            map (\ (f,s) -> (fromJava f, fromJava s)) jfields
          where jfields :: [(JString,JType v)] = fromJava jfieldsMap  

        fieldsMap :: Dhall.Map Text (Dhall.Type v) =
          Dhall.Map.fromList fields

        valueType :: Text -> Dhall.Type v
        valueType k = fromJust $ Dhall.Map.lookup k fieldsMap

        extract :: DhallExpr -> Maybe (Map JString v) 
        extract (Dhall.RecordLit efields) = Just $
          unsafePerformJava $ do
            newMap <- newHashMap (length efields)
            newMap <.>
              (forM_ (Dhall.Map.toList efields) $
                \ (k, v) -> put (toJava k)
                                (fromJust $ Dhall.extract (valueType k) $ v))
            return $ superCast newMap
            
        extract _ = Nothing
        
        expected :: DhallExpr
        expected = Dhall.Record $ fmap Dhall.expected fieldsMap

foreign export java "@static org.dhall.eta.Types.objMap" objMap
  :: (v <: Object) => Map JString (JType v) -> JType (Map JString v)

homMap :: forall v. (Class v)
    => List JString -> JType v -> JType (Map JString v)
homMap jfields jvalueType = objMap jfieldsMap
  where fields :: [JString] = fromJava jfields
        jfieldsMap  = unsafePerformJava $ do
            newMap <- newHashMap (length fields)
            newMap <.> ( forM_ fields $ \ f -> put f jvalueType)
            return $ superCast newMap

foreign export java "@static org.dhall.eta.Types.homMap" homMap
  :: (v <: Object) => List JString -> JType v -> JType (Map JString v)

data JRecordType ja = JRecordType (@org.dhall.eta.RecordType ja)
  deriving (Class)

foreign import java unsafe "@interface getFieldsTypes"
  jrecordTyFieldsTypes :: (ja <: Object) => Java (JRecordType ja) (Map JString (JType Object))

foreign import java unsafe "@interface fromFieldsValues"
  jrecordTyFromFieldsValues :: (ja <: Object)
                            => Map JString Object
                            -> Java (JRecordType ja) ja

record :: forall ja. (Class ja) => JRecordType ja -> JType ja
record recTy = mkJType jextract' jexpected'
  where mapTy ::  JType (Map JString Object)
        mapTy = objMap (unsafePerformJavaWith recTy jrecordTyFieldsTypes)

        jextract' :: JExpr JSrc JX
                  -> Java (JType ja) (Optional ja)
        jextract' jexpr =  do
          map <- mapTy <.> jextract jexpr
          res <- recTy <.> jrecordTyFromFieldsValues (optGet map)
          return $ Types.optional res

        jexpected' = mapTy <.> jexpected

foreign export java "@static org.dhall.eta.Types.record" record
  :: (ja <: Object) => JRecordType ja -> JType ja


data TagValue v = TagValue (@org.dhall.eta.TagValue v)
  deriving (Class,Show,Eq)

foreign import java unsafe "@new"
  newTagValue :: (v <: Object) => JString -> v -> TagValue v


unionTag :: forall v. Class v
         => Map JString (JType v) -> JType (TagValue v)
unionTag jfieldsMap = toJava $ Dhall.Type {..}
  where fields :: [(Text,Dhall.Type v)] =
            map (\ (f,s) -> (fromJava f, fromJava s)) jfields
          where jfields :: [(JString,JType v)] = fromJava jfieldsMap  

        fieldsMap :: Dhall.Map Text (Dhall.Type v) =
          Dhall.Map.fromList fields

        valueType :: Text -> Dhall.Type v
        valueType k = fromJust $ Dhall.Map.lookup k fieldsMap

        extract :: DhallExpr -> Maybe (TagValue v) 
        extract (Dhall.UnionLit tag exprVal _) =
          Just $ newTagValue (toJava tag) val
          where val :: v = fromJust $ Dhall.extract (valueType tag) $ exprVal
        extract _ = Nothing
        
        expected :: DhallExpr
        expected = Dhall.Union $ fmap Dhall.expected fieldsMap

foreign export java "@static org.dhall.eta.Types.unionTag" unionTag
  :: (v <: Object) => Map JString (JType v) -> JType (TagValue v)

enumTag :: forall v. (Class v)
        => List JString -> JType v -> JType (TagValue v)
enumTag jfields jvalueType = unionTag jfieldsMap
  where fields :: [JString] = fromJava jfields
        jfieldsMap  = unsafePerformJava $ do
            newMap <- newHashMap (length fields)
            newMap <.> ( forM_ fields $ \ f -> put f jvalueType)
            return $ superCast newMap

foreign export java "@static org.dhall.eta.Types.enumTag" enumTag
  :: (v <: Object) => List JString -> JType v -> JType (TagValue v)


data JUnionType ja = JUnionType (@org.dhall.eta.UnionType ja)
  deriving (Class)

foreign import java unsafe "@interface getTagsTypes"
  junionTyTagsTypes :: (ja <: Object) => Java (JUnionType ja) (Map JString (JType Object))

foreign import java unsafe "@interface fromTagValue"
  junionTyFromTagValue :: (ja <: Object)
                       => TagValue Object
                       -> Java (JUnionType ja) ja

union :: forall ja. (Class ja) => JUnionType ja -> JType ja
union unionTy = mkJType jextract' jexpected'
  where tagTy ::  JType (TagValue Object)
        tagTy = unionTag (unsafePerformJavaWith unionTy junionTyTagsTypes)

        jextract' :: JExpr JSrc JX
                  -> Java (JType ja) (Optional ja)
        jextract' jexpr =  do
          tag <- tagTy <.> jextract jexpr
          res <- unionTy <.> junionTyFromTagValue (optGet tag)
          return $ Types.optional res

        jexpected' = tagTy <.> jexpected

foreign export java "@static org.dhall.eta.Types.union" union
  :: (ja <: Object) => JUnionType ja -> JType ja

data JInputType ja = JInputType (@org.dhall.eta.InputType ja)
  deriving (Class)

foreign import java unsafe "embed"
  jembed :: (ja <: Object) => ja -> Java (JInputType ja) (JExpr JSrc JX)

foreign import java unsafe "declared"
  jdeclared :: (ja <: Object) => Java (JInputType ja) (JExpr JSrc JX)

foreign import java unsafe "@wrapper embed,declared"
  mkJInputType :: (ja <: Object)
          => (ja -> Java (JInputType ja) (JExpr JSrc JX))
          -> Java (JInputType ja) (JExpr JSrc JX)
          -> JInputType ja

instance forall ja. Class ja
      => JavaConverter (Dhall.InputType ja) (JInputType ja) where
  toJava  (Dhall.InputType {..}) = mkJInputType jembed jdeclared
    where jembed :: ja -> Java (JInputType ja) (JExpr JSrc JX)
          jembed ja = return $ toJava (embed ja)
          jdeclared :: Java (JInputType ja) (JExpr JSrc JX) =
            return $ toJava declared
  fromJava jt = Dhall.InputType {..}
    where embed ja = fromJava $ unsafePerformJavaWith jt
                              $ jembed ja
          declared = fromJava $ unsafePerformJavaWith jt
                              $ jdeclared

instance forall a ja. (Class ja, DeepFromJava ja a)
      => DeepToJava (Dhall.InputType a) (JInputType ja) where
  deepToJava ty = toJava tyj
    where tyj :: Dhall.InputType ja = contramap deepFromJava ty
    
instance forall a ja. (Class ja, DeepToJava a ja)
      => DeepFromJava (JInputType ja) (Dhall.InputType a) where
  deepFromJava jty = contramap deepToJava tyj
    where tyj :: Dhall.InputType ja = fromJava jty

inputType :: forall proxy a ja.(Dhall.Inject a, Class ja, JavaConverter a ja)
          => proxy a -> Dhall.InterpretOptions -> JInputType ja
inputType _ opts = toJava intyj
  where inty :: Dhall.InputType a = Dhall.injectWith opts
        intyj :: Dhall.InputType ja = contramap fromJava inty

