{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables,
             FlexibleInstances, UndecidableInstances,
             AllowAmbiguousTypes #-}
module Eta.Types where

import qualified Data.List.NonEmpty as NonEmpty (toList, fromList)
import qualified Data.Map as Map
import qualified Data.Set (Set, fromList, toList, map)

import Data.List.NonEmpty (NonEmpty)

import Data.Functor.Identity
import Data.Proxy
import Data.Scientific (Scientific)
import Java
import Java.Math
import Numeric.Natural (Natural)

class Class j => DeepToJava t j where
  deepToJava :: t -> j

class Class j => DeepFromJava j t where
  deepFromJava :: j -> t

instance {-# OVERLAPPABLE #-} (Class j , JavaConverter t j) => DeepToJava t j where
  deepToJava = toJava

instance {-# OVERLAPPABLE #-} (Class j , JavaConverter t j) => DeepFromJava j t where
  deepFromJava = fromJava

instance {-# OVERLAPPABLE #-} (ja <: Object) => JavaConverter ja ja where
  toJava = id
  fromJava = id

data JNatural = JNatural @org.dhall.common.types.Natural
   deriving (Class,Show,Eq)

foreign import java unsafe "@new" newNatural  :: JString -> JNatural

foreign import java unsafe "getValue" natVal :: JNatural -> BigInteger

foreign import java unsafe "getValueAsStr" natValStr :: JNatural -> JString

instance JavaConverter Natural JNatural where
  toJava nat = newNatural $ toJava $ show nat
  fromJava jnat = read $ fromJava $ natValStr jnat

foreign import java unsafe "@new" newBigInteger  :: JString -> BigInteger

foreign import java unsafe "toString" bigIntegerToStr  :: BigInteger -> JString

deriving instance Show BigInteger

instance JavaConverter Integer BigInteger where
  toJava i = newBigInteger $ toJava $ show i
  fromJava ji = read $ fromJava $ bigIntegerToStr ji

foreign import java unsafe "@new" newBigDecimal  :: JString -> BigDecimal

foreign import java unsafe "toString" bigDecimalStr :: BigDecimal -> JString

deriving instance Show BigDecimal

instance JavaConverter Scientific BigDecimal where
  toJava d = newBigDecimal $ toJava $ show d
  fromJava jd = read $ fromJava $ bigDecimalStr jd

instance JavaConverter Double BigDecimal where
  toJava d = newBigDecimal $ toJava $ show d
  fromJava jd = read $ fromJava $ bigDecimalStr jd

data JEither l r = JEither (@org.dhall.common.types.Either l r)
 deriving (Class,Show,Eq)

foreign import java unsafe "@static org.dhall.common.types.Either.left"
  left :: (l <: Object, r <: Object) => l -> JEither l r

foreign import java unsafe "@static org.dhall.common.types.Either.right"
  right :: (l <: Object, r <: Object) => r -> JEither l r

data JLeft l r = JLeft (@org.dhall.common.types.either.Left l r)
 deriving (Class,Show,Eq)

type instance Inherits (JLeft l r) = '[JEither l r]

foreign import java unsafe "getValue"
  jleftValue :: (l <: Object, r <: Object) => JLeft l r -> l

data JRight l r = JRight (@org.dhall.common.types.either.Right l r)
 deriving (Class,Show,Eq)

type instance Inherits (JRight l r) = '[JEither l r]
 
foreign import java unsafe "getValue"
  jrightValue :: (l <: Object, r <: Object) => JRight l r -> r

instance (Class jl, Class jr, DeepToJava l jl, DeepToJava r jr) =>
         DeepToJava (Either l r) (JEither jl jr) where
  deepToJava (Left l)  = toJava je
    where je :: Either jl jr = Left $ deepToJava l
  deepToJava (Right r) = toJava je
    where je :: Either jl jr = Right $ deepToJava r
    
instance (Class jl, Class jr, DeepFromJava jl l, DeepFromJava jr r) =>
         DeepFromJava (JEither jl jr) (Either l r) where
  deepFromJava je = case e of Left jl -> Left $ deepFromJava jl
                              Right jr -> Right $ deepFromJava jr
    where e :: Either jl jr = fromJava je

instance (Class jl, Class jr) => JavaConverter (Either jl jr) (JEither jl jr) where
  toJava (Left l)  = left l
  toJava (Right r) = right r
  
  fromJava je | je `instanceOf` getClass (Proxy :: Proxy (JLeft jl jr)) =
                 Left $ left je
              | je `instanceOf` getClass (Proxy :: Proxy (JRight jl jr)) =
                 Right $ right je
              | otherwise = error $ "Unknown subclass: " ++ show je
              
    where left :: JEither jl jr -> jl
          left je = jleftValue ((unsafeCast je) :: JLeft jl jr)

          right :: JEither jl jr -> jr
          right je = jrightValue ((unsafeCast je) :: JRight jl jr)


data Optional t = Optional (@java.util.Optional t)
  deriving (Class,Show,Eq)

foreign import java unsafe "@static java.util.Optional.ofNullable"
  optional :: (t <: Object) => t -> Optional t

foreign import java unsafe "@static java.util.Optional.empty"
  optEmpty :: (t <: Object) => Optional t

foreign import java unsafe "get"
  optGet :: (t <: Object) => Optional t -> t

foreign import java unsafe "isPresent"
  optIsPresent :: (t <: Object) => Optional t -> Bool


instance {-# OVERLAPPING #-} (Class j,DeepToJava t j)
                          => DeepToJava (Maybe t) (Optional j) where
  deepToJava mb = toJava mbj
     where mbj :: Maybe j = fmap deepToJava mb

instance {-# OVERLAPPING #-} (Class j,DeepFromJava j t)
                          => DeepFromJava (Optional j) (Maybe t) where
  deepFromJava opt = fmap deepFromJava mbj
    where mbj :: Maybe j = fromJava opt
    
instance (Class j) => JavaConverter (Maybe j) (Optional j) where
  toJava (Just j) = optional j
  toJava Nothing  = optEmpty

  fromJava opt | optIsPresent opt = Just (optGet opt)
               | otherwise        = Nothing

data Pair a b = Pair (@org.dhall.common.types.Pair a b)
  deriving (Class,Show,Eq)

foreign import java unsafe "@static org.dhall.common.types.Pair.of"
  newPair :: (f <: Object, s <:Object) => f -> s -> Pair f s

foreign import java unsafe "getFst"
  pairFst :: (f <: Object, s <:Object) => Pair f s -> f

foreign import java unsafe "getSnd"
  pairSnd :: (f <: Object, s <:Object) => Pair f s -> s

instance (Class jf, Class js, DeepToJava f jf, DeepToJava s js) =>
          DeepToJava (f,s) (Pair jf js) where
   deepToJava (f,s) = toJava pj
     where pj :: (jf,js) = (deepToJava f, deepToJava s) 
     

instance (Class jf, Class js, DeepFromJava jf f, DeepFromJava js s) =>
          DeepFromJava (Pair jf js) (f,s) where
   deepFromJava jp = (deepFromJava jf, deepFromJava js) 
     where (jf, js) :: (jf, js) = fromJava jp

instance (Class jf, Class js) => JavaConverter (jf,js) (Pair jf js) where
  toJava (a,b) = newPair a b
  fromJava p = (pairFst p, pairSnd p) 

instance (Class j, DeepToJava t j) => DeepToJava [t] (List j) where
  deepToJava xs = toJava $ ((fmap deepToJava xs) :: [j])

instance (Class j, DeepFromJava j t) => DeepFromJava (List j) [t] where
  deepFromJava jlist = fmap deepFromJava $ ((fromJava jlist) :: [j])


instance (Ord t, Ord j, Class j, DeepToJava t j)
      => DeepToJava (Data.Set.Set t) (Set j) where
  deepToJava set  = toJava sj
    where sj :: Data.Set.Set j = Data.Set.map deepToJava set

instance (Ord t, Ord j, Class j, DeepFromJava j t)
      => DeepFromJava (Set j) (Data.Set.Set t) where  
  deepFromJava jset = Data.Set.map deepFromJava sj 
    where sj :: Data.Set.Set j = fromJava jset

instance (Ord j, Class j) => JavaConverter (Data.Set.Set j) (Set j) where
  toJava set = toJava $ Data.Set.toList set
  fromJava jset = Data.Set.fromList $ fromJava jset
    
data JNonEmptyArrayList e = JNonEmptyArrayList (@org.dhall.common.types.NonEmptyArrayList e)
  deriving (Class,Show,Eq)

type instance Inherits (JNonEmptyArrayList e) = '[List e]

foreign import java unsafe "@static org.dhall.common.types.NonEmptyArrayList.of"
  newNonEmptyArrayList :: (e <: Object) => List e -> JNonEmptyArrayList e

instance (Class j, DeepToJava t j)
      => DeepToJava (NonEmpty t) (JNonEmptyArrayList j) where
  deepToJava ne = toJava nej
    where nej :: NonEmpty j = fmap deepToJava ne

instance (Class j, DeepFromJava j t)
      => DeepFromJava (JNonEmptyArrayList j) (NonEmpty t) where
  deepFromJava jne = fmap deepFromJava nej
    where nej :: NonEmpty j = fromJava jne

instance (Class j) => JavaConverter (NonEmpty j) (JNonEmptyArrayList j) where
  toJava xs = newNonEmptyArrayList $ toJava $ NonEmpty.toList xs
  fromJava jlist = NonEmpty.fromList $ asList
    where asList :: [j]
          asList = fromJava $ ((superCast jlist) :: List j)

instance (Class jk, Class jv, DeepToJava k jk, DeepToJava v jv)
      => DeepToJava (Map.Map k v) (Map jk jv) where
  deepToJava m = jm
    where toJavaKeyVal (k,v)  = (deepToJava k, deepToJava v)
          jxs :: [(jk, jv)] =  map toJavaKeyVal (Map.toList m)
          jm :: Map jk jv = toJava jxs

instance (Ord k, Class jk, Class jv, DeepFromJava jk k, DeepFromJava jv v)
      => DeepFromJava (Map jk jv) (Map.Map k v) where
  deepFromJava jm = m
    where fromJavaKeyVal (jk,jv)  = (deepFromJava jk, deepFromJava jv)
          jxs :: [(jk, jv)] = fromJava jm
          xs :: [(k, v)] =  map fromJavaKeyVal jxs
          m :: Map.Map k v = Map.fromList xs

data Function t r = Function (@java.util.function.Function t r)
  deriving Class

foreign import java unsafe "@wrapper apply"
  mkFunction :: (t <: Object, r <: Object)
             => (t -> Java (Function t r) r) -> Function t r

foreign import java unsafe "@interface apply"
  applyFunction :: (t <: Object, r <: Object)
                => t -> Java (Function t r) r

instance (Class ja, Class jb, DeepFromJava ja a, DeepToJava b jb)
      => DeepToJava (a -> b) (Function ja jb) where
  deepToJava f = toJava fj 
    where fj :: ja -> jb
          fj = deepToJava . f . deepFromJava

instance (Class ja, Class jb, DeepToJava a ja, DeepFromJava jb b)
      => DeepFromJava (Function ja jb) (a -> b) where
  deepFromJava jf = deepFromJava . fj . deepToJava
    where fj :: ja -> jb
          fj = fromJava jf
          
instance (Class ja, Class jb)
      => JavaConverter (ja -> jb) (Function ja jb) where 
  toJava f = mkFunction ( return . f )
  fromJava jf = apply
    where apply ja = unsafePerformJavaWith jf $ applyFunction ja
 
data JUnit = JUnit @org.dhall.common.types.Unit
   deriving (Class,Show,Eq)

foreign import java unsafe "@static @field org.dhall.common.types.Unit.instance"
  unit :: JUnit

instance JavaConverter () JUnit where
  toJava _ = unit
  fromJava _ = ()

jfmap :: forall proxy f jf jt jr.
         (Functor f, Class (jf jt), Class (jf jr), Class jt, Class jr,
          JavaConverter (f jt) (jf jt), JavaConverter (f jr) (jf jr))
      => proxy f -> Function jt jr -> jf jt -> jf jr
jfmap _ jfun jfunctor = jres
  where fun :: (jt -> jr) = fromJava jfun
        functor :: (f jt) = fromJava jfunctor
        res :: (f jr) = fmap fun functor
        jres :: (jf jr) = toJava res

jfmapOpt :: forall jt jr. (Class jt, Class jr)
         => Function jt jr -> Optional jt -> Optional jr
jfmapOpt = jfmap (Proxy :: Proxy Maybe)

foreign export java "@static org.eta.types.Optional.fmap"
  jfmapOpt :: (t <: Object, r <: Object)
           => Function t r -> Optional t -> Optional r

jcombine :: forall proxy s js.
            (Semigroup s, Class js, JavaConverter s js)
         => proxy s -> js -> js -> js
jcombine _ jx jy = jz
  where x  :: s   = fromJava jx
        y  :: s   = fromJava jy
        z  :: s   = x <> y
        jz :: js = toJava z

data JIdentity a = JIdentity (@org.dhall.common.types.functor.Identity a)
   deriving (Class,Show,Eq)

foreign import java unsafe "@new" newIdentity  :: (a <: Object)
                                               => a -> JIdentity a

foreign import java unsafe "get" jrunIdentity :: (a <: Object)
                                              => JIdentity a -> a

instance Class a => JavaConverter (Identity a) (JIdentity a) where
  toJava = newIdentity . runIdentity
  fromJava = pure . jrunIdentity

data HashMap k v = HashMap (@java.util.HashMap k v)
   deriving (Class,Show,Eq)

type instance Inherits (HashMap k v) = '[Map k v]

foreign import java unsafe "@new" newHashMap
  :: (k <: Object, v <: Object) => Int -> Java a (HashMap k v)

data LinkedHashMap k v = LinkedHashMap (@java.util.LinkedHashMap k v)
  deriving (Class,Show,Eq)

type instance Inherits (LinkedHashMap k v) = '[Map k v]

foreign import java unsafe "@new" newLinkedHashMap
  :: Int -> Java c (LinkedHashMap k v)

foreign import java unsafe "@interface put" put
  :: (k <: Object, v <: Object, b <: (Map k v))
  => k -> v -> Java b v

foreign import java unsafe "@interface entrySet" jmapEntries
  :: (k <: Object, v <: Object) => Java (Map k v) (Set (MapEntry k v))

data MapEntry k v = MapEntry (@java.util.Map$Entry k v)
  deriving Class

foreign import java unsafe "@interface getKey" getKey
  :: (k <: Object, v <: Object) => Java (MapEntry k v) k

foreign import java unsafe "@interface getValue" getValue
  :: (k <: Object, v <: Object) => Java (MapEntry k v) v

foreign import java unsafe "@new" wrapByte :: Byte -> JByte

foreign import java unsafe "byteValue" unwrapByte :: JByte -> Byte

byteArrayToList :: JByteArray -> List JByte
byteArrayToList ba = toJava jbas
  where bas :: [Byte] = unsafePerformJavaWith ba $ arrayToList
        jbas :: [JByte] = map wrapByte bas

byteArrayFromList :: List JByte -> JByteArray
byteArrayFromList bas = unsafePerformJava $ arrayFromList bas'
  where jbas :: [JByte] = fromJava bas
        bas' :: [Byte] = map unwrapByte jbas
