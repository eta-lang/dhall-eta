{-# LANGUAGE ScopedTypeVariables  #-}
module Dhall.Eta.Map where

import qualified Dhall.Map
import Control.Monad (forM_)
import Eta.Types
import Java

instance (Ord k, Ord jk,  Class jk, Class jv, DeepToJava k jk, DeepToJava v jv) =>
         DeepToJava (Dhall.Map.Map k v) (LinkedHashMap jk jv)  where
  deepToJava m  = toJava mj
    where mj :: Dhall.Map.Map jk jv =
            Dhall.Map.fromList ( map deepToJava' $ Dhall.Map.toList m )
          deepToJava' :: (k,v) -> (jk, jv)
          deepToJava' (f,s) = (deepToJava f, deepToJava s)
          
instance (Ord k, Ord jk, Class jk, Class jv, DeepFromJava jk k, DeepFromJava jv v) =>
         DeepFromJava (LinkedHashMap jk jv) (Dhall.Map.Map k v) where
  deepFromJava jm = Dhall.Map.fromList ( map deepFromJava' $ Dhall.Map.toList mj )
    where mj :: Dhall.Map.Map jk jv = fromJava jm
          deepFromJava' :: (jk,jv) -> (k,v)
          deepFromJava' (jf,js) = (deepFromJava jf, deepFromJava js)
    
instance (Ord jk, Class jk, Class jv) =>
         JavaConverter (Dhall.Map.Map jk jv) (LinkedHashMap jk jv)  where
  toJava   = superCast . toLinkedHashMap
  fromJava = fromMap

toLinkedHashMap :: (Ord jk, jk <: Object, jv <: Object) =>
                   Dhall.Map.Map jk jv -> LinkedHashMap jk jv
toLinkedHashMap map = unsafePerformJava $ do
  jmap <- newLinkedHashMap (length map)
  jmap <.> (forM_ (Dhall.Map.toList map) $ \(k, v) -> put k v)
  return jmap

fromMap :: (Ord jk, Class jk, Class jv) =>
           LinkedHashMap jk jv -> Dhall.Map.Map jk jv 
fromMap jmap = unsafePerformJavaWith (superCast jmap) $ do
  (set :: Set (MapEntry jk jv)) <- jmapEntries
  entries <- flip mapM (fromJava set) $ \me -> do
    withObject me $ do
      (key :: jk) <- getKey
      (val :: jv) <- getValue
      return (key, val)
  return $ Dhall.Map.fromList entries
