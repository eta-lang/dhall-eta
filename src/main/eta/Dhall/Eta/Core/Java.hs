{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Dhall.Eta.Core.Java where

import qualified Crypto.Hash
import qualified Data.ByteArray as ByteArray (pack,unpack, Bytes)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import qualified Dhall.Core      as Dhall
import qualified Dhall.Set
import qualified Dhall.TypeCheck as Dhall
import qualified Java.Utils as Java (Enum)

import Dhall.Eta.Map ()
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import Eta.Types
import Java
import Java.Math

data JX = JX @java.lang.Void
  deriving (Class, Eq)

absurd :: forall a. JX -> a
absurd = undefined

instance JavaConverter Dhall.X JX where
  toJava = Dhall.absurd
  fromJava = absurd

-- Imports

data JDirectory = JDirectory @org.dhall.core.imports.Directory
  deriving (Class,Show,Eq)

foreign import java unsafe "@new" newDirectory  :: List JString -> JDirectory

foreign import java unsafe "getComponents" jdirComps  :: JDirectory -> List JString

instance JavaConverter Dhall.Directory JDirectory where
  toJava (Dhall.Directory comps) = newDirectory $ deepToJava comps
  fromJava jDir = Dhall.Directory $ deepFromJava $ jdirComps jDir 

data JFile = JFile @org.dhall.core.imports.File
  deriving (Class,Show,Eq)

foreign import java unsafe "@new" newFile  :: JDirectory -> JString -> JFile

foreign import java unsafe "getDirectory" jfileDir  :: JFile -> JDirectory

foreign import java unsafe "getFile" jfileFile  :: JFile -> JString

instance JavaConverter Dhall.File JFile where
  toJava (Dhall.File dir file) = newFile (toJava dir) (toJava file)
  fromJava jFile = Dhall.File (fromJava $ jfileDir jFile)
                              (fromJava $ jfileFile jFile)

data JFilePrefix = JFilePrefix @org.dhall.core.imports.FilePrefix
  deriving (Class,Show,Eq)

type instance Inherits JFilePrefix = '[Java.Enum JFilePrefix]

foreign import java unsafe
  "@static @field org.dhall.core.imports.FilePrefix.Absolute"
  filePrefixAbsolute :: JFilePrefix

foreign import java unsafe
  "@static @field org.dhall.core.imports.FilePrefix.Here"
  filePrefixHere :: JFilePrefix

foreign import java unsafe
  "@static @field org.dhall.core.imports.FilePrefix.Home"
  filePrefixHome :: JFilePrefix

instance JavaConverter Dhall.FilePrefix JFilePrefix where
  toJava Dhall.Absolute = filePrefixAbsolute
  toJava Dhall.Here     = filePrefixHere
  toJava Dhall.Home     = filePrefixHome
  
  fromJava jFilePre | jFilePre == filePrefixAbsolute = Dhall.Absolute
                    | jFilePre == filePrefixHere     = Dhall.Here
                    | jFilePre == filePrefixHome     = Dhall.Home
                    | otherwise                      = error $ "Unknown FilePrefix:" ++ show jFilePre                               
                    

data JScheme = JSCheme @org.dhall.core.imports.types.url.Scheme
  deriving (Class,Show,Eq)

type instance Inherits JScheme = '[Java.Enum JScheme]

foreign import java unsafe
  "@static @field org.dhall.core.imports.types.url.Scheme.HTTP"
  schemeHTTP :: JScheme

foreign import java unsafe
  "@static @field org.dhall.core.imports.types.url.Scheme.HTTPS"
  schemeHTTPS :: JScheme

instance JavaConverter Dhall.Scheme JScheme where
  toJava Dhall.HTTP = schemeHTTP
  toJava Dhall.HTTPS= schemeHTTPS
  
  fromJava jScheme | jScheme == schemeHTTP = Dhall.HTTP
                   | jScheme == schemeHTTPS= Dhall.HTTPS
                   | otherwise = error $ "Unknown Scheme: " ++ show jScheme
                   
data JURL = JURL @org.dhall.core.imports.types.URL
  deriving (Class,Show,Eq)

foreign import java unsafe "@new" newURL  :: JScheme -> JString -> JFile
                                          -> Optional JString -> Optional JString
                                          -> Optional JImportHashed -> JURL

foreign import java unsafe "getScheme" jURLScheme     :: JURL -> JScheme

foreign import java unsafe "getAuthority" jURLAuth    :: JURL -> JString

foreign import java unsafe "getPath" jURLPath         :: JURL -> JFile

foreign import java unsafe "getQuery" jURLQuery       :: JURL -> Optional JString

foreign import java unsafe "getFragment" jURLFragment :: JURL -> Optional JString

foreign import java unsafe "getHeaders" jURLHeaders    :: JURL -> Optional JImportHashed


instance JavaConverter Dhall.URL JURL where
  toJava (Dhall.URL {..}) =
    newURL (toJava scheme) (toJava authority) (toJava path)
           (deepToJava query) (deepToJava fragment) (deepToJava headers)
  fromJava jURL = Dhall.URL (fromJava $ jURLScheme jURL)
                            (fromJava $ jURLAuth jURL)
                            (fromJava $ jURLPath jURL)
                            (deepFromJava $ jURLQuery jURL)
                            (deepFromJava $ jURLFragment jURL)
                            (deepFromJava $ jURLHeaders jURL)                  

data JImportType = JImportType @org.dhall.core.imports.ImportType
  deriving (Class,Show,Eq)

data JImportTypeLocal = JImportTypeLocal @org.dhall.core.imports.types.Local
  deriving (Class,Show,Eq)

type instance Inherits JImportTypeLocal = '[JImportType]

foreign import java unsafe "@new" newImportTypeLocal  :: JFilePrefix -> JFile
                                                      -> JImportTypeLocal

foreign import java unsafe "getFilePrefix"
  jimportTypeLocalFilePrefix :: JImportTypeLocal -> JFilePrefix

foreign import java unsafe "getFile"
  jimportTypeLocalFile :: JImportTypeLocal -> JFile

data JImportTypeRemote = JImportTypeRemote @org.dhall.core.imports.types.Remote
  deriving (Class,Show,Eq)

type instance Inherits JImportTypeRemote = '[JImportType]

foreign import java unsafe "@new" newImportTypeRemote  :: JURL
                                                       -> JImportTypeRemote

foreign import java unsafe "getURL"
  jimportTypeRemoteURL :: JImportTypeRemote -> JURL

data JImportTypeEnv = JImportTypeEnv @org.dhall.core.imports.types.Env
  deriving (Class,Show,Eq)

type instance Inherits JImportTypeEnv = '[JImportType]

foreign import java unsafe "@new" newImportTypeEnv  :: JString
                                                    -> JImportTypeEnv

foreign import java unsafe "getVarName"
  jimportTypeEnvVarName :: JImportTypeEnv -> JString

data JImportTypeMissing = JImportTypeMissing @org.dhall.core.imports.types.Missing
  deriving (Class,Show,Eq)

type instance Inherits JImportTypeMissing = '[JImportType]

foreign import java unsafe "@new" newImportTypeMissing  :: JImportTypeMissing

instance JavaConverter Dhall.ImportType JImportType where
  toJava (Dhall.Local pre file) =
    superCast $ newImportTypeLocal (toJava pre) (toJava file)
  toJava (Dhall.Remote url) =
    superCast $ newImportTypeRemote (toJava url)
  toJava (Dhall.Env var) =
    superCast $ newImportTypeEnv (toJava var)
  toJava Dhall.Missing =
    superCast $ newImportTypeMissing

  fromJava jimportTy | jimportTy `instanceOf` getClass (Proxy :: Proxy JImportTypeLocal) =
                         Dhall.Local  (fromJava $ jimportTypeLocalFilePrefix
                                                $ unsafeCast jimportTy)
                                      (fromJava $ jimportTypeLocalFile
                                                $ unsafeCast jimportTy)
                     | jimportTy `instanceOf` getClass (Proxy :: Proxy JImportTypeRemote) =
                         Dhall.Remote (fromJava $ jimportTypeRemoteURL
                                                $ unsafeCast jimportTy)
                     | jimportTy `instanceOf` getClass (Proxy :: Proxy JImportTypeEnv) =
                         Dhall.Env    (fromJava $ jimportTypeEnvVarName
                                                $ unsafeCast jimportTy)
                     | jimportTy `instanceOf` getClass (Proxy :: Proxy JImportTypeMissing) =
                         Dhall.Missing
                     | otherwise =
                         error $ "Unknown subclass: " ++ show jimportTy

data JImportMode = JImportMode @org.dhall.core.imports.ImportMode
  deriving (Class,Show,Eq)

type instance Inherits JImportMode = '[Java.Enum JImportMode]

foreign import java unsafe
  "@static @field org.dhall.core.imports.ImportMode.Code"
  importModeCode :: JImportMode

foreign import java unsafe
  "@static @field org.dhall.core.imports.ImportMode.RawText"
  importModeRawText :: JImportMode

instance JavaConverter Dhall.ImportMode JImportMode where
  toJava Dhall.Code    = importModeCode
  toJava Dhall.RawText = importModeRawText
  
  fromJava jMode | jMode == importModeCode    = Dhall.Code
                 | jMode == importModeRawText = Dhall.RawText
                 | otherwise = error $ "Unknown ImportMode: " ++ show jMode

data JSHA256 = JSHA256 @org.dhall.core.imports.hashed.SHA256
  deriving (Class,Show,Eq)

data JDigest a = JDigest (@org.dhall.core.imports.hashed.Digest a)
  deriving (Class,Show,Eq)

foreign import java unsafe "@new" newDigest :: (a <: Object)
                                            => JByteArray -> JDigest a

foreign import java unsafe "getHash" jdigestHash :: (a <: Object)
                                                 => JDigest a -> JByteArray

instance JavaConverter (Crypto.Hash.Digest Crypto.Hash.SHA256) (JDigest JSHA256) where
  toJava d = newDigest $ toJava $ ByteArray.unpack d
  fromJava jDigest = fromJust $ Crypto.Hash.digestFromByteString byteArray
    where bytes = fromJava $ jdigestHash jDigest
          byteArray :: ByteArray.Bytes = ByteArray.pack bytes

data JImportHashed = JImportHashed @org.dhall.core.imports.ImportHashed
  deriving (Class,Show,Eq)

foreign import java unsafe "@new" newImportHashed :: Optional (JDigest JSHA256)
                                                  -> JImportType -> JImportHashed

foreign import java unsafe "getDigest" jimportHashedDigest :: JImportHashed
                                                           -> Optional (JDigest JSHA256)

foreign import java unsafe "getType" jimportHashedType :: JImportHashed
                                                       -> JImportType

instance JavaConverter Dhall.ImportHashed JImportHashed where
  toJava Dhall.ImportHashed{..} = newImportHashed (deepToJava hash) (toJava importType)
  fromJava jimport = Dhall.ImportHashed
                     { Dhall.hash = deepFromJava $ jimportHashedDigest jimport
                     , Dhall.importType = fromJava $ jimportHashedType jimport
                     }

data JImport = JImport @org.dhall.core.Import
  deriving (Class,Show,Eq)

foreign import java unsafe "@new" newImport :: JImportHashed -> JImportMode -> JImport

foreign import java unsafe "getHashed" jimportHashed :: JImport -> JImportHashed

foreign import java unsafe "getMode" jimportMode :: JImport -> JImportMode

instance JavaConverter Dhall.Import JImport where
  toJava Dhall.Import{..} = newImport (deepToJava importHashed) (toJava importMode)
  fromJava jimport = Dhall.Import
                     { Dhall.importHashed = deepFromJava $ jimportHashed jimport
                     , Dhall.importMode = fromJava $ jimportMode jimport
                     }


data JConst = JConst @org.dhall.core.Const
  deriving (Class,Show,Eq)

data JType = JType @org.dhall.core.constant.Type
  deriving (Class,Show,Eq)

type instance Inherits JType = '[JConst]

foreign import java unsafe "@new" newType  :: JType

data JKind = JKind @org.dhall.core.constant.Kind
  deriving (Class,Show,Eq)

type instance Inherits JKind = '[JConst]

foreign import java unsafe "@new" newKind  :: JKind

data JSort = JSort @org.dhall.core.constant.Sort
  deriving (Class,Show,Eq)

type instance Inherits JSort = '[JConst]

foreign import java unsafe "@new" newSort  :: JSort

instance JavaConverter Dhall.Const JConst where
  toJava Dhall.Type = superCast newType
  toJava Dhall.Kind = superCast newKind
  toJava Dhall.Sort = superCast newSort
  
  fromJava jconst | jconst `instanceOf` getClass (Proxy :: Proxy JType) =
                      Dhall.Type   
                  | jconst `instanceOf` getClass (Proxy :: Proxy JKind) =
                      Dhall.Kind
                  | jconst `instanceOf` getClass (Proxy :: Proxy JSort) =
                      Dhall.Sort
                  | otherwise =
                      error $ "Unknown subclass: " ++ show jconst


data JVar = JVar @org.dhall.core.Var
  deriving  (Class,Show,Eq)

foreign import java unsafe "@new" newVar  :: JString -> JInteger -> JVar

foreign import java unsafe "getName" jvarName  :: JVar -> JString

foreign import java unsafe "getIndex" jvarIndex  :: JVar -> JInteger

instance JavaConverter Dhall.Var JVar where
  toJava (Dhall.V name idx) = newVar (toJava name) (toJava idx)
  fromJava jvar = Dhall.V (fromJava $ jvarName jvar)
                          (fromJava $ jvarIndex jvar)

data JChunks s a = JChunks (@org.dhall.core.Chunks s a)
  deriving (Class,Show,Eq)

foreign import java unsafe "@new" newChunks  :: List (Pair JString (JExpr s a))
                                             -> JString -> JChunks s a

foreign import java unsafe "getInterpolations"
  chunkInts :: (a <: Object, s <: Object)
            => JChunks s a -> List (Pair JString (JExpr s a))

foreign import java unsafe "getText"
  chunkText :: (a <: Object, s <: Object) => JChunks s a -> JString

instance (Class sj, Class aj, JavaConverter s sj, JavaConverter a aj) =>
         JavaConverter (Dhall.Chunks s a) (JChunks sj aj) where
  toJava (Dhall.Chunks ints txt) = newChunks (deepToJava  ints) (toJava txt) 
  fromJava jchunks = Dhall.Chunks (deepFromJava $ chunkInts jchunks)
                                  (fromJava $ chunkText jchunks)


data JBinding s a = JBinding (@org.dhall.core.Binding s a)
  deriving (Class,Show,Eq)

foreign import java unsafe "@new"
  newBinding :: (s <: Object, a <: Object)
             => JString -> Optional ( JExpr s a )
             -> JExpr s a -> JBinding s a

foreign import java unsafe "getVariable"
  bindingVar :: (a <: Object, s <: Object) => JBinding s a -> JString

foreign import java unsafe "getAnnotation"
  bindingAnnot :: (a <: Object, s <: Object)
               => JBinding s a -> Optional (JExpr s a)

foreign import java unsafe "getValue"
  bindingVal :: (a <: Object, s <: Object)
             => JBinding s a -> JExpr s a

instance (Class sj, Class aj, JavaConverter s sj, JavaConverter a aj) =>
         JavaConverter (Dhall.Binding s a) (JBinding sj aj) where
  toJava (Dhall.Binding var annot val) =
    newBinding (toJava var) (deepToJava annot) (toJava val) 
  fromJava jbinding = Dhall.Binding
                        (fromJava $ bindingVar jbinding)
                        (deepFromJava $ bindingAnnot jbinding)
                        (fromJava $ bindingVal jbinding)

  
data JExpr s a = JExpr (@org.dhall.core.Expr s a)
  deriving (Class,Show,Eq)

data JExprBinary s a = JExprBinary (@org.dhall.core.expr.ExprBinary s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprBinary s a) = '[JExpr s a]

foreign import java unsafe "getLeft"
  jexprBinLeft  :: (a <: Object, s <: Object, t <: JExprBinary s a)
                => t -> JExpr s a

foreign import java unsafe "getRight"
  jexprBinRight :: (a <: Object, s <: Object, t <: JExprBinary s a)
                => t -> JExpr s a

data JExprUnary s a = JExprUnary (@org.dhall.core.expr.ExprUnary s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprUnary s a) = '[JExpr s a]

foreign import java unsafe "getSubexpr"
  jexprUnarySubexpr :: (a <: Object, s <: Object, t <: JExprUnary s a)
                    => t -> JExpr s a

data JExprConst s a = JExprConst (@org.dhall.core.expr.ExprConst s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprConst s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprConst  :: JConst -> JExprConst s a

foreign import java unsafe "getConstant" jexprConst  :: JExprConst s a -> JConst

data JExprVar s a = JExprVar (@org.dhall.core.expr.ExprVar s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprVar s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprVar  :: JVar -> JExprVar s a

foreign import java unsafe "getVar" jexprVar  :: JExprVar s a -> JVar

data JExprLam s a = JExprLam (@org.dhall.core.expr.ExprLam s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprLam s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprLam  :: JString -> JExpr s a -> JExpr s a
                                              -> JExprLam s a

foreign import java unsafe "getName" jexprLamName  :: JExprLam s a -> JString

foreign import java unsafe "getType" jexprLamType  :: JExprLam s a -> JExpr s a

foreign import java unsafe "getBody" jexprLamBody  :: JExprLam s a -> JExpr s a

data JExprPi s a = JExprPi (@org.dhall.core.expr.ExprPi s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprPi s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprPi  :: JString -> JExpr s a -> JExpr s a
                                             -> JExprPi s a

foreign import java unsafe "getName" jexprPiName  :: JExprPi s a -> JString

foreign import java unsafe "getKind" jexprPiKind  :: JExprPi s a -> JExpr s a

foreign import java unsafe "getBody" jexprPiBody  :: JExprPi s a -> JExpr s a

data JExprApp s a = JExprApp (@org.dhall.core.expr.ExprApp s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprApp s a) = '[JExprBinary s a]

foreign import java unsafe "@new" newExprApp  :: JExpr s a -> JExpr s a
                                              -> JExprApp s a

data JExprLet s a = JExprLet (@org.dhall.core.expr.ExprLet s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprLet s a) = '[JExpr s a]

foreign import java unsafe "@new"
  newExprLet  :: JNonEmptyArrayList (JBinding s a)
              -> JExpr s a -> JExprLet s a

foreign import java unsafe "getBindings"
  jexprLetBindings  :: JExprLet s a -> JNonEmptyArrayList (JBinding s a)

foreign import java unsafe "getBody"
  jexprLetBody  :: JExprLet s a -> JExpr s a

data JExprAnnot s a = JExprAnnot (@org.dhall.core.expr.ExprAnnot s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprAnnot s a) = '[JExprBinary s a]

foreign import java unsafe "@new" newExprAnnot  :: JExpr s a -> JExpr s a
                                                -> JExprAnnot s a

data JExprBool s a = JExprBool (@org.dhall.core.expr.ExprBool s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprBool s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprBool  :: JExprBool s a

data JExprBoolLit s a = JExprBoolLit (@org.dhall.core.expr.ExprBoolLit s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprBoolLit s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprBoolLit  :: JBoolean -> JExprBoolLit s a

foreign import java unsafe "getValue" jexprBoolLit  :: JExprBoolLit s a -> JBoolean

data JExprBoolAnd s a = JExprBoolAnd (@org.dhall.core.expr.ExprBoolAnd s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprBoolAnd s a) = '[JExprBinary s a]

foreign import java unsafe "@new" newExprBoolAnd  :: JExpr s a -> JExpr s a
                                                  -> JExprBoolAnd s a

data JExprBoolOr s a = JExprBoolOr (@org.dhall.core.expr.ExprBoolOr s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprBoolOr s a) = '[JExprBinary s a]

foreign import java unsafe "@new" newExprBoolOr  :: JExpr s a -> JExpr s a
                                                 -> JExprBoolOr s a

data JExprBoolEQ s a = JExprBoolEQ (@org.dhall.core.expr.ExprBoolEQ s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprBoolEQ s a) = '[JExprBinary s a]

foreign import java unsafe "@new" newExprBoolEQ  :: JExpr s a -> JExpr s a
                                                 -> JExprBoolEQ s a

data JExprBoolNE s a = JExprBoolNE (@org.dhall.core.expr.ExprBoolNE s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprBoolNE s a) = '[JExprBinary s a]

foreign import java unsafe "@new" newExprBoolNE :: JExpr s a -> JExpr s a
                                                -> JExprBoolNE s a

data JExprBoolIf s a = JExprBoolIf (@org.dhall.core.expr.ExprBoolIf s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprBoolIf s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprBoolIf  :: JExpr s a -> JExpr s a -> JExpr s a
                                                 -> JExprBoolIf s a

foreign import java unsafe "getTest" jexprBoolIfTest :: JExprBoolIf s a -> JExpr s a

foreign import java unsafe "getThen" jexprBoolIfThen :: JExprBoolIf s a -> JExpr s a

foreign import java unsafe "getElse" jexprBoolIfElse :: JExprBoolIf s a -> JExpr s a

data JExprNatural s a = JExprNatural (@org.dhall.core.expr.ExprNatural s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprNatural s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprNatural  :: JExprNatural s a

data JExprNaturalLit s a = JExprNaturalLit (@org.dhall.core.expr.ExprNaturalLit s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprNaturalLit s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprNaturalLit  :: JNatural -> JExprNaturalLit s a

foreign import java unsafe "getValue" jexprNaturalLit  :: JExprNaturalLit s a -> JNatural

data JExprNaturalFold s a = JExprNaturalFold (@org.dhall.core.expr.ExprNaturalFold s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprNaturalFold s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprNaturalFold  :: JExprNaturalFold s a

data JExprNaturalBuild s a = JExprNaturalBuild (@org.dhall.core.expr.ExprNaturalBuild s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprNaturalBuild s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprNaturalBuild  :: JExprNaturalBuild s a

data JExprNaturalIsZero s a = JExprNaturalIsZero (@org.dhall.core.expr.ExprNaturalIsZero s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprNaturalIsZero s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprNaturalIsZero  :: JExprNaturalIsZero s a

data JExprNaturalEven s a = JExprNaturalEven (@org.dhall.core.expr.ExprNaturalEven s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprNaturalEven s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprNaturalEven  :: JExprNaturalEven s a

data JExprNaturalOdd s a = JExprNaturalOdd (@org.dhall.core.expr.ExprNaturalOdd s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprNaturalOdd s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprNaturalOdd  :: JExprNaturalOdd s a

data JExprNaturalToInteger s a = JExprNaturalToInteger (@org.dhall.core.expr.ExprNaturalToInteger s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprNaturalToInteger s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprNaturalToInteger  :: JExprNaturalToInteger s a

data JExprNaturalShow s a = JExprNaturalShow (@org.dhall.core.expr.ExprNaturalShow s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprNaturalShow s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprNaturalShow  :: JExprNaturalShow s a

data JExprNaturalPlus s a = JExprNaturalPlus (@org.dhall.core.expr.ExprNaturalPlus s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprNaturalPlus s a) = '[JExprBinary s a]

foreign import java unsafe "@new" newExprNaturalPlus  :: JExpr s a -> JExpr s a
                                                 -> JExprNaturalPlus s a

data JExprNaturalTimes s a = JExprNaturalTimes (@org.dhall.core.expr.ExprNaturalTimes s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprNaturalTimes s a) = '[JExprBinary s a]

foreign import java unsafe "@new" newExprNaturalTimes  :: JExpr s a -> JExpr s a
                                                 -> JExprNaturalTimes s a

data JExprInteger s a = JExprInteger (@org.dhall.core.expr.ExprInteger s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprInteger s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprInteger  :: JExprInteger s a

data JExprIntegerLit s a = JExprIntegerLit (@org.dhall.core.expr.ExprIntegerLit s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprIntegerLit s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprIntegerLit  :: BigInteger -> JExprIntegerLit s a

foreign import java unsafe "getValue" jexprIntegerLit  :: JExprIntegerLit s a -> BigInteger

data JExprIntegerShow s a = JExprIntegerShow (@org.dhall.core.expr.ExprIntegerShow s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprIntegerShow s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprIntegerShow  :: JExprIntegerShow s a

data JExprIntegerToDouble s a = JExprIntegerToDouble (@org.dhall.core.expr.ExprIntegerToDouble s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprIntegerToDouble s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprIntegerToDouble  :: JExprIntegerToDouble s a

data JExprDouble s a = JExprDouble (@org.dhall.core.expr.ExprDouble s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprDouble s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprDouble  :: JExprDouble s a

data JExprDoubleLit s a = JExprDoubleLit (@org.dhall.core.expr.ExprDoubleLit s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprDoubleLit s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprDoubleLit  :: JDouble -> JExprDoubleLit s a

foreign import java unsafe "getValue" jexprDoubleLit  :: JExprDoubleLit s a -> JDouble

data JExprDoubleShow s a = JExprDoubleShow (@org.dhall.core.expr.ExprDoubleShow s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprDoubleShow s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprDoubleShow  :: JExprDoubleShow s a

data JExprText s a = JExprText (@org.dhall.core.expr.ExprText s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprText s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprText  :: JExprText s a

data JExprTextLit s a = JExprTextLit (@org.dhall.core.expr.ExprTextLit s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprTextLit s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprTextLit  :: JChunks s a -> JExprTextLit s a

foreign import java unsafe "getValue" jexprTextLit  :: JExprTextLit s a -> JChunks s a

data JExprTextAppend s a = JExprTextAppend (@org.dhall.core.expr.ExprTextAppend s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprTextAppend s a) = '[JExprBinary s a]

foreign import java unsafe "@new" newExprTextAppend  :: JExpr s a -> JExpr s a
                                                     -> JExprTextAppend s a

data JExprList s a = JExprList (@org.dhall.core.expr.ExprList s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprList s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprList  :: JExprList s a

data JExprListLit s a = JExprListLit (@org.dhall.core.expr.ExprListLit s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprListLit s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprListLit  :: Optional (JExpr s a) -> List (JExpr s a)
                                                  -> JExprListLit s a

foreign import java unsafe "getValue" jexprListLitValue  :: JExprListLit s a -> List (JExpr s a)

foreign import java unsafe "getType" jexprListLitType  :: JExprListLit s a -> Optional (JExpr s a)

data JExprListAppend s a = JExprListAppend (@org.dhall.core.expr.ExprListAppend s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprListAppend s a) = '[JExprBinary s a]

foreign import java unsafe "@new" newExprListAppend  :: JExpr s a -> JExpr s a
                                                     -> JExprListAppend s a

data JExprListBuild s a = JExprListBuild (@org.dhall.core.expr.ExprListBuild s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprListBuild s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprListBuild  :: JExprListBuild s a

data JExprListFold s a = JExprListFold (@org.dhall.core.expr.ExprListFold s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprListFold s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprListFold  :: JExprListFold s a

data JExprListLength s a = JExprListLength (@org.dhall.core.expr.ExprListLength s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprListLength s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprListLength  :: JExprListLength s a

data JExprListHead s a = JExprListHead (@org.dhall.core.expr.ExprListHead s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprListHead s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprListHead  :: JExprListHead s a

data JExprListLast s a = JExprListLast (@org.dhall.core.expr.ExprListLast s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprListLast s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprListLast  :: JExprListLast s a

data JExprListIndexed s a = JExprListIndexed (@org.dhall.core.expr.ExprListIndexed s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprListIndexed s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprListIndexed  :: JExprListIndexed s a

data JExprListReverse s a = JExprListReverse (@org.dhall.core.expr.ExprListReverse s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprListReverse s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprListReverse  :: JExprListReverse s a

data JExprOptional s a = JExprOptional (@org.dhall.core.expr.ExprOptional s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprOptional s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprOptional  :: JExprOptional s a

data JExprOptionalLit s a = JExprOptionalLit (@org.dhall.core.expr.ExprOptionalLit s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprOptionalLit s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprOptionalLit  :: JExpr s a
                                                      -> Optional (JExpr s a)
                                                      -> JExprOptionalLit s a

foreign import java unsafe "getType" jexprOptLitType :: JExprOptionalLit s a -> JExpr s a

foreign import java unsafe "getValue" jexprOptLitValue :: JExprOptionalLit s a
                                                       -> Optional (JExpr s a)

data JExprSome s a = JExprSome (@org.dhall.core.expr.ExprSome s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprSome s a) = '[JExprUnary s a]

foreign import java unsafe "@new" newExprSome :: JExpr s a -> JExprSome s a

data JExprNone s a = JExprNone (@org.dhall.core.expr.ExprNone s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprNone s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprNone :: JExprNone s a

data JExprOptionalBuild s a = JExprOptionalBuild (@org.dhall.core.expr.ExprOptionalBuild s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprOptionalBuild s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprOptionalBuild  :: JExprOptionalBuild s a

data JExprOptionalFold s a = JExprOptionalFold (@org.dhall.core.expr.ExprOptionalFold s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprOptionalFold s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprOptionalFold  :: JExprOptionalFold s a

data JExprRecord s a = JExprRecord (@org.dhall.core.expr.ExprRecord s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprRecord s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprRecord  :: LinkedHashMap JString (JExpr s a)
                                                 -> JExprRecord s a

foreign import java unsafe "getType" jexprRecordType :: JExprRecord s a
                                                     -> LinkedHashMap JString (JExpr s a)

data JExprRecordLit s a = JExprRecordLit (@org.dhall.core.expr.ExprRecordLit s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprRecordLit s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprRecordLit  :: LinkedHashMap JString (JExpr s a)
                                                    -> JExprRecordLit s a

foreign import java unsafe "getValue" jexprRecordLitValue :: JExprRecordLit s a
                                                          -> LinkedHashMap JString (JExpr s a)

data JExprUnion s a = JExprUnion (@org.dhall.core.expr.ExprUnion s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprUnion s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprUnion  :: LinkedHashMap JString (JExpr s a)
                                                -> JExprUnion s a

foreign import java unsafe "getType" jexprUnionType :: JExprUnion s a
                                                    -> LinkedHashMap JString (JExpr s a)

data JExprUnionLit s a = JExprUnionLit (@org.dhall.core.expr.ExprUnionLit s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprUnionLit s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprUnionLit  :: JString -> JExpr s a
                                                   -> LinkedHashMap JString (JExpr s a)
                                                   -> JExprUnionLit s a

foreign import java unsafe "getName" jexprUnionLitName :: JExprUnionLit s a
                                                       -> JString

foreign import java unsafe "getValue" jexprUnionLitValue :: JExprUnionLit s a
                                                         -> JExpr s a

foreign import java unsafe "getCases" jexprUnionLitCases :: JExprUnionLit s a
                                                         -> LinkedHashMap JString (JExpr s a)

data JExprCombine s a = JExprCombine (@org.dhall.core.expr.ExprCombine s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprCombine s a) = '[JExprBinary s a]

foreign import java unsafe "@new" newExprCombine  :: JExpr s a -> JExpr s a
                                                   -> JExprCombine s a

data JExprCombineTypes s a = JExprCombineTypes (@org.dhall.core.expr.ExprCombineTypes s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprCombineTypes s a) = '[JExprBinary s a]

foreign import java unsafe "@new" newExprCombineTypes  :: JExpr s a -> JExpr s a
                                                       -> JExprCombineTypes s a

data JExprPrefer s a = JExprPrefer (@org.dhall.core.expr.ExprPrefer s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprPrefer s a) = '[JExprBinary s a]

foreign import java unsafe "@new" newExprPrefer  :: JExpr s a -> JExpr s a
                                                 -> JExprPrefer s a

data JExprMerge s a = JExprMerge (@org.dhall.core.expr.ExprMerge s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprMerge s a) = '[JExprBinary s a]

foreign import java unsafe "@new" newExprMerge  :: JExpr s a -> JExpr s a
                                                -> Optional (JExpr s a)
                                                -> JExprMerge s a

foreign import java unsafe "getType" jexprMergeType  :: JExprMerge s a
                                                     -> Optional (JExpr s a)

data JExprConstructors s a = JExprConstructors (@org.dhall.core.expr.ExprConstructors s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprConstructors s a) = '[JExprUnary s a]

foreign import java unsafe "@new" newExprConstructors :: JExpr s a -> JExprConstructors s a

data JExprField s a = JExprField (@org.dhall.core.expr.ExprField s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprField s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprField  :: JExpr s a -> JString 
                                                -> JExprField s a

foreign import java unsafe "getKey" jexprFieldKey  :: JExprField s a -> JString

foreign import java unsafe "getRecord" jexprFieldRecord  :: JExprField s a -> JExpr s a

data JExprProject s a = JExprProject (@org.dhall.core.expr.ExprProject s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprProject s a) = '[JExpr s a]

foreign import java unsafe "@new" newExprProject  :: JExpr s a -> Set JString
                                                  -> JExprProject s a

foreign import java unsafe "getKeys" jexprProjectKeys  :: JExprProject s a -> Set JString

foreign import java unsafe "getRecord" jexprProjectRecord  :: JExprProject s a -> JExpr s a

data JExprNote s a = JExprNote (@org.dhall.core.expr.ExprNote s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprNote s a) = '[JExprUnary s a]

foreign import java unsafe "@new" newExprNote :: (s <: Object) 
                                              => s -> JExpr s a
                                              -> JExprNote s a

foreign import java unsafe "getAnnotation" jexprNoteAnnot  ::
  (s <: Object, a <: Object) => JExprNote s a -> s

data JExprImportAlt s a = JExprImportAlt (@org.dhall.core.expr.ExprImportAlt s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprImportAlt s a) = '[JExprBinary s a]

foreign import java unsafe "@new" newExprImportAlt  :: JExpr s a -> JExpr s a
                                                    -> JExprImportAlt s a

data JExprEmbed s a = JExprEmbed (@org.dhall.core.expr.ExprEmbed s a)
  deriving (Class,Show,Eq)

type instance Inherits (JExprEmbed s a) = '[JExpr s a]

foreign import java unsafe "@new"
  newExprEmbed :: (a <: Object, s <: Object) => a -> JExprEmbed s a

foreign import java unsafe "getEmbedded"
  jexprEmbedded :: (a <: Object, s <: Object) => JExprEmbed s a -> a

instance (Class js, Class ja, JavaConverter s js, JavaConverter a ja) =>
         JavaConverter (Dhall.Expr s a) (JExpr js ja) where

  toJava (Dhall.Const const)=
    superCast $ newExprConst (toJava const)
  toJava (Dhall.Var var)=
    superCast $ newExprVar (toJava var)
  toJava (Dhall.Lam n ty body)=
    superCast $ newExprLam (toJava n) (toJava ty) (toJava body)
  toJava (Dhall.Pi n kind body)=
    superCast $ newExprPi (toJava n) (toJava kind) (toJava body)
  toJava (Dhall.App n val) =
    superCast $ newExprApp (toJava n) (toJava val)
  toJava (Dhall.Let bindings body) =
    superCast $ newExprLet (deepToJava bindings) (toJava body)
  toJava (Dhall.Annot n val) =
    superCast $ newExprAnnot (toJava n) (toJava val)
  toJava Dhall.Bool =
    superCast $ newExprBool
  toJava (Dhall.BoolLit bool) =
    superCast $ newExprBoolLit $ toJava bool
  toJava (Dhall.BoolAnd l r) =
    superCast $ newExprBoolAnd (toJava l) (toJava r)
  toJava (Dhall.BoolOr l r) =
    superCast $ newExprBoolOr (toJava l) (toJava r)
  toJava (Dhall.BoolEQ l r) =
    superCast $ newExprBoolEQ (toJava l) (toJava r)
  toJava (Dhall.BoolNE l r) =
    superCast $ newExprBoolNE (toJava l) (toJava r)
  toJava (Dhall.BoolIf test then' else') =
    superCast $ newExprBoolIf (toJava test) (toJava then') (toJava else')
  toJava Dhall.Natural =
    superCast $ newExprNatural
  toJava (Dhall.NaturalLit nat) =
    superCast $ newExprNaturalLit $ toJava nat
  toJava Dhall.NaturalFold =
    superCast $ newExprNaturalFold
  toJava Dhall.NaturalBuild =
    superCast $ newExprNaturalBuild
  toJava Dhall.NaturalIsZero =
    superCast $ newExprNaturalIsZero
  toJava Dhall.NaturalEven =
    superCast $ newExprNaturalEven
  toJava Dhall.NaturalOdd =
    superCast $ newExprNaturalOdd
  toJava Dhall.NaturalToInteger =
    superCast $ newExprNaturalToInteger
  toJava Dhall.NaturalShow =
    superCast $ newExprNaturalShow
  toJava (Dhall.NaturalPlus l r) =
    superCast $ newExprNaturalPlus (toJava l) (toJava r)
  toJava (Dhall.NaturalTimes l r) =
    superCast $ newExprNaturalTimes (toJava l) (toJava r)
  toJava Dhall.Integer =
    superCast $ newExprInteger
  toJava (Dhall.IntegerLit nat) =
    superCast $ newExprIntegerLit $ toJava nat
  toJava Dhall.IntegerShow =
    superCast $ newExprIntegerShow
  toJava Dhall.IntegerToDouble =
    superCast $ newExprIntegerToDouble
  toJava Dhall.Double =
    superCast $ newExprDouble
  toJava (Dhall.DoubleLit n) =
    superCast $ newExprDoubleLit $ toJava n
  toJava Dhall.DoubleShow =
    superCast $ newExprDoubleShow
  toJava Dhall.Text =
    superCast $ newExprText
  toJava (Dhall.TextLit chunks) =
    superCast $ newExprTextLit (toJava chunks)
  toJava (Dhall.TextAppend l r) =
    superCast $ newExprTextAppend (toJava l) (toJava r)
  toJava Dhall.List =
    superCast $ newExprList
  toJava (Dhall.ListLit ty exprs) =
    superCast $ newExprListLit (deepToJava ty) (deepToJava $ toList $ exprs)
  toJava (Dhall.ListAppend l r) =
    superCast $ newExprListAppend (toJava l) (toJava r)
  toJava Dhall.ListBuild =
    superCast $ newExprListBuild
  toJava Dhall.ListFold =
    superCast $ newExprListFold
  toJava Dhall.ListLength =
    superCast $ newExprListLength
  toJava Dhall.ListHead =
    superCast $ newExprListHead
  toJava Dhall.ListLast =
    superCast $ newExprListLast
  toJava Dhall.ListIndexed =
    superCast $ newExprListIndexed
  toJava Dhall.ListReverse =
    superCast $ newExprListReverse
  toJava Dhall.Optional =
    superCast $ newExprOptional
  toJava (Dhall.OptionalLit ty val) =
    superCast $ newExprOptionalLit (toJava ty) (deepToJava val)
  toJava (Dhall.Some expr) =
    superCast $ newExprSome (toJava expr)
  toJava Dhall.None =
    superCast newExprNone
  toJava Dhall.OptionalBuild =
    superCast $ newExprOptionalBuild
  toJava Dhall.OptionalFold =
    superCast $ newExprOptionalFold
  toJava (Dhall.Record ty) =
    superCast $ newExprRecord (deepToJava ty)
  toJava (Dhall.RecordLit val) =
    superCast $ newExprRecordLit (deepToJava val)
  toJava (Dhall.Union ty) =
    superCast $ newExprUnion (deepToJava ty)
  toJava (Dhall.UnionLit name val cases) =
    superCast $ newExprUnionLit (toJava name) (toJava val) (deepToJava cases)
  toJava (Dhall.Combine l r) =
    superCast $ newExprCombine (toJava l) (toJava r)
  toJava (Dhall.CombineTypes l r) =
    superCast $ newExprCombineTypes (toJava l) (toJava r)
  toJava (Dhall.Prefer l r) =
    superCast $ newExprPrefer (toJava l) (toJava r)
  toJava (Dhall.Merge l r ty) =
    superCast $ newExprMerge (toJava l) (toJava r) (deepToJava ty)
  toJava (Dhall.Constructors expr) =
    superCast $ newExprConstructors (toJava expr)
  toJava (Dhall.Field record key)=
    superCast $ newExprField (toJava record) (toJava key)
  toJava (Dhall.Project record keys)=
    superCast $ newExprProject (toJava record) (deepToJava $ Dhall.Set.toSet keys)
  toJava (Dhall.Note annot subexpr)=
    superCast $ newExprNote (toJava annot) (toJava subexpr)
  toJava (Dhall.ImportAlt l r)=
    superCast $ newExprImportAlt (toJava l) (toJava r)
  toJava (Dhall.Embed embed)=
    superCast $ newExprEmbed (toJava embed)
    
  fromJava jexpr
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprConst js ja)) =
        Dhall.Const   (fromJava $ jexprConst $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprVar js ja)) =
        Dhall.Var     (fromJava $ jexprVar $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprLam js ja)) =
        Dhall.Lam     (fromJava $ jexprLamName $ unsafeCast jexpr)
                      (fromJava $ jexprLamType $ unsafeCast jexpr)
                      (fromJava $ jexprLamBody $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprPi js ja)) =
        Dhall.Pi      (fromJava $ jexprPiName  $ unsafeCast jexpr)
                      (fromJava $ jexprPiKind  $ unsafeCast jexpr)
                      (fromJava $ jexprPiBody  $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprApp js ja)) =
        Dhall.App     (fromJava $ left jexpr)
                      (fromJava $ right jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprLet js ja)) =
        Dhall.Let     (deepFromJava $ jexprLetBindings $ unsafeCast jexpr)
                      (fromJava $ jexprLetBody $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprAnnot js ja)) =
        Dhall.Annot   (fromJava $ left jexpr)
                      (fromJava $ right jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprBool js ja)) =
        Dhall.Bool
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprBoolLit js ja)) =
        Dhall.BoolLit (fromJava $ jexprBoolLit $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprBoolAnd js ja)) =
        Dhall.BoolAnd (fromJava $ left jexpr)
                      (fromJava $ right jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprBoolOr  js ja)) =
        Dhall.BoolOr  (fromJava $ left jexpr)
                      (fromJava $ right jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprBoolEQ  js ja)) =
        Dhall.BoolEQ  (fromJava $ left jexpr)
                      (fromJava $ right jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprBoolNE js ja)) =
        Dhall.BoolNE  (fromJava $ left jexpr)
                      (fromJava $ right jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprBoolIf js ja)) =
        Dhall.BoolIf  (fromJava $ jexprBoolIfTest $ unsafeCast jexpr)
                      (fromJava $ jexprBoolIfThen $ unsafeCast jexpr)
                      (fromJava $ jexprBoolIfElse $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprNatural js ja)) =
        Dhall.Natural
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprNaturalLit js ja)) =
        Dhall.NaturalLit (fromJava $ jexprNaturalLit $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprNaturalFold js ja)) =
        Dhall.NaturalFold
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprNaturalBuild js ja)) =
        Dhall.NaturalBuild
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprNaturalIsZero js ja)) =
        Dhall.NaturalIsZero
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprNaturalEven js ja)) =
        Dhall.NaturalEven
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprNaturalOdd js ja)) =
        Dhall.NaturalOdd
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprNaturalToInteger js ja)) =
        Dhall.NaturalToInteger
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprNaturalShow js ja)) =
        Dhall.NaturalShow
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprNaturalPlus  js ja)) =
        Dhall.NaturalPlus (fromJava $ left jexpr)
                          (fromJava $ right jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprNaturalTimes  js ja)) =
        Dhall.NaturalTimes (fromJava $ left jexpr)
                           (fromJava $ right jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprInteger js ja)) =
        Dhall.Integer
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprIntegerLit js ja)) =
        Dhall.IntegerLit (fromJava $ jexprIntegerLit $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprIntegerShow js ja)) =
        Dhall.IntegerShow
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprIntegerToDouble js ja)) =
        Dhall.IntegerToDouble
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprDouble js ja)) =
        Dhall.Double
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprDoubleLit js ja)) =
        Dhall.DoubleLit (fromJava $ jexprDoubleLit $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprDoubleShow js ja)) =
        Dhall.DoubleShow
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprText js ja)) =
        Dhall.Text
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprTextLit js ja)) =
        Dhall.TextLit (fromJava $ jexprTextLit $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprTextAppend  js ja)) =
        Dhall.TextAppend (fromJava $ left jexpr)
                         (fromJava $ right jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprList js ja)) =
        Dhall.List
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprListLit js ja)) =
        Dhall.ListLit (deepFromJava $ jexprListLitType $ unsafeCast jexpr)
                      (Seq.fromList $ deepFromJava $ jexprListLitValue $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprListAppend  js ja)) =
        Dhall.ListAppend (fromJava $ left jexpr)
                         (fromJava $ right jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprListBuild js ja)) =
        Dhall.ListBuild
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprListFold js ja)) =
        Dhall.ListFold
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprListLength js ja)) =
        Dhall.ListLength
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprListHead js ja)) =
        Dhall.ListHead
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprListLast js ja)) =
        Dhall.ListLast
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprListIndexed js ja)) =
        Dhall.ListIndexed
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprListReverse js ja)) =
        Dhall.ListReverse
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprOptional js ja)) =
        Dhall.Optional
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprOptionalLit js ja)) =
        Dhall.OptionalLit (fromJava $ jexprOptLitType $ unsafeCast jexpr)
                          (deepFromJava $ jexprOptLitValue $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprSome js ja)) =
        Dhall.Some (fromJava $ subexpr jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprNone js ja)) =
        Dhall.None
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprOptionalBuild js ja)) =
        Dhall.OptionalBuild
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprOptionalFold js ja)) =
        Dhall.OptionalFold
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprRecord js ja)) =
        Dhall.Record (deepFromJava $ jexprRecordType $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprRecordLit js ja)) =
        Dhall.RecordLit (deepFromJava $ jexprRecordLitValue $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprUnion js ja)) =
        Dhall.Union (deepFromJava $ jexprUnionType $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprUnionLit js ja)) =
        Dhall.UnionLit (fromJava $ jexprUnionLitName $ unsafeCast jexpr)
                       (fromJava $ jexprUnionLitValue $ unsafeCast jexpr)
                       (deepFromJava $ jexprUnionLitCases $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprCombine js ja)) =
        Dhall.Combine (fromJava $ left jexpr)
                      (fromJava $ right jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprCombineTypes js ja)) =
        Dhall.CombineTypes (fromJava $ left jexpr)
                           (fromJava $ right jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprPrefer js ja)) =
        Dhall.Prefer (fromJava $ left jexpr)
                     (fromJava $ right jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprMerge js ja)) =
        Dhall.Merge (fromJava $ left jexpr)
                    (fromJava $ right jexpr)
                    (deepFromJava $ jexprMergeType $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprConstructors js ja)) =
        Dhall.Constructors (fromJava $ subexpr jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprField js ja)) =
        Dhall.Field (fromJava $ jexprFieldRecord $ unsafeCast jexpr)
                    (fromJava $ jexprFieldKey $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprProject js ja)) =
        Dhall.Project (fromJava $ jexprProjectRecord $ unsafeCast jexpr)
                      (Dhall.Set.fromList $ Set.toList
                                          $ deepFromJava $ jexprProjectKeys $ unsafeCast jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprNote js ja)) =
        Dhall.Note (fromJava $ jexprNoteAnnot $ unsafeCast jexpr)
                   (fromJava $ subexpr jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprImportAlt js ja)) =
        Dhall.ImportAlt (fromJava $ left jexpr)
                        (fromJava $ right jexpr)
    | jexpr `instanceOf` getClass (Proxy :: Proxy (JExprEmbed js ja)) =
        Dhall.Embed (fromJava $ jexprEmbedded $ unsafeCast jexpr)
    | otherwise =
        error $ "Unknown subclass: " ++ show jexpr

    where left :: JExpr js ja -> JExpr js ja
          left jexpr = jexprBinLeft $  ((unsafeCast jexpr) :: JExprBinary js ja)

          right :: JExpr js ja -> JExpr js ja
          right jexpr = jexprBinRight $ ((unsafeCast jexpr) :: JExprBinary js ja)
          
          subexpr :: JExpr js ja -> JExpr js ja
          subexpr jexpr = jexprUnarySubexpr $ ((unsafeCast jexpr) :: JExprUnary js ja)
