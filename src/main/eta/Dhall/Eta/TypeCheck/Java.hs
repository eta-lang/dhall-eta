{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Dhall.Eta.TypeCheck.Java where 

import qualified Dhall.TypeCheck as Dhall

import Dhall.Eta.Context
import Dhall.Eta.Core
import Eta.Types
import Java

data JTypeMessage s a = JTypeMessage (@org.dhall.typecheck.TypeMessage s a)
  deriving (Class,Show,Eq)

data JAnnotationMessage s a =
  JAnnotationMessage (@org.dhall.typecheck.message.AnnotationMessage s a)
  deriving (Class,Show,Eq)

type instance Inherits (JAnnotationMessage s a) = '[JTypeMessage s a]

foreign import java unsafe "getAnnotation"
  jtypeMsgAnnot  :: (t <: JAnnotationMessage s a) => t -> JExpr s a

data JAnnotatedExpressionMessage s a =
  JAnnotatedExpressionMessage
    (@org.dhall.core.typecheck.AnnotatedExpressionMessage s a)
  deriving (Class,Show,Eq)

type instance Inherits (JAnnotatedExpressionMessage s a) = '[JAnnotationMessage s a]

foreign import java unsafe "getExpression"
  jtypeMsgExpr  :: (t <: JAnnotatedExpressionMessage s a) => t -> JExpr s a

data JNameMessage s a = JNameMessage (@org.dhall.core.typecheck.NameMessage s a)
  deriving (Class,Show,Eq)

type instance Inherits (JNameMessage s a) = '[JTypeMessage s a]

foreign import java unsafe "getName"
  jtypeMsgName  :: (t <: JNameMessage s a) => t -> JString

data JAnnotatedNameMessage s a =
  JAnnotatedExpressionNameMessage
    (@org.dhall.typecheck.message.AnnotatedExpressionNameMessage s a)
  deriving (Class,Show,Eq)

type instance Inherits (JAnnotatedNameMessage s a) = '[JAnnotationMessage s a]

foreign import java unsafe "getName"
  jtypeMsgAnnName  :: (t <: JAnnotatedNameMessage s a) => t -> JString

data JAnnotatedExpressionMismatchMessage s a =
  JAnnotatedExpressionMismatchMessage
    (@org.dhall.typecheck.message.AnnotatedExpressionMismatchMessage s a)
  deriving (Class,Show,Eq)

type instance Inherits (JAnnotatedExpressionMismatchMessage s a) =
  '[JAnnotatedExpressionMessage s a]

foreign import java unsafe "getExpectedAnnotation"
  jtypeMsgExpectedAnnot  :: (t <: JAnnotatedExpressionMismatchMessage s a)
                         => t -> JExpr s a

data JAnnotMismatch s a = JAnnotMismatch (@org.dhall.typecheck.message.AnnotMismatch s a)
  deriving (Class,Show,Eq)

type instance Inherits (JAnnotMismatch s a) = '[JAnnotatedExpressionMismatchMessage s a]

foreign import java unsafe "@new"
  newAnnotMismatch  :: JExpr s a -> JExpr s a -> JExpr s a
                    -> JAnnotMismatch s a

data JCantAccess s a =
  JCantAccess (@org.dhall.typecheck.message.CantAccess s a)
  deriving (Class,Show,Eq)

type instance Inherits (JCantAccess s a) = '[JAnnotatedNameMessage s a]

foreign import java unsafe "@new"
  newCantAccess  :: JString -> JExpr s a -> JExpr s a
                 -> JCantAccess s a

foreign import java unsafe "getExpectedType"
  jcantAccessExpectedType :: (t <: JCantAccess s a)
                          => t -> JExpr s a


data JCantAdd s a = JCantAdd (@org.dhall.typecheck.message.CantAdd s a)
  deriving (Class,Show,Eq)

type instance Inherits (JCantAdd s a) = '[JAnnotatedExpressionMessage s a]

foreign import java unsafe "@new"
  newCantAdd  :: JExpr s a -> JExpr s a -> JCantAdd s a

data JCantAnd s a = JCantAnd (@org.dhall.typecheck.message.CantAnd s a)
  deriving (Class,Show,Eq)

type instance Inherits (JCantAnd s a) = '[JAnnotatedExpressionMessage s a]

foreign import java unsafe "@new"
  newCantAnd  :: JExpr s a -> JExpr s a -> JCantAnd s a

data JCantEQ s a = JCantEQ (@org.dhall.typecheck.message.CantEQ s a)
  deriving (Class,Show,Eq)

type instance Inherits (JCantEQ s a) = '[JAnnotatedExpressionMessage s a]

foreign import java unsafe "@new"
  newCantEQ  :: JExpr s a -> JExpr s a -> JCantEQ s a

data JCantInterpolate s a =
  JCantInterpolate (@org.dhall.typecheck.message.CantInterpolate s a)
  deriving (Class,Show,Eq)

type instance Inherits (JCantInterpolate s a) = '[JAnnotatedExpressionMessage s a]

foreign import java unsafe "@new"
  newCantInterpolate  :: JExpr s a -> JExpr s a -> JCantInterpolate s a
data JCantListAppend s a =
  JCantListAppend (@org.dhall.typecheck.message.CantListAppend s a)
  deriving (Class,Show,Eq)

type instance Inherits (JCantListAppend s a) = '[JAnnotatedExpressionMessage s a]

foreign import java unsafe "@new"
  newCantListAppend  :: JExpr s a -> JExpr s a -> JCantListAppend s a

data JCantMultiply s a =
  JCantMultiply (@org.dhall.typecheck.message.CantMultiply s a)
  deriving (Class,Show,Eq)

type instance Inherits (JCantMultiply s a) = '[JAnnotatedExpressionMessage s a]

foreign import java unsafe "@new"
  newCantMultiply  :: JExpr s a -> JExpr s a -> JCantMultiply s a

data JCantNE s a = JCantNE (@org.dhall.typecheck.message.CantNE s a)
  deriving (Class,Show,Eq)

type instance Inherits (JCantNE s a) = '[JAnnotatedExpressionMessage s a]

foreign import java unsafe "@new"
  newCantNE :: JExpr s a -> JExpr s a -> JCantNE s a

data JCantOr s a = JCantOr (@org.dhall.typecheck.message.CantOr s a)
  deriving (Class,Show,Eq)

type instance Inherits (JCantOr s a) = '[JAnnotatedExpressionMessage s a]

foreign import java unsafe "@new"
  newCantOr :: JExpr s a -> JExpr s a -> JCantOr s a

data JCantProject s a =
  JCantProject (@org.dhall.typecheck.message.CantProject s a)
  deriving (Class,Show,Eq)

type instance Inherits (JCantProject s a) = '[JAnnotatedNameMessage s a]

foreign import java unsafe "@new"
  newCantProject :: JString -> JExpr s a -> JExpr s a
                 -> JCantProject s a

foreign import java unsafe "getExpectedType"
  jcantProjectExpectedType :: (t <: JCantProject s a)
                           => t -> JExpr s a

data JCantTextAppend s a =
  JCantTextAppend (@org.dhall.typecheck.message.CantTextAppend s a)
  deriving (Class,Show,Eq)

type instance Inherits (JCantTextAppend s a) = '[JAnnotatedExpressionMessage s a]

foreign import java unsafe "@new"
  newCantTextAppend :: JExpr s a -> JExpr s a -> JCantOr s a

data JCombineTypesRequiresRecordType s a =
  JCombineTypesRequiresRecordType
    (@org.dhall.typecheck.message.CombineTypesRequiresRecordType s a)
  deriving (Class,Show,Eq)

type instance Inherits (JCombineTypesRequiresRecordType s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newCombineTypesRequiresRecordType :: JExpr s a -> JExpr s a
                                    -> JCombineTypesRequiresRecordType s a

foreign import java unsafe "getArgument"
  jcombineTypesRequiresRecordTypeArg  :: (t <: JCombineTypesRequiresRecordType s a)
                                      => t -> JExpr s a

foreign import java unsafe "getNormalized"
  jcombineTypesRequiresRecordTypeNorm :: (t <: JCombineTypesRequiresRecordType s a)
                                      => t -> JExpr s a

data JConstructorsRequiresAUnionType s a =
    JConstructorsRequiresAUnionType
      (@org.dhall.typecheck.message.ConstructorsRequiresAUnionType s a)
  deriving (Class,Show,Eq)

type instance Inherits (JConstructorsRequiresAUnionType s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newConstructorsRequiresAUnionType :: JExpr s a -> JExpr s a
                                    -> JConstructorsRequiresAUnionType s a

foreign import java unsafe "getArgument"
  jconstructorsRequiresAUnionTypeArg  :: (t <: JConstructorsRequiresAUnionType s a)
                                      => t -> JExpr s a

foreign import java unsafe "getNormalized"
  jconstructorsRequiresAUnionTypeNorm :: (t <: JConstructorsRequiresAUnionType s a)
                                      => t -> JExpr s a

data JDuplicateAlternative s a =
  JDuplicateAlternative (@org.dhall.typecheck.message.DuplicateAlternative s a)
  deriving (Class,Show,Eq)

type instance Inherits (JDuplicateAlternative s a) = '[JNameMessage s a]

foreign import java unsafe "@new"
  newDuplicateAlternative :: JString -> JDuplicateAlternative s a

data JFieldAnnotationMismatch s a =
    JFieldAnnotationMismatch (@org.dhall.typecheck.message.FieldAnnotationMismatch s a)
  deriving (Class,Show,Eq)

type instance Inherits (JFieldAnnotationMismatch s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newFieldAnnotationMismatch :: JString -> JExpr s a -> JConst
                             -> JString -> JExpr s a -> JConst
                             -> JFieldAnnotationMismatch s a

foreign import java unsafe "getField1Name"
  jfieldAnnotationMismatchField1Name  :: (t <: JFieldAnnotationMismatch s a)
                                      => t -> JString

foreign import java unsafe "getField1Annotation"
  jfieldAnnotationMismatchField1Annot :: (t <: JFieldAnnotationMismatch s a)
                                      => t -> JExpr s a

foreign import java unsafe "getField1Constant"
  jfieldAnnotationMismatchField1Const :: (t <: JFieldAnnotationMismatch s a)
                                      => t -> JConst

foreign import java unsafe "getField2Name"
  jfieldAnnotationMismatchField2Name :: (t <: JFieldAnnotationMismatch s a)
                                     => t -> JString

foreign import java unsafe "getField2Annotation"
  jfieldAnnotationMismatchField2Annot :: (t <: JFieldAnnotationMismatch s a)
                                      => t -> JExpr s a

foreign import java unsafe "getField2Constant"
  jfieldAnnotationMismatchField2Const :: (t <: JFieldAnnotationMismatch s a)
                                      => t -> JConst

data JFieldCollision s a =
  JFieldCollision (@org.dhall.typecheck.message.FieldCollision s a)
  deriving (Class,Show,Eq)

type instance Inherits (JFieldCollision s a) = '[JNameMessage s a]

foreign import java unsafe "@new"
  newFieldCollision :: JString -> JFieldCollision s a

data JFieldMismatch s a =
    JFieldMismatch (@org.dhall.typecheck.message.FieldMismatch s a)
  deriving (Class,Show,Eq)

type instance Inherits (JFieldMismatch s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newFieldMismatch :: JString -> JExpr s a -> JConst 
                   -> JString -> JExpr s a -> JConst
                   -> JFieldMismatch s a

foreign import java unsafe "getField1Name"
  jfieldMismatchField1Name :: (t <: JFieldMismatch s a)
                           => t -> JString

foreign import java unsafe "getField1Value"
  jfieldMismatchField1Value :: (t <: JFieldMismatch s a)
                            => t -> JExpr s a

foreign import java unsafe "getField1Constant"
  jfieldMismatchField1Const :: (t <: JFieldMismatch s a)
                            => t -> JConst

foreign import java unsafe "getField2Name"
  jfieldMismatchField2Name :: (t <: JFieldMismatch s a)
                           => t -> JString

foreign import java unsafe "getField2Value"
  jfieldMismatchField2Value :: (t <: JFieldMismatch s a)
                            => t -> JExpr s a

foreign import java unsafe "getField2Constant"
  jfieldMismatchField2Const :: (t <: JFieldMismatch s a)
                            => t -> JConst

data JHandlerInputTypeMismatch s a =
  JHandlerInputTypeMismatch (@org.dhall.typecheck.message.HandlerInputTypeMismatch s a)
  deriving (Class,Show,Eq)

type instance Inherits (JHandlerInputTypeMismatch s a) = '[JAnnotatedNameMessage s a]

foreign import java unsafe "@new"
  newHandlerInputTypeMismatch  :: JString -> JExpr s a -> JExpr s a
                               -> JHandlerInputTypeMismatch s a

foreign import java unsafe "getExpectedType"
  jhandlerInputTypeMismatchExpectedType :: (t <: JHandlerInputTypeMismatch s a)
                                        => t -> JExpr s a

data JHandlerNotAFunction s a =
 JHandlerNotAFunction (@org.dhall.typecheck.message.HandlerNotAFunction s a)
  deriving (Class,Show,Eq)

type instance Inherits (JHandlerNotAFunction s a) = '[JAnnotatedNameMessage s a]

foreign import java unsafe "@new"
  newHandlerNotAFunction  :: JString -> JExpr s a -> JHandlerNotAFunction s a

data JHandlerOutputTypeMismatch s a =
    JHandlerOutputTypeMismatch (@org.dhall.typecheck.message.HandlerOutputTypeMismatch s a)
  deriving (Class,Show,Eq)

type instance Inherits (JHandlerOutputTypeMismatch s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newHandlerOutputTypeMismatch :: JString -> JExpr s a
                               -> JString -> JExpr s a
                               -> JFieldMismatch s a

foreign import java unsafe "getAlternative1"
  jhandlerOutputTypeMismatchAlt1 :: (t <: JHandlerOutputTypeMismatch s a)
                                 => t -> JString

foreign import java unsafe "getOutputType1"
  jhandlerOutputTypeMismatchOutType1 :: (t <: JHandlerOutputTypeMismatch s a)
                                     => t -> JExpr s a

foreign import java unsafe "getAlternative2"
  jhandlerOutputTypeMismatchAlt2 :: (t <: JHandlerOutputTypeMismatch s a)
                                 => t -> JString

foreign import java unsafe "getOutputType2"
  jhandlerOutputTypeMismatchOutType2 :: (t <: JHandlerOutputTypeMismatch s a)
                                     => t -> JExpr s a
data JIfBranchMismatch s a =
    JIfBranchMismatch (@org.dhall.typecheck.message.IfBranchMismatch s a)
  deriving (Class,Show,Eq)

type instance Inherits (JIfBranchMismatch s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newIfBranchMismatch :: JExpr s a -> JExpr s a
                      -> JExpr s a -> JExpr s a
                      -> JIfBranchMismatch s a

foreign import java unsafe "getThenExpression"
  jifBranchMismatchThenExpr :: (t <: JIfBranchMismatch s a)
                            => t -> JExpr s a

foreign import java unsafe "getThenType"
  jifBranchMismatchThenType :: (t <: JIfBranchMismatch s a)
                            => t -> JExpr s a

foreign import java unsafe "getElseExpression"
  jifBranchMismatchElseExpr :: (t <: JIfBranchMismatch s a)
                            => t -> JExpr s a

foreign import java unsafe "getElseType"
  jifBranchMismatchElseType :: (t <: JIfBranchMismatch s a)
                            => t -> JExpr s a

data JIfBranchMustBeTerm s a =
    JIfBranchMustBeTerm (@org.dhall.typecheck.message.IfBranchMustBeTerm s a)
  deriving (Class,Show,Eq)

type instance Inherits (JIfBranchMustBeTerm s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newIfBranchMustBeTerm :: Bool -> JExpr s a
                        -> JExpr s a -> JExpr s a
                        -> JIfBranchMustBeTerm s a

foreign import java unsafe "getIsThen"
  jifBranchMustBeTermIsThen :: (t <: JIfBranchMustBeTerm s a)
                            => t -> Bool

foreign import java unsafe "getExpression"
  jifBranchMustBeTermExpr :: (t <: JIfBranchMustBeTerm s a)
                          => t -> JExpr s a

foreign import java unsafe "getKind"
  jifBranchMustBeTermKind :: (t <: JIfBranchMustBeTerm s a)
                          => t -> JExpr s a

foreign import java unsafe "getSort"
  jifBranchMustBeTermSort :: (t <: JIfBranchMustBeTerm s a)
                          => t -> JExpr s a

data JInvalidAlternative s a =
  JInvalidAlternative (@org.dhall.typecheck.message.InvalidAlternative s a)
  deriving (Class,Show,Eq)

type instance Inherits (JInvalidAlternative s a) = '[JAnnotatedNameMessage s a]

foreign import java unsafe "@new"
  newInvalidAlternative  :: JString -> JExpr s a -> JInvalidAlternative s a

data JInvalidAlternativeType s a =
  JInvalidAlternativeType (@org.dhall.typecheck.message.InvalidAlternativeType s a)
  deriving (Class,Show,Eq)

type instance Inherits (JInvalidAlternativeType s a) = '[JAnnotatedNameMessage s a]

foreign import java unsafe "@new"
  newInvalidAlternativeType  :: JString -> JExpr s a -> JInvalidAlternativeType s a

data JInvalidField s a =
  JInvalidField (@org.dhall.typecheck.message.InvalidField s a)
  deriving (Class,Show,Eq)

type instance Inherits (JInvalidField s a) = '[JAnnotatedNameMessage s a]

foreign import java unsafe "@new"
  newInvalidField  :: JString -> JExpr s a -> JInvalidField s a

data JInvalidFieldType s a =
  JInvalidFieldType (@org.dhall.typecheck.message.InvalidFieldType s a)
  deriving (Class,Show,Eq)

type instance Inherits (JInvalidFieldType s a) = '[JAnnotatedNameMessage s a]

foreign import java unsafe "@new"
  newInvalidFieldType  :: JString -> JExpr s a -> JInvalidFieldType s a

data JInvalidHandlerOutputType s a =
  JInvalidHandlerOutputType (@org.dhall.typecheck.message.InvalidHandlerOutputType s a)
  deriving (Class,Show,Eq)

type instance Inherits (JInvalidHandlerOutputType s a) = '[JAnnotatedNameMessage s a]

foreign import java unsafe "@new"
  newInvalidHandlerOutputType  :: JString -> JExpr s a -> JExpr s a
                               -> JInvalidHandlerOutputType s a

foreign import java unsafe "getExpectedType"
  jinvalidHandlerOutputTypeExpectedType :: (t <: JInvalidHandlerOutputType s a)
                                        => t -> JExpr s a

data JInvalidInputType s a =
  JInvalidInputType (@org.dhall.typecheck.message.InvalidInputType s a)
  deriving (Class,Show,Eq)

type instance Inherits (JInvalidInputType s a) = '[JAnnotationMessage s a]

foreign import java unsafe "@new"
  newInvalidInputType  :: JExpr s a -> JInvalidInputType s a

data JInvalidListElement s a =
  JInvalidListElement (@org.dhall.typecheck.message.InvalidListElement s a)
  deriving (Class,Show,Eq)

type instance Inherits (JInvalidListElement s a) = '[JAnnotatedExpressionMismatchMessage s a]

foreign import java unsafe "@new"
  newInvalidListElement  :: JInteger -> JExpr s a -> JExpr s a -> JExpr s a
                         -> JInvalidListElement s a

foreign import java unsafe "getIndex"
  jinvalidListElementIndex :: (t <: JInvalidListElement s a)
                           => t -> JInteger

data JInvalidListType s a =
  JInvalidListType (@org.dhall.typecheck.message.InvalidListType s a)
  deriving (Class,Show,Eq)

type instance Inherits (JInvalidListType s a) = '[JAnnotationMessage s a]

foreign import java unsafe "@new"
  newInvalidListType  :: JExpr s a -> JInvalidListType s a

data JInvalidOptionalElement s a =
  JInvalidOptionalElement (@org.dhall.typecheck.message.InvalidOptionalElement s a)
  deriving (Class,Show,Eq)

type instance Inherits (JInvalidOptionalElement s a) =
  '[JAnnotatedExpressionMismatchMessage s a]

foreign import java unsafe "@new"
  newInvalidOptionalElement :: JExpr s a -> JExpr s a -> JExpr s a
                            -> JInvalidOptionalElement s a

data JInvalidOptionalType s a =
  JInvalidOptionalType (@org.dhall.typecheck.message.InvalidOptionalType s a)
  deriving (Class,Show,Eq)

type instance Inherits (JInvalidOptionalType s a) = '[JAnnotationMessage s a]

foreign import java unsafe "@new"
  newInvalidOptionalType  :: JExpr s a -> JInvalidOptionalType s a

data JInvalidOutputType s a =
  JInvalidOutputType (@org.dhall.typecheck.message.InvalidOutputType s a)
  deriving (Class,Show,Eq)

type instance Inherits (JInvalidOutputType s a) = '[JAnnotationMessage s a]

foreign import java unsafe "@new"
  newInvalidOutputType  :: JExpr s a -> JInvalidOutputType s a

data JInvalidPredicate s a =
  JInvalidPredicate (@org.dhall.typecheck.message.InvalidPredicate s a)
  deriving (Class,Show,Eq)

type instance Inherits (JInvalidPredicate s a) =
  '[JAnnotatedExpressionMessage s a]

foreign import java unsafe "@new"
  newInvalidPredicate  :: JExpr s a -> JExpr s a -> JCantAdd s a

data JInvalidSome s a =
  JInvalidSome (@org.dhall.typecheck.message.InvalidSome s a)
  deriving (Class,Show,Eq)

type instance Inherits (JInvalidSome s a) =
  '[JAnnotatedExpressionMessage s a]

foreign import java unsafe "@new"
  newInvalidSome :: JExpr s a -> JExpr s a -> JExpr s a
                 -> JInvalidSome s a

foreign import java unsafe "getKind"
  jinvalidSomeKind :: (t <: JInvalidSome s a)
                    => t -> JExpr s a

data JListAppendMismatch s a =
  JListAppendMismatch (@org.dhall.typecheck.message.ListAppendMismatch s a)
  deriving (Class,Show,Eq)

type instance Inherits (JListAppendMismatch s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newListAppendMismatch :: JExpr s a -> JExpr s a -> JListAppendMismatch s a

foreign import java unsafe "getList1Type"
  jlistAppendMismatchList1Ty :: (t <: JListAppendMismatch s a)
                             => t -> JExpr s a

foreign import java unsafe "getList2Type"
  jlistAppendMismatchList2Ty :: (t <: JListAppendMismatch s a)
                             => t -> JExpr s a

data JMismatchedListElements s a =
  JMismatchedListElements (@org.dhall.typecheck.message.MismatchedListElements s a)
  deriving (Class,Show,Eq)

type instance Inherits (JMismatchedListElements s a) = '[JAnnotatedExpressionMismatchMessage s a]

foreign import java unsafe "@new"
  newMismatchedListElements  :: JInteger -> JExpr s a -> JExpr s a -> JExpr s a
                             -> JMismatchedListElements s a

foreign import java unsafe "getIndex"
  jmismatchedListElementsIndex :: (t <: JMismatchedListElements s a)
                               => t -> JInteger

data JMissingField s a =
  JMissingField (@org.dhall.typecheck.message.MissingField s a)
  deriving (Class,Show,Eq)

type instance Inherits (JMissingField s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newMissingField :: JString -> JExpr s a -> JMissingField s a

foreign import java unsafe "getMissingField"
  jmissingField :: (t <: JMissingField s a) => t -> JString

foreign import java unsafe "getActualFields"
  jmissinfFieldActualFields :: (t <: JMissingField s a)
                             => t -> JExpr s a

data JMissingHandler s a =
  JMissingHandler (@org.dhall.typecheck.message.MissingHandler s a)
  deriving (Class,Show,Eq)

type instance Inherits (JMissingHandler s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newMissingHandler :: Set JString  -> JMissingHandler s a

foreign import java unsafe "getHandlers"
  jmissingHandlerActualHandlers :: (t <: JMissingHandler s a) => t -> Set JString

data JMissingListType s a =
  JMissingListType (@org.dhall.typecheck.message.MissingListType s a)
  deriving (Class,Show,Eq)

type instance Inherits (JMissingListType s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newMissingListType :: JMissingListType s a

data JMissingMergeType s a =
  JMissingMergeType (@org.dhall.typecheck.message.MissingMergeType s a)
  deriving (Class,Show,Eq)

type instance Inherits (JMissingMergeType s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newMissingMergeType :: JMissingMergeType s a

data JMustCombineARecord s a =
  JMustCombineARecord (@org.dhall.typecheck.message.MustCombineARecord s a)
  deriving (Class,Show,Eq)

type instance Inherits (JMustCombineARecord s a) = '[JAnnotatedExpressionMessage s a]

foreign import java unsafe "@new"
  newMustCombineARecord  :: Char -> JExpr s a -> JExpr s a
                         -> JMismatchedListElements s a

foreign import java unsafe "getOperation"
  jmustCombineARecordOp :: (t <: JMustCombineARecord s a)
                        => t -> Char

data JMustMergeARecord s a =
  JMustMergeARecord (@org.dhall.typecheck.message.MustMergeARecord s a)
  deriving (Class,Show,Eq)

type instance Inherits (JMustMergeARecord s a) = '[JAnnotatedExpressionMessage s a]

foreign import java unsafe "@new"
  newMustMergeARecord  :: JExpr s a -> JExpr s a
                       -> JMustMergeARecord s a

data JMustMergeUnion s a =
  JMustMergeUnion (@org.dhall.typecheck.message.MustMergeUnion s a)
  deriving (Class,Show,Eq)

type instance Inherits (JMustMergeUnion s a) = '[JAnnotatedExpressionMessage s a]

foreign import java unsafe "@new"
  newMustMergeUnion  :: JExpr s a -> JExpr s a
                     -> JMustMergeUnion s a

data JNoDependentTypes s a =
  JNoDependentTypes (@org.dhall.typecheck.message.NoDependentTypes s a)
  deriving (Class,Show,Eq)

type instance Inherits (JNoDependentTypes s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newNoDependentTypes :: JExpr s a -> JExpr s a -> JNoDependentTypes s a

foreign import java unsafe "getInputType"
  jnoDependentTypesInputType :: (t <: JNoDependentTypes s a)
                             => t -> JExpr s a

foreign import java unsafe "getOutputKind"
  jnoDependentTypesOutputKind :: (t <: JNoDependentTypes s a)
                              => t -> JExpr s a

data JNotAFunction s a =
  JNotAFunction (@org.dhall.typecheck.message.NotAFunction s a)
  deriving (Class,Show,Eq)

type instance Inherits (JNotAFunction s a) = '[JAnnotatedExpressionMessage s a]

foreign import java unsafe "@new"
  newNotAFunction  :: JExpr s a -> JExpr s a
                   -> JNotAFunction s a

data JRecordMismatch s a =
  JRecordMismatch (@org.dhall.typecheck.message.RecordMismatch s a)
  deriving (Class,Show,Eq)

type instance Inherits (JRecordMismatch s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newRecordMismatch :: Char
                    -> JExpr s a -> JExpr s a
                    -> JConst -> JConst
                    -> JRecordMismatch s a

foreign import java unsafe "getOperation"
  jrecordMismatchOp :: (t <: JRecordMismatch s a) => t -> Char

foreign import java unsafe "getRecord1Const"
  jrecordMismatchRecord1Const :: (t <: JRecordMismatch s a)
                              => t -> JConst

foreign import java unsafe "getRecord2Const"
  jrecordMismatchRecord2Const :: (t <: JRecordMismatch s a)
                              => t -> JConst

foreign import java unsafe "getRecord1"
  jrecordMismatchRecord1 :: (t <: JRecordMismatch s a) => t -> JExpr s a

foreign import java unsafe "getRecord2"
  jrecordMismatchRecord2 :: (t <: JRecordMismatch s a) => t -> JExpr s a


data JRecordTypeMismatch s a =
  JRecordTypeMismatch (@org.dhall.typecheck.message.RecordTypeMismatch s a)
  deriving (Class,Show,Eq)

type instance Inherits (JRecordTypeMismatch s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newRecordTypeMismatch :: JConst    -> JConst 
                        -> JExpr s a -> JExpr s a
                        -> JRecordTypeMismatch s a

foreign import java unsafe "getRecord1"
  jrecordTypeMismatchRecord1 :: (t <: JRecordTypeMismatch s a)
                             => t -> JExpr s a

foreign import java unsafe "getRecord1Const"
  jrecordTypeMismatchRecord1Const :: (t <: JRecordTypeMismatch s a)
                                  => t -> JConst

foreign import java unsafe "getRecord2"
  jrecordTypeMismatchRecord2 :: (t <: JRecordTypeMismatch s a)
                             => t -> JExpr s a

foreign import java unsafe "getRecord2Const"
  jrecordTypeMismatchRecord2Const :: (t <: JRecordTypeMismatch s a)
                                  => t -> JConst

data JTypeMismatch s a =
  JTypeMismatch (@org.dhall.typecheck.message.TypeMismatch s a)
  deriving (Class,Show,Eq)

type instance Inherits (JTypeMismatch s a) = '[JAnnotatedExpressionMismatchMessage s a]

foreign import java unsafe "@new"
  newTypeMismatch :: JExpr s a -> JExpr s a
                  -> JExpr s a -> JExpr s a
                  -> JTypeMismatch s a

foreign import java unsafe "getFunction"
  jtypeMismatchFunction :: (t <: JTypeMismatch s a)
                        => t -> JExpr s a

data JUnboundVariable s a =
  JUnboundVariable (@org.dhall.typecheck.message.UnboundVariable s a)
  deriving (Class,Show,Eq)

type instance Inherits (JUnboundVariable s a) = '[JNameMessage s a]

foreign import java unsafe "@new"
  newUnboundVariable :: JString -> JUnboundVariable s a

data JUntyped s a =
  JUntyped (@org.dhall.typecheck.message.Untyped s a)
  deriving (Class,Show,Eq)

type instance Inherits (JUntyped s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newUntyped :: JUntyped s a

data JUnusedHandler s a =
  JUnusedHandler (@org.dhall.typecheck.message.UnusedHandler s a)
  deriving (Class,Show,Eq)

type instance Inherits (JUnusedHandler s a) = '[JTypeMessage s a]

foreign import java unsafe "@new"
  newUnusedHandler :: Set JString -> JUnusedHandler s a

foreign import java unsafe "getHandlers"
  junusedHandlerHandlers :: (t <: JUnusedHandler s a) => t -> Set JString

instance (Class js, Class ja, JavaConverter s js, JavaConverter a ja) =>
         JavaConverter (Dhall.TypeMessage s a) (JTypeMessage js ja) where

  toJava (Dhall.AnnotMismatch expr expTy actTy) =
    superCast $ newAnnotMismatch (toJava expr) (toJava expTy) (toJava actTy)
  toJava (Dhall.CantAccess key expectTy actTy) =
    superCast $ newCantAccess (toJava key) (toJava expectTy)
                              (toJava actTy)
  toJava (Dhall.CantAdd expr actTy) =
    superCast $ newCantAdd (toJava expr) (toJava actTy)
  toJava (Dhall.CantAnd expr actTy) =
    superCast $ newCantAnd (toJava expr) (toJava actTy)
  toJava (Dhall.CantEQ expr actTy) =
    superCast $ newCantEQ (toJava expr) (toJava actTy)
  toJava (Dhall.CantInterpolate expr actTy) =
    superCast $ newCantInterpolate (toJava expr) (toJava actTy)
  toJava (Dhall.CantListAppend expr actTy) =
    superCast $ newCantListAppend (toJava expr) (toJava actTy)
  toJava (Dhall.CantMultiply expr actTy) =
    superCast $ newCantMultiply (toJava expr) (toJava actTy)
  toJava (Dhall.CantNE expr actTy) =
    superCast $ newCantNE (toJava expr) (toJava actTy)
  toJava (Dhall.CantOr expr actTy) =
    superCast $ newCantOr (toJava expr) (toJava actTy)
  toJava (Dhall.CantProject key expectTy actTy) =
    superCast $ newCantProject (toJava key) (toJava expectTy)
                               (toJava actTy)
  toJava (Dhall.CantTextAppend expr actTy) =
    superCast $ newCantTextAppend (toJava expr) (toJava actTy)
  toJava (Dhall.CombineTypesRequiresRecordType arg norm) =
    superCast $ newCombineTypesRequiresRecordType (toJava arg) (toJava norm)
  toJava (Dhall.ConstructorsRequiresAUnionType arg norm) =
    superCast $ newConstructorsRequiresAUnionType (toJava arg) (toJava norm)
  toJava (Dhall.DuplicateAlternative name) =
    superCast $ newDuplicateAlternative (toJava name)
  toJava (Dhall.FieldAnnotationMismatch f1Name f1Annot f1Const f2Name f2Annot f2Const) =
    superCast $ newFieldAnnotationMismatch
      (toJava f1Name) (toJava f1Annot) (toJava f1Const)
      (toJava f2Name) (toJava f2Annot) (toJava f2Const)
  toJava (Dhall.FieldCollision name) =
    superCast $ newFieldCollision (toJava name)
  toJava (Dhall.FieldMismatch f1Name f1Val f1Const f2Name f2Val f2Const) =
    superCast $ newFieldMismatch (toJava f1Name) (toJava f1Val)
                                 (toJava f1Const)
                                 (toJava f2Name) (toJava f2Val)
                                 (toJava f2Const)
  toJava (Dhall.HandlerInputTypeMismatch alter expectTy actTy) =
    superCast $ newHandlerInputTypeMismatch (toJava alter) (toJava expectTy)
                                            (toJava actTy)
  toJava (Dhall.HandlerNotAFunction name ty) =
    superCast $ newHandlerNotAFunction (toJava name) (toJava ty)
  toJava (Dhall.HandlerOutputTypeMismatch alt1 outTy1 alt2 outTy2) =
    superCast $ newHandlerOutputTypeMismatch (toJava alt1) (toJava outTy1)
                                             (toJava alt2) (toJava outTy2)
  toJava (Dhall.IfBranchMismatch thenExpr thenTy elseExpr elseTy) =
    superCast $ newIfBranchMismatch (toJava thenExpr) (toJava thenTy)
                                    (toJava elseExpr) (toJava elseTy)
  toJava (Dhall.IfBranchMustBeTerm isThen expr kind sort) =
    superCast $ newIfBranchMustBeTerm isThen (toJava expr)
                                      (toJava kind) (toJava sort)
  toJava (Dhall.InvalidAlternative name ty) =
    superCast $ newInvalidAlternative (toJava name) (toJava ty)
  toJava (Dhall.InvalidAlternativeType name ty) =
    superCast $ newInvalidAlternativeType (toJava name) (toJava ty)
  toJava (Dhall.InvalidField name ty) =
    superCast $ newInvalidField (toJava name) (toJava ty)
  toJava (Dhall.InvalidFieldType name ty) =
    superCast $ newInvalidFieldType (toJava name) (toJava ty)
  toJava (Dhall.InvalidHandlerOutputType name expectTy actTy) =
    superCast $ newInvalidHandlerOutputType (toJava name) (toJava expectTy)
                                            (toJava actTy)
  toJava (Dhall.InvalidInputType ty) =
    superCast $ newInvalidInputType (toJava ty)
  toJava (Dhall.InvalidListElement idx expectTy elemExpr actTy) =
    superCast $ newInvalidListElement (toJava idx) (toJava expectTy)
                                      (toJava elemExpr) (toJava actTy)
  toJava (Dhall.InvalidListType ty) =
    superCast $ newInvalidListType (toJava ty)
  toJava (Dhall.InvalidOptionalElement expectTy expr actTy) =
    superCast $ newInvalidOptionalElement (toJava expectTy)
                                          (toJava expr) (toJava actTy)
  toJava (Dhall.InvalidOptionalType ty) =
    superCast $ newInvalidOptionalType (toJava ty)
  toJava (Dhall.InvalidOutputType ty) =
    superCast $ newInvalidOutputType (toJava ty)  
  toJava (Dhall.InvalidPredicate expr actTy) =
    superCast $ newInvalidPredicate (toJava expr) (toJava actTy) 
  toJava (Dhall.InvalidSome expr ty kind) =
    superCast $ newInvalidSome (toJava expr)
                               (toJava ty) (toJava kind)
  toJava (Dhall.ListAppendMismatch list1Ty list2Ty) =
    superCast $ newListAppendMismatch (toJava list1Ty) (toJava list2Ty) 
  toJava (Dhall.MismatchedListElements idx expectTy elemExpr actTy) =
    superCast $ newMismatchedListElements (toJava idx) (toJava expectTy)
                                          (toJava elemExpr) (toJava actTy)
  toJava (Dhall.MissingField missingField actualFields) =
    superCast $ newMissingField (toJava missingField) (toJava actualFields)
  toJava (Dhall.MissingHandler actualHandlers) =
    superCast $ newMissingHandler (deepToJava actualHandlers)
  toJava Dhall.MissingListType =
    superCast $ newMissingListType
  toJava Dhall.MissingMergeType =
    superCast $ newMissingMergeType
  toJava (Dhall.MustCombineARecord op actVal actTy) =
    superCast $ newMustCombineARecord op (toJava actVal) (toJava actTy)
  toJava (Dhall.MustMergeARecord actVal actTy) =
    superCast $ newMustMergeARecord (toJava actVal) (toJava actTy)
  toJava (Dhall.MustMergeUnion actVal actTy) =
    superCast $ newMustMergeUnion (toJava actVal) (toJava actTy)
  toJava (Dhall.NoDependentTypes inputTy outputKind) =
    superCast $ newListAppendMismatch (toJava inputTy) (toJava outputKind) 
  toJava (Dhall.NotAFunction actVal actTy) =
    superCast $ newNotAFunction (toJava actVal) (toJava actTy)
  toJava (Dhall.RecordMismatch op rec1 rec2 rec1Const rec2Const) =
    superCast $ newRecordMismatch op (toJava rec1) (toJava rec2)
                                     (toJava rec1Const) (toJava rec2Const)
  toJava (Dhall.RecordTypeMismatch rec1Const rec2Const rec1 rec2) =
    superCast $ newRecordTypeMismatch (toJava rec1Const) (toJava rec2Const)
                                      (toJava rec1) (toJava rec2)
  toJava (Dhall.TypeMismatch func expectTy elemExpr actTy) =
    superCast $ newTypeMismatch (toJava func) (toJava expectTy)
                                (toJava elemExpr) (toJava actTy)  
  toJava (Dhall.UnboundVariable name) =
    superCast $ newUnboundVariable (toJava name)
  toJava Dhall.Untyped =
    superCast $ newUntyped
  toJava (Dhall.UnusedHandler handlers) =
    superCast $ newUnusedHandler (deepToJava handlers)

  
  fromJava jtyMsg
    | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JAnnotMismatch js ja)) =
        Dhall.AnnotMismatch (fromJava expr) (fromJava expectedAnnot)
                            (fromJava annot)
    | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JCantAccess js ja)) =
        Dhall.CantAccess
          (fromJava annName)
          (fromJava $ jcantAccessExpectedType
                    $ ((unsafeCast jtyMsg) :: JCantAccess js ja))
          (fromJava annot)
    | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JCantAdd js ja)) =
        Dhall.CantAdd (fromJava expr) (fromJava annot)
    | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JCantAnd js ja)) =
        Dhall.CantAnd (fromJava expr) (fromJava annot)
    | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JCantEQ js ja)) =
        Dhall.CantEQ (fromJava expr) (fromJava annot)
    | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JCantInterpolate js ja)) =
        Dhall.CantInterpolate (fromJava expr) (fromJava annot)
    | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JCantListAppend js ja)) =
        Dhall.CantListAppend (fromJava expr) (fromJava annot)
    | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JCantMultiply js ja)) =
        Dhall.CantMultiply (fromJava expr) (fromJava annot)
    | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JCantNE js ja)) =
        Dhall.CantNE (fromJava expr) (fromJava annot)
    | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JCantOr js ja)) =
        Dhall.CantOr (fromJava expr) (fromJava annot)
    | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JCantProject js ja)) =
        Dhall.CantProject
          (fromJava annName)
          (fromJava $ jcantProjectExpectedType
                    $ ((unsafeCast jtyMsg) :: JCantProject js ja))
          (fromJava annot)
    | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JCantTextAppend js ja)) =
        Dhall.CantTextAppend (fromJava expr) (fromJava annot)
    | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JCombineTypesRequiresRecordType js ja)) =
        Dhall.CombineTypesRequiresRecordType
          (fromJava $ jcombineTypesRequiresRecordTypeArg
                    $ ((unsafeCast jtyMsg) :: JCombineTypesRequiresRecordType js ja))
          (fromJava $ jcombineTypesRequiresRecordTypeNorm
                    $ ((unsafeCast jtyMsg) :: JCombineTypesRequiresRecordType js ja))
    | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JConstructorsRequiresAUnionType js ja)) =
        Dhall.ConstructorsRequiresAUnionType
          (fromJava $ jconstructorsRequiresAUnionTypeArg
                    $ ((unsafeCast jtyMsg) :: JConstructorsRequiresAUnionType js ja))
          (fromJava $ jconstructorsRequiresAUnionTypeNorm
                    $ ((unsafeCast jtyMsg) :: JConstructorsRequiresAUnionType js ja))
    | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JDuplicateAlternative js ja)) =
        Dhall.DuplicateAlternative (fromJava name)
    | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JFieldAnnotationMismatch js ja)) =
        Dhall.FieldAnnotationMismatch
          (fromJava $ jfieldAnnotationMismatchField1Name
                    $ ((unsafeCast jtyMsg) :: JFieldAnnotationMismatch js ja))
          (fromJava $ jfieldAnnotationMismatchField1Annot
                    $ ((unsafeCast jtyMsg) :: JFieldAnnotationMismatch js ja))
          (fromJava $ jfieldAnnotationMismatchField1Const
                    $ ((unsafeCast jtyMsg) :: JFieldAnnotationMismatch js ja))
          (fromJava $ jfieldAnnotationMismatchField2Name
                    $ ((unsafeCast jtyMsg) :: JFieldAnnotationMismatch js ja))
          (fromJava $ jfieldAnnotationMismatchField2Annot
                    $ ((unsafeCast jtyMsg) :: JFieldAnnotationMismatch js ja))
          (fromJava $ jfieldAnnotationMismatchField2Const
                    $ ((unsafeCast jtyMsg) :: JFieldAnnotationMismatch js ja))
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JFieldCollision js ja)) =
        Dhall.FieldCollision (fromJava name)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JFieldMismatch js ja)) =
        Dhall.FieldMismatch
          (fromJava $ jfieldMismatchField1Name
                    $ ((unsafeCast jtyMsg) :: JFieldMismatch js ja))
          (fromJava $ jfieldMismatchField1Value
                    $ ((unsafeCast jtyMsg) :: JFieldMismatch js ja))
          (fromJava $ jfieldMismatchField1Const
                    $ ((unsafeCast jtyMsg) :: JFieldMismatch js ja))
          (fromJava $ jfieldMismatchField2Name
                    $ ((unsafeCast jtyMsg) :: JFieldMismatch js ja))
          (fromJava $ jfieldMismatchField2Value
                    $ ((unsafeCast jtyMsg) :: JFieldMismatch js ja))
          (fromJava $ jfieldMismatchField2Const
                    $ ((unsafeCast jtyMsg) :: JFieldMismatch js ja))
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JHandlerInputTypeMismatch js ja)) =
        Dhall.HandlerInputTypeMismatch
          (fromJava annName)
          (fromJava $ jhandlerInputTypeMismatchExpectedType
                    $ ((unsafeCast jtyMsg) :: JHandlerInputTypeMismatch js ja))
          (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JHandlerNotAFunction js ja)) =
        Dhall.HandlerNotAFunction (fromJava annName) (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JHandlerOutputTypeMismatch js ja)) =
        Dhall.HandlerOutputTypeMismatch
          (fromJava $ jhandlerOutputTypeMismatchAlt1
                    $ ((unsafeCast jtyMsg) :: JHandlerOutputTypeMismatch js ja))
          (fromJava $ jhandlerOutputTypeMismatchOutType1
                    $ ((unsafeCast jtyMsg) :: JHandlerOutputTypeMismatch js ja))
          (fromJava $ jhandlerOutputTypeMismatchAlt2
                    $ ((unsafeCast jtyMsg) :: JHandlerOutputTypeMismatch js ja))
          (fromJava $ jhandlerOutputTypeMismatchOutType2
                    $ ((unsafeCast jtyMsg) :: JHandlerOutputTypeMismatch js ja))
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JIfBranchMismatch js ja)) =
        Dhall.IfBranchMismatch
          (fromJava $ jifBranchMismatchThenExpr
                    $ ((unsafeCast jtyMsg) :: JIfBranchMismatch js ja))
          (fromJava $ jifBranchMismatchThenType
                    $ ((unsafeCast jtyMsg) :: JIfBranchMismatch js ja))
          (fromJava $ jifBranchMismatchElseExpr
                    $ ((unsafeCast jtyMsg) :: JIfBranchMismatch js ja))
          (fromJava $ jifBranchMismatchElseType
                    $ ((unsafeCast jtyMsg) :: JIfBranchMismatch js ja))
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JIfBranchMustBeTerm js ja)) =
        Dhall.IfBranchMustBeTerm
          (jifBranchMustBeTermIsThen
                    $ ((unsafeCast jtyMsg) :: JIfBranchMustBeTerm js ja))
          (fromJava $ jifBranchMustBeTermExpr
                    $ ((unsafeCast jtyMsg) :: JIfBranchMustBeTerm js ja))
          (fromJava $ jifBranchMustBeTermKind
                    $ ((unsafeCast jtyMsg) :: JIfBranchMustBeTerm js ja))
          (fromJava $ jifBranchMustBeTermSort
                    $ ((unsafeCast jtyMsg) :: JIfBranchMustBeTerm js ja))
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JInvalidAlternative js ja)) =
        Dhall.InvalidAlternative (fromJava annName) (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JInvalidAlternativeType js ja)) =
        Dhall.InvalidAlternativeType (fromJava annName) (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JInvalidField js ja)) =
        Dhall.InvalidField (fromJava annName) (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JInvalidFieldType js ja)) =
        Dhall.InvalidFieldType (fromJava annName) (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JInvalidHandlerOutputType js ja)) =
        Dhall.InvalidHandlerOutputType
          (fromJava annName)
          (fromJava $ jinvalidHandlerOutputTypeExpectedType
                    $ ((unsafeCast jtyMsg) :: JInvalidHandlerOutputType js ja))
          (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JInvalidInputType js ja)) =
        Dhall.InvalidInputType (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JInvalidListElement js ja)) =
        Dhall.InvalidListElement
          (fromJava $ jinvalidListElementIndex
                    $ ((unsafeCast jtyMsg) :: JInvalidListElement js ja))
          (fromJava expectedAnnot) (fromJava expr) (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JInvalidListType js ja)) =
        Dhall.InvalidListType (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JInvalidOptionalElement js ja)) =
        Dhall.InvalidOptionalElement
          (fromJava expectedAnnot) (fromJava expr) (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JInvalidOptionalType js ja)) =
        Dhall.InvalidOptionalType (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JInvalidOutputType js ja)) =
        Dhall.InvalidOutputType (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JInvalidPredicate js ja)) =
        Dhall.InvalidPredicate (fromJava expr) (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JInvalidSome js ja)) =
        Dhall.InvalidSome
          (fromJava expr) (fromJava annot)
          (fromJava $ jinvalidSomeKind
                    $ ((unsafeCast jtyMsg) :: JInvalidSome js ja))
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JListAppendMismatch js ja)) =
        Dhall.ListAppendMismatch
          (fromJava $ jlistAppendMismatchList1Ty
                    $ ((unsafeCast jtyMsg) :: JListAppendMismatch js ja))
          (fromJava $ jlistAppendMismatchList2Ty
                    $ ((unsafeCast jtyMsg) :: JListAppendMismatch js ja))
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JMismatchedListElements js ja)) =
        Dhall.MismatchedListElements
          (fromJava $ jmismatchedListElementsIndex
                    $ ((unsafeCast jtyMsg) :: JMismatchedListElements js ja))
          (fromJava expectedAnnot) (fromJava expr) (fromJava annot)       
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JMissingField js ja)) =
        Dhall.MissingField
          (fromJava $ jmissingField
                    $ ((unsafeCast jtyMsg) :: JMissingField js ja))
          (fromJava $ jmissinfFieldActualFields
                    $ ((unsafeCast jtyMsg) :: JMissingField js ja))
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JMissingHandler js ja)) =
        Dhall.MissingHandler
          (deepFromJava $ jmissingHandlerActualHandlers
                    $ ((unsafeCast jtyMsg) :: JMissingHandler js ja))
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JMissingListType js ja)) =
        Dhall.MissingListType
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JMissingMergeType js ja)) =
        Dhall.MissingMergeType
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JMustCombineARecord js ja)) =
        Dhall.MustCombineARecord
          (jmustCombineARecordOp ((unsafeCast jtyMsg) :: JMustCombineARecord js ja))
          (fromJava expr) (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JMustMergeARecord js ja)) =
        Dhall.MustMergeARecord (fromJava expr) (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JMustMergeUnion js ja)) =
        Dhall.MustMergeUnion (fromJava expr) (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JNoDependentTypes js ja)) =
        Dhall.NoDependentTypes
          (fromJava $ jnoDependentTypesInputType
                    $ ((unsafeCast jtyMsg) :: JNoDependentTypes js ja))
          (fromJava $ jnoDependentTypesOutputKind
                    $ ((unsafeCast jtyMsg) :: JNoDependentTypes js ja))     
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JNotAFunction js ja)) =
        Dhall.NotAFunction (fromJava expr) (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JRecordMismatch js ja)) =
        Dhall.RecordMismatch
          (jrecordMismatchOp ((unsafeCast jtyMsg) :: JRecordMismatch js ja))
          (fromJava $ jrecordMismatchRecord1
                    $ ((unsafeCast jtyMsg) :: JRecordMismatch js ja))
          (fromJava $ jrecordMismatchRecord2
                    $ ((unsafeCast jtyMsg) :: JRecordMismatch js ja))
          (fromJava $ jrecordMismatchRecord1Const
                    $ ((unsafeCast jtyMsg) :: JRecordMismatch js ja))
          (fromJava $ jrecordMismatchRecord2Const
                    $ ((unsafeCast jtyMsg) :: JRecordMismatch js ja))
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JRecordTypeMismatch js ja)) =
        Dhall.RecordTypeMismatch
          (fromJava $ jrecordTypeMismatchRecord1Const
                    $ ((unsafeCast jtyMsg) :: JRecordTypeMismatch js ja))
          (fromJava $ jrecordTypeMismatchRecord2Const
                    $ ((unsafeCast jtyMsg) :: JRecordTypeMismatch js ja))
          (fromJava $ jrecordTypeMismatchRecord1
                    $ ((unsafeCast jtyMsg) :: JRecordTypeMismatch js ja))
          (fromJava $ jrecordTypeMismatchRecord2
                    $ ((unsafeCast jtyMsg) :: JRecordTypeMismatch js ja))
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JTypeMismatch js ja)) =
        Dhall.TypeMismatch
          (fromJava $ jtypeMismatchFunction
                    $ ((unsafeCast jtyMsg) :: JTypeMismatch js ja))
          (fromJava expectedAnnot) (fromJava expr) (fromJava annot)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JUnboundVariable js ja)) =
        Dhall.UnboundVariable (fromJava name)
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JUntyped js ja)) =
        Dhall.Untyped
     | jtyMsg `instanceOf` getClass (Proxy :: Proxy (JUnusedHandler js ja)) =
        Dhall.UnusedHandler
          (deepFromJava $ junusedHandlerHandlers
                    $ ((unsafeCast jtyMsg) :: JUnusedHandler js ja))
     | otherwise =
        error $ "Unknown subclass: " ++ show jtyMsg
        
    where annot :: JExpr js ja
          annot = jtypeMsgAnnot $ ((unsafeCast jtyMsg) :: JAnnotationMessage js ja)

          expr :: JExpr js ja
          expr = jtypeMsgExpr $ ((unsafeCast jtyMsg) :: JAnnotatedExpressionMessage js ja)

          name :: JString
          name = jtypeMsgName $ ((unsafeCast jtyMsg) :: JNameMessage js ja)

          annName :: JString
          annName = jtypeMsgAnnName $ ((unsafeCast jtyMsg) :: JAnnotatedNameMessage js ja)
          
          expectedAnnot :: JExpr js ja
          expectedAnnot = jtypeMsgExpectedAnnot $
            ((unsafeCast jtyMsg) :: JAnnotatedExpressionMismatchMessage js ja)


data JTypeError s a = JTypeError (@org.dhall.typecheck.TypeError s a)
  deriving (Class,Show,Eq)

foreign import java unsafe "@new"
  newTypeError  :: JContext (JExpr s a) -> JExpr s a -> JTypeMessage s a
                -> JTypeError s a

foreign import java unsafe "getContext"
  jtypeErrorContext  :: JTypeError s a -> JContext (JExpr s a)

foreign import java unsafe "getCurrent"
  jtypeErrorCurrent  :: JTypeError s a -> JExpr s a

foreign import java unsafe "getMessage"
  jtypeErrorMessage  :: JTypeError s a -> JTypeMessage s a

instance (Class js, Class ja, JavaConverter s js, JavaConverter a ja)
      => JavaConverter (Dhall.TypeError s a) (JTypeError js ja) where
  toJava (Dhall.TypeError ctx curr msg) =
    newTypeError (toJava ctx) (toJava curr) (toJava msg)
  fromJava jtyErr = Dhall.TypeError
                    (fromJava $ jtypeErrorContext jtyErr)
                    (fromJava $ jtypeErrorCurrent jtyErr)
                    (fromJava $ jtypeErrorMessage jtyErr)

data JDetailedTypeError s a = JDetailedTypeError
  (@org.dhall.typecheck.DetailedTypeError s a)
     deriving (Class,Show,Eq)

foreign import java unsafe "@new"
  newDetailedTypeError  :: JTypeError s a -> JDetailedTypeError s a

foreign import java unsafe "getTypeError"
  jdetailedTypeError  :: JDetailedTypeError s a -> JTypeError s a

instance (Class js, Class ja, JavaConverter s js, JavaConverter a ja)
      => JavaConverter (Dhall.DetailedTypeError s a)
                       (JDetailedTypeError js ja) where
  toJava (Dhall.DetailedTypeError tyErr) =
    newDetailedTypeError (toJava tyErr)
  fromJava jdetTyErr = Dhall.DetailedTypeError
                    (fromJava $ jdetailedTypeError jdetTyErr)
