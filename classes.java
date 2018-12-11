src/main/java/org/dhall/eta/InputType.java
src/main/java/org/dhall/eta/RecordType.java
src/main/java/org/dhall/eta/TagValue.java
src/main/java/org/dhall/eta/Type.java
src/main/java/org/dhall/eta/UnionType.java
java/src/main/java/org/dhall/Context.java
java/src/main/java/org/dhall/Named.java
java/src/main/java/org/dhall/StandardVersion.java
java/src/main/java/org/dhall/binary/DecodingFailure.java
java/src/main/java/org/dhall/binary/decoding/failure/CannotDecodeVersionString.java
java/src/main/java/org/dhall/binary/decoding/failure/CBORIsNotDhall.java
java/src/main/java/org/dhall/binary/decoding/failure/UnsupportedVersionString.java
java/src/main/java/org/dhall/core/Binding.java
java/src/main/java/org/dhall/core/Chunks.java
java/src/main/java/org/dhall/core/Const.java
java/src/main/java/org/dhall/core/Expr.java
java/src/main/java/org/dhall/core/Import.java
java/src/main/java/org/dhall/core/Normalizer.java
java/src/main/java/org/dhall/core/Var.java
java/src/main/java/org/dhall/core/constant/Kind.java
java/src/main/java/org/dhall/core/constant/Sort.java
java/src/main/java/org/dhall/core/constant/Type.java
java/src/main/java/org/dhall/core/expr/ExprAnnot.java
java/src/main/java/org/dhall/core/expr/ExprApp.java
java/src/main/java/org/dhall/core/expr/ExprBinary.java
java/src/main/java/org/dhall/core/expr/ExprBool.java
java/src/main/java/org/dhall/core/expr/ExprBoolAnd.java
java/src/main/java/org/dhall/core/expr/ExprBoolEQ.java
java/src/main/java/org/dhall/core/expr/ExprBoolIf.java
java/src/main/java/org/dhall/core/expr/ExprBoolLit.java
java/src/main/java/org/dhall/core/expr/ExprBoolNE.java
java/src/main/java/org/dhall/core/expr/ExprBoolOr.java
java/src/main/java/org/dhall/core/expr/ExprCombine.java
java/src/main/java/org/dhall/core/expr/ExprCombineTypes.java
java/src/main/java/org/dhall/core/expr/ExprConst.java
java/src/main/java/org/dhall/core/expr/ExprConstructors.java
java/src/main/java/org/dhall/core/expr/ExprDouble.java
java/src/main/java/org/dhall/core/expr/ExprDoubleLit.java
java/src/main/java/org/dhall/core/expr/ExprDoubleShow.java
java/src/main/java/org/dhall/core/expr/ExprEmbed.java
java/src/main/java/org/dhall/core/expr/ExprField.java
java/src/main/java/org/dhall/core/expr/ExprImportAlt.java
java/src/main/java/org/dhall/core/expr/ExprInteger.java
java/src/main/java/org/dhall/core/expr/ExprIntegerLit.java
java/src/main/java/org/dhall/core/expr/ExprIntegerShow.java
java/src/main/java/org/dhall/core/expr/ExprIntegerToDouble.java
java/src/main/java/org/dhall/core/expr/ExprLam.java
java/src/main/java/org/dhall/core/expr/ExprLet.java
java/src/main/java/org/dhall/core/expr/ExprList.java
java/src/main/java/org/dhall/core/expr/ExprListAppend.java
java/src/main/java/org/dhall/core/expr/ExprListBuild.java
java/src/main/java/org/dhall/core/expr/ExprListFold.java
java/src/main/java/org/dhall/core/expr/ExprListHead.java
java/src/main/java/org/dhall/core/expr/ExprListIndexed.java
java/src/main/java/org/dhall/core/expr/ExprListLast.java
java/src/main/java/org/dhall/core/expr/ExprListLength.java
java/src/main/java/org/dhall/core/expr/ExprListLit.java
java/src/main/java/org/dhall/core/expr/ExprListReverse.java
java/src/main/java/org/dhall/core/expr/ExprMerge.java
java/src/main/java/org/dhall/core/expr/ExprNatural.java
java/src/main/java/org/dhall/core/expr/ExprNaturalBuild.java
java/src/main/java/org/dhall/core/expr/ExprNaturalEven.java
java/src/main/java/org/dhall/core/expr/ExprNaturalFold.java
java/src/main/java/org/dhall/core/expr/ExprNaturalIsZero.java
java/src/main/java/org/dhall/core/expr/ExprNaturalLit.java
java/src/main/java/org/dhall/core/expr/ExprNaturalOdd.java
java/src/main/java/org/dhall/core/expr/ExprNaturalPlus.java
java/src/main/java/org/dhall/core/expr/ExprNaturalShow.java
java/src/main/java/org/dhall/core/expr/ExprNaturalTimes.java
java/src/main/java/org/dhall/core/expr/ExprNaturalToInteger.java
java/src/main/java/org/dhall/core/expr/ExprNone.java
java/src/main/java/org/dhall/core/expr/ExprNote.java
java/src/main/java/org/dhall/core/expr/ExprOptional.java
java/src/main/java/org/dhall/core/expr/ExprOptionalBuild.java
java/src/main/java/org/dhall/core/expr/ExprOptionalFold.java
java/src/main/java/org/dhall/core/expr/ExprOptionalLit.java
java/src/main/java/org/dhall/core/expr/ExprPi.java
java/src/main/java/org/dhall/core/expr/ExprPrefer.java
java/src/main/java/org/dhall/core/expr/ExprProject.java
java/src/main/java/org/dhall/core/expr/ExprRecord.java
java/src/main/java/org/dhall/core/expr/ExprRecordLit.java
java/src/main/java/org/dhall/core/expr/ExprSome.java
java/src/main/java/org/dhall/core/expr/ExprText.java
java/src/main/java/org/dhall/core/expr/ExprTextAppend.java
java/src/main/java/org/dhall/core/expr/ExprTextLit.java
java/src/main/java/org/dhall/core/expr/ExprUnary.java
java/src/main/java/org/dhall/core/expr/ExprUnion.java
java/src/main/java/org/dhall/core/expr/ExprUnionLit.java
java/src/main/java/org/dhall/core/expr/ExprVar.java
java/src/main/java/org/dhall/core/expr/Literal.java
java/src/main/java/org/dhall/core/expr/OptionallyTyped.java
java/src/main/java/org/dhall/core/expr/Typed.java
java/src/main/java/org/dhall/core/imports/Directory.java
java/src/main/java/org/dhall/core/imports/File.java
java/src/main/java/org/dhall/core/imports/FilePrefix.java
java/src/main/java/org/dhall/core/imports/ImportHashed.java
java/src/main/java/org/dhall/core/imports/ImportMode.java
java/src/main/java/org/dhall/core/imports/ImportType.java
java/src/main/java/org/dhall/core/imports/hashed/Digest.java
java/src/main/java/org/dhall/core/imports/hashed/SHA256.java
java/src/main/java/org/dhall/core/imports/types/Env.java
java/src/main/java/org/dhall/core/imports/types/Local.java
java/src/main/java/org/dhall/core/imports/types/Missing.java
java/src/main/java/org/dhall/core/imports/types/Remote.java
java/src/main/java/org/dhall/core/imports/types/URL.java
java/src/main/java/org/dhall/core/imports/types/url/Scheme.java
java/src/main/java/org/dhall/imports/Status.java
java/src/main/java/org/dhall/parser/Pos.java
java/src/main/java/org/dhall/parser/SourcePos.java
java/src/main/java/org/dhall/parser/Src.java
java/src/main/java/org/dhall/parser/error/ParseError.java
java/src/main/java/org/dhall/typecheck/DetailedTypeError.java
java/src/main/java/org/dhall/typecheck/TypeError.java
java/src/main/java/org/dhall/typecheck/TypeMessage.java
java/src/main/java/org/dhall/typecheck/message/AnnotatedExpressionMessage.java
java/src/main/java/org/dhall/typecheck/message/AnnotatedExpressionMismatchMessage.java
java/src/main/java/org/dhall/typecheck/message/AnnotatedNameMessage.java
java/src/main/java/org/dhall/typecheck/message/AnnotationMessage.java
java/src/main/java/org/dhall/typecheck/message/AnnotMismatch.java
java/src/main/java/org/dhall/typecheck/message/CantAdd.java
java/src/main/java/org/dhall/typecheck/message/CantAnd.java
java/src/main/java/org/dhall/typecheck/message/CantEQ.java
java/src/main/java/org/dhall/typecheck/message/CantInterpolate.java
java/src/main/java/org/dhall/typecheck/message/CantListAppend.java
java/src/main/java/org/dhall/typecheck/message/CantMultiply.java
java/src/main/java/org/dhall/typecheck/message/CantNE.java
java/src/main/java/org/dhall/typecheck/message/CantOr.java
java/src/main/java/org/dhall/typecheck/message/CantTextAppend.java
java/src/main/java/org/dhall/typecheck/message/CombineTypesRequiresRecordType.java
java/src/main/java/org/dhall/typecheck/message/ConstructorsRequiresAUnionType.java
java/src/main/java/org/dhall/typecheck/message/DuplicateAlternative.java
java/src/main/java/org/dhall/typecheck/message/FieldAnnotationMismatch.java
java/src/main/java/org/dhall/typecheck/message/FieldCollision.java
java/src/main/java/org/dhall/typecheck/message/FieldMismatch.java
java/src/main/java/org/dhall/typecheck/message/HandlerInputTypeMismatch.java
java/src/main/java/org/dhall/typecheck/message/HandlerNotAFunction.java
java/src/main/java/org/dhall/typecheck/message/HandlerOutputTypeMismatch.java
java/src/main/java/org/dhall/typecheck/message/IfBranchMismatch.java
java/src/main/java/org/dhall/typecheck/message/IfBranchMustBeTerm.java
java/src/main/java/org/dhall/typecheck/message/InvalidAlternative.java
java/src/main/java/org/dhall/typecheck/message/InvalidAlternativeType.java
java/src/main/java/org/dhall/typecheck/message/InvalidField.java
java/src/main/java/org/dhall/typecheck/message/InvalidFieldType.java
java/src/main/java/org/dhall/typecheck/message/InvalidHandlerOutputType.java
java/src/main/java/org/dhall/typecheck/message/InvalidInputType.java
java/src/main/java/org/dhall/typecheck/message/InvalidListElement.java
java/src/main/java/org/dhall/typecheck/message/InvalidListType.java
java/src/main/java/org/dhall/typecheck/message/InvalidOptionalElement.java
java/src/main/java/org/dhall/typecheck/message/InvalidOptionalType.java
java/src/main/java/org/dhall/typecheck/message/InvalidOutputType.java
java/src/main/java/org/dhall/typecheck/message/InvalidPredicate.java
java/src/main/java/org/dhall/typecheck/message/InvalidSome.java
java/src/main/java/org/dhall/typecheck/message/ListAppendMismatch.java
java/src/main/java/org/dhall/typecheck/message/MismatchedListElements.java
java/src/main/java/org/dhall/typecheck/message/MissingField.java
java/src/main/java/org/dhall/typecheck/message/MissingHandler.java
java/src/main/java/org/dhall/typecheck/message/MissingListType.java
java/src/main/java/org/dhall/typecheck/message/MissingMergeType.java
java/src/main/java/org/dhall/typecheck/message/MustCombineARecord.java
java/src/main/java/org/dhall/typecheck/message/MustMergeARecord.java
java/src/main/java/org/dhall/typecheck/message/MustMergeUnion.java
java/src/main/java/org/dhall/typecheck/message/NameMessage.java
java/src/main/java/org/dhall/typecheck/message/NoDependentTypes.java
java/src/main/java/org/dhall/typecheck/message/NotAFunction.java
java/src/main/java/org/dhall/typecheck/message/RecordMismatch.java
java/src/main/java/org/dhall/typecheck/message/RecordTypeMismatch.java
java/src/main/java/org/dhall/typecheck/message/TypeMismatch.java
java/src/main/java/org/dhall/typecheck/message/UnboundVariable.java
java/src/main/java/org/dhall/typecheck/message/Untyped.java
java/src/main/java/org/dhall/typecheck/message/UnusedHandler.java
java/src/main/java/org/haskell/types/Either.java
java/src/main/java/org/haskell/types/Natural.java
java/src/main/java/org/haskell/types/NonEmptyArrayList.java
java/src/main/java/org/haskell/types/Pair.java
java/src/main/java/org/haskell/types/Unit.java
java/src/main/java/org/haskell/types/either/Left.java
java/src/main/java/org/haskell/types/either/Right.java
java/src/main/java/org/haskell/types/functor/Identity.java