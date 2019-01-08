package org.dhall.typecheck;

import java.util.function.Function;

import org.dhall.typecheck.message.AnnotMismatch;
import org.dhall.typecheck.message.CantAccess;
import org.dhall.typecheck.message.CantAdd;
import org.dhall.typecheck.message.CantEQ;
import org.dhall.typecheck.message.CantInterpolate;
import org.dhall.typecheck.message.CantListAppend;
import org.dhall.typecheck.message.CantMultiply;
import org.dhall.typecheck.message.CantNE;
import org.dhall.typecheck.message.CantOr;
import org.dhall.typecheck.message.CantProject;
import org.dhall.typecheck.message.CantTextAppend;
import org.dhall.typecheck.message.CombineTypesRequiresRecordType;
import org.dhall.typecheck.message.ConstructorsRequiresAUnionType;
import org.dhall.typecheck.message.DuplicateAlternative;
import org.dhall.typecheck.message.FieldAnnotationMismatch;
import org.dhall.typecheck.message.FieldCollision;
import org.dhall.typecheck.message.FieldMismatch;
import org.dhall.typecheck.message.HandlerInputTypeMismatch;
import org.dhall.typecheck.message.HandlerNotAFunction;
import org.dhall.typecheck.message.HandlerOutputTypeMismatch;
import org.dhall.typecheck.message.IfBranchMismatch;
import org.dhall.typecheck.message.IfBranchMustBeTerm;
import org.dhall.typecheck.message.InvalidAlternative;
import org.dhall.typecheck.message.InvalidAlternativeType;
import org.dhall.typecheck.message.InvalidField;
import org.dhall.typecheck.message.InvalidFieldType;
import org.dhall.typecheck.message.InvalidHandlerOutputType;
import org.dhall.typecheck.message.InvalidInputType;
import org.dhall.typecheck.message.InvalidListElement;
import org.dhall.typecheck.message.InvalidListType;
import org.dhall.typecheck.message.InvalidOptionalElement;
import org.dhall.typecheck.message.InvalidOptionalType;
import org.dhall.typecheck.message.InvalidOutputType;
import org.dhall.typecheck.message.InvalidPredicate;
import org.dhall.typecheck.message.InvalidSome;
import org.dhall.typecheck.message.ListAppendMismatch;
import org.dhall.typecheck.message.MismatchedListElements;
import org.dhall.typecheck.message.MissingField;
import org.dhall.typecheck.message.MissingHandler;
import org.dhall.typecheck.message.MustCombineARecord;
import org.dhall.typecheck.message.MustMergeARecord;
import org.dhall.typecheck.message.NoDependentTypes;
import org.dhall.typecheck.message.NotAFunction;
import org.dhall.typecheck.message.RecordMismatch;
import org.dhall.typecheck.message.RecordTypeMismatch;
import org.dhall.typecheck.message.TypeMismatch;
import org.dhall.typecheck.message.UnboundVariable;
import org.dhall.typecheck.message.Untyped;
import org.dhall.typecheck.message.UnusedHandler;

public abstract class TypeMessage<S,A> {

    public <R> R matcher(Matcher<S,A,R> m) {
        return m.apply(this);
    }

    public static class Matcher<S,A,R> implements Function<TypeMessage<S,A>,R>{
        
    	private Function<AnnotMismatch<S, A>, R> annotMismatch;
    	private Function<CantAccess<S, A>, R> cantAccess;
    	private Function<CantAdd<S, A>, R> cantAdd;
    	private Function<CantEQ<S, A>, R> cantEQ;
    	private Function<CantInterpolate<S, A>, R> cantInterpolate;
    	private Function<CantListAppend<S, A>, R> cantListAppend;
    	private Function<CantMultiply<S, A>, R> cantMultiply;
    	private Function<CantNE<S, A>, R> cantNE;
    	private Function<CantOr<S, A>, R> cantOr;
    	private Function<CantProject<S, A>, R> cantProject;
    	private Function<CantTextAppend<S, A>, R> cantTextAppend;
    	private Function<CombineTypesRequiresRecordType<S, A>, R> combineTypesRequiresRecordType;
    	private Function<ConstructorsRequiresAUnionType<S, A>, R> constructorsRequiresAUnionType;
    	private Function<DuplicateAlternative<S, A>, R> duplicateAlternative;
    	private Function<FieldAnnotationMismatch<S, A>,R> fieldAnnotationMismatch;
    	private Function<FieldCollision<S, A>, R> fieldCollision;
    	private Function<FieldMismatch<S, A>, R> fieldMismatch;
    	private Function<HandlerInputTypeMismatch<S, A>, R> handlerInputTypeMismatch;
    	private Function<HandlerNotAFunction<S, A>, R> handlerNotAFunction;
    	private Function<HandlerOutputTypeMismatch<S, A>, R> handlerOutputTypeMismatch;
    	private Function<IfBranchMismatch<S, A>, R> ifBranchMismatch;
    	private Function<IfBranchMustBeTerm<S, A>, R> ifBranchMustBeTerm;
    	private Function<InvalidAlternative<S, A>, R> invalidAlternative;
    	private Function<InvalidAlternativeType<S, A>, R> invalidAlternativeType;
    	private Function<InvalidField<S, A>, R> invalidField;
    	private Function<InvalidFieldType<S, A>, R> invalidFieldType;
    	private Function<InvalidHandlerOutputType<S, A>, R> invalidHandlerOutputType;
    	private Function<InvalidInputType<S, A>, R> invalidInputType;
    	private Function<InvalidListElement<S, A>, R> invalidListElement;
    	private Function<InvalidListType<S, A>, R> invalidListType;
    	private Function<InvalidOptionalElement<S, A>, R> invalidOptionalElement;
    	private Function<InvalidOptionalType<S, A>,R> invalidOptionalType;
    	private Function<InvalidOutputType<S, A>, R> invalidOutputType;
    	private Function<InvalidPredicate<S, A>, R> invalidPredicate;
    	private Function<InvalidSome<S, A>, R> invalidSome;
    	private Function<ListAppendMismatch<S, A>, R> listAppendMismatch;
    	private Function<MismatchedListElements<S, A>, R> mismatchedListElements;
    	private Function<MissingField<S, A>, R> missingField;
    	private Function<MissingHandler<S, A>, R> missingHandler;
    	private Function<MustCombineARecord<S, A>, R> mustCombineARecord;
    	private Function<MustMergeARecord<S, A>, R> mustMergeARecord;
    	private Function<NoDependentTypes<S, A>, R> noDependentTypes;
    	private Function<NotAFunction<S, A>, R> notAFunction;
    	private Function<RecordMismatch<S, A>, R> recordMismatch;
    	private Function<RecordTypeMismatch<S, A>,R> recordTypeMismatch;
    	private Function<TypeMismatch<S, A>, R> typeMismatch;
    	private Function<UnboundVariable<S, A>, R> unboundVariable;
    	private Function<Untyped<S, A>, R> untyped;
    	private Function<UnusedHandler<S, A>, R> unusedHandler;
		private final Function<? super TypeMessage<S, A>, R> _any;
    	
    	public Matcher(Function<? super TypeMessage<S, A>, R> _any) {
			super();
			this._any = _any;
		}

		public Matcher<S, A, R> AnnotMismatch(Function<AnnotMismatch<S, A>, R> annotMismatch) {
    		this.annotMismatch=annotMismatch;
    		return this;
    	}

		public Matcher<S, A, R> CantAccess(Function<CantAccess<S, A>, R> cantAccess) {
			this.cantAccess = cantAccess;
			return this;
		}

		public Matcher<S, A, R> CantAdd(Function<CantAdd<S, A>, R> cantAdd) {
			this.cantAdd = cantAdd;
			return this;
		}

		public Matcher<S, A, R> CantEQ(Function<CantEQ<S, A>, R> cantEQ) {
			this.cantEQ = cantEQ;
			return this;
		}

		public Matcher<S, A, R> CantInterpolate(Function<CantInterpolate<S, A>, R> cantInterpolate) {
			this.cantInterpolate = cantInterpolate;
			return this;
		}

		public Matcher<S, A, R> CantListAppend(Function<CantListAppend<S, A>, R> cantListAppend) {
			this.cantListAppend = cantListAppend;
			return this;
		}

		public Matcher<S, A, R> CantMultiply(Function<CantMultiply<S, A>, R> cantMultiply) {
			this.cantMultiply = cantMultiply;
			return this;
		}

		public Matcher<S, A, R> CantNE(Function<CantNE<S, A>, R> cantNE) {
			this.cantNE = cantNE;
			return this;
		}

		public Matcher<S, A, R> CantOr(Function<CantOr<S, A>, R> cantOr) {
			this.cantOr = cantOr;
			return this;
		}

		public Matcher<S, A, R> CantProject(Function<CantProject<S, A>, R> cantProject) {
			this.cantProject = cantProject;
			return this;
		}

		public Matcher<S, A, R> CantTextAppend(Function<CantTextAppend<S, A>, R> cantTextAppend) {
			this.cantTextAppend = cantTextAppend;
			return this;
		}

		public Matcher<S, A, R> CombineTypesRequiresRecordType(
				Function<CombineTypesRequiresRecordType<S, A>, R> combineTypesRequiresRecordType) {
			this.combineTypesRequiresRecordType = combineTypesRequiresRecordType;
			return this;
		}

		public Matcher<S, A, R> ConstructorsRequiresAUnionType(
				Function<ConstructorsRequiresAUnionType<S, A>, R> constructorsRequiresAUnionType) {
			this.constructorsRequiresAUnionType = constructorsRequiresAUnionType;
			return this;
		}

		public Matcher<S, A, R> DuplicateAlternative(Function<DuplicateAlternative<S, A>, R> duplicateAlternative) {
			this.duplicateAlternative = duplicateAlternative;
			return this;
		}

		public Matcher<S, A, R> FieldAnnotationMismatch(
				Function<FieldAnnotationMismatch<S, A>, R> fieldAnnotationMismatch) {
			this.fieldAnnotationMismatch = fieldAnnotationMismatch;
			return this;
		}

		public Matcher<S, A, R> FieldCollision(Function<FieldCollision<S, A>, R> fieldCollision) {
			this.fieldCollision = fieldCollision;
			return this;
		}

		public Matcher<S, A, R> FieldMismatch(Function<FieldMismatch<S, A>, R> fieldMismatch) {
			this.fieldMismatch = fieldMismatch;
			return this;
		}

		public Matcher<S, A, R> HandlerInputTypeMismatch(
				Function<HandlerInputTypeMismatch<S, A>, R> handlerInputTypeMismatch) {
			this.handlerInputTypeMismatch = handlerInputTypeMismatch;
			return this;
		}

		public Matcher<S, A, R> HandlerNotAFunction(Function<HandlerNotAFunction<S, A>, R> handlerNotAFunction) {
			this.handlerNotAFunction = handlerNotAFunction;
			return this;
		}

		public Matcher<S, A, R> HandlerOutputTypeMismatch(
				Function<HandlerOutputTypeMismatch<S, A>, R> handlerOutputTypeMismatch) {
			this.handlerOutputTypeMismatch = handlerOutputTypeMismatch;
			return this;
		}

		public Matcher<S, A, R> IfBranchMismatch(Function<IfBranchMismatch<S, A>, R> ifBranchMismatch) {
			this.ifBranchMismatch = ifBranchMismatch;
			return this;
		}

		public Matcher<S, A, R> IfBranchMustBeTerm(Function<IfBranchMustBeTerm<S, A>, R> ifBranchMustBeTerm) {
			this.ifBranchMustBeTerm = ifBranchMustBeTerm;
			return this;
		}

		public Matcher<S, A, R> InvalidAlternative(Function<InvalidAlternative<S, A>, R> invalidAlternative) {
			this.invalidAlternative = invalidAlternative;
			return this;
		}

		public Matcher<S, A, R> InvalidAlternativeType(
				Function<InvalidAlternativeType<S, A>, R> invalidAlternativeType) {
			this.invalidAlternativeType = invalidAlternativeType;
			return this;
		}

		public Matcher<S, A, R> InvalidField(Function<InvalidField<S, A>, R> invalidField) {
			this.invalidField = invalidField;
			return this;
		}

		public Matcher<S, A, R> InvalidFieldType(Function<InvalidFieldType<S, A>, R> invalidFieldType) {
			this.invalidFieldType = invalidFieldType;
			return this;
		}

		public Matcher<S, A, R> InvalidHandlerOutputType(
				Function<InvalidHandlerOutputType<S, A>, R> invalidHandlerOutputType) {
			this.invalidHandlerOutputType = invalidHandlerOutputType;
			return this;
		}

		public Matcher<S, A, R> InvalidInputType(Function<InvalidInputType<S, A>, R> invalidInputType) {
			this.invalidInputType = invalidInputType;
			return this;
		}

		public Matcher<S, A, R> InvalidListElement(Function<InvalidListElement<S, A>, R> invalidListElement) {
			this.invalidListElement = invalidListElement;
			return this;
		}

		public Matcher<S, A, R> InvalidListType(Function<InvalidListType<S, A>, R> invalidListType) {
			this.invalidListType = invalidListType;
			return this;
		}

		public Matcher<S, A, R> InvalidOptionalElement(
				Function<InvalidOptionalElement<S, A>, R> invalidOptionalElement) {
			this.invalidOptionalElement = invalidOptionalElement;
			return this;
		}

		public Matcher<S, A, R> InvalidOptionalType(Function<InvalidOptionalType<S, A>, R> invalidOptionalType) {
			this.invalidOptionalType = invalidOptionalType;
			return this;
		}

		public Matcher<S, A, R> InvalidOutputType(Function<InvalidOutputType<S, A>, R> invalidOutputType) {
			this.invalidOutputType = invalidOutputType;
			return this;
		}

		public Matcher<S, A, R> InvalidPredicate(Function<InvalidPredicate<S, A>, R> invalidPredicate) {
			this.invalidPredicate = invalidPredicate;
			return this;
		}

		public Matcher<S, A, R> InvalidSome(Function<InvalidSome<S, A>, R> invalidSome) {
			this.invalidSome = invalidSome;
			return this;
		}

		public Matcher<S, A, R> ListAppendMismatch(Function<ListAppendMismatch<S, A>, R> listAppendMismatch) {
			this.listAppendMismatch = listAppendMismatch;
			return this;
		}

		public Matcher<S, A, R> MismatchedListElements(
				Function<MismatchedListElements<S, A>, R> mismatchedListElements) {
			this.mismatchedListElements = mismatchedListElements;
			return this;
		}

		public Matcher<S, A, R> MissingField(Function<MissingField<S, A>, R> missingField) {
			this.missingField = missingField;
			return this;
		}

		public Matcher<S, A, R> MissingHandler(Function<MissingHandler<S, A>, R> missingHandler) {
			this.missingHandler = missingHandler;
			return this;
		}

		public Matcher<S, A, R> MustCombineARecord(Function<MustCombineARecord<S, A>, R> mustCombineARecord) {
			this.mustCombineARecord = mustCombineARecord;
			return this;
		}

		public Matcher<S, A, R> MustMergeARecord(Function<MustMergeARecord<S, A>, R> mustMergeARecord) {
			this.mustMergeARecord = mustMergeARecord;
			return this;
		}

		public Matcher<S, A, R> NoDependentTypes(Function<NoDependentTypes<S, A>, R> noDependentTypes) {
			this.noDependentTypes = noDependentTypes;
			return this;
		}

		public Matcher<S, A, R> NotAFunction(Function<NotAFunction<S, A>, R> notAFunction) {
			this.notAFunction = notAFunction;
			return this;
		}

		public Matcher<S, A, R> RecordMismatch(Function<RecordMismatch<S, A>, R> recordMismatch) {
			this.recordMismatch = recordMismatch;
			return this;
		}

		public Matcher<S, A, R> RecordTypeMismatch(Function<RecordTypeMismatch<S, A>, R> recordTypeMismatch) {
			this.recordTypeMismatch = recordTypeMismatch;
			return this;
		}

		public Matcher<S, A, R> TypeMismatch(Function<TypeMismatch<S, A>, R> typeMismatch) {
			this.typeMismatch = typeMismatch;
			return this;
		}

		public Matcher<S, A, R> UnboundVariable(Function<UnboundVariable<S, A>, R> unboundVariable) {
			this.unboundVariable = unboundVariable;
			return this;
		}

		public Matcher<S, A, R> Untyped(Function<Untyped<S, A>, R> untyped) {
			this.untyped = untyped;
			return this;
		}

		public Matcher<S, A, R> UnusedHandler(Function<UnusedHandler<S, A>, R> unusedHandler) {
			this.unusedHandler = unusedHandler;
			return this;
		}

		
		public R match(TypeMessage<S,A> instance) {
			if (annotMismatch != null && instance instanceof AnnotMismatch) {
				return annotMismatch.apply((org.dhall.typecheck.message.AnnotMismatch<S, A>) instance);
			} else if (cantAccess != null && instance instanceof CantAccess) {
				return cantAccess.apply((CantAccess<S, A>) instance);
			} else if (cantAdd != null && instance instanceof CantAdd) {
				return cantAdd.apply((CantAdd<S, A>) instance);
			} else if (cantEQ != null && instance instanceof CantEQ) {
				return cantEQ.apply((org.dhall.typecheck.message.CantEQ<S, A>) instance);
			} else if (cantInterpolate != null && instance instanceof CantInterpolate) {
				return cantInterpolate.apply((CantInterpolate<S, A>) instance);
			} else if (cantListAppend != null && instance instanceof CantListAppend) {
				return cantListAppend.apply((CantListAppend<S, A>) instance);
			} else if (cantMultiply != null && instance instanceof CantMultiply) {
				return cantMultiply.apply((CantMultiply<S, A>) instance);
			} else if (cantNE != null && instance instanceof CantNE) {
				return cantNE.apply((CantNE<S, A>) instance);
			} else if (cantOr != null && instance instanceof CantOr) {
				return cantOr.apply((CantOr<S, A>) instance);
			} else if (cantProject != null && instance instanceof CantProject) {
				return cantProject.apply((CantProject<S, A>) instance);
			} else if (cantTextAppend != null && instance instanceof CantTextAppend) {
				return cantTextAppend.apply((CantTextAppend<S, A>) instance);
			} else if (combineTypesRequiresRecordType != null && instance instanceof CombineTypesRequiresRecordType) {
				return combineTypesRequiresRecordType.apply((CombineTypesRequiresRecordType<S, A>) instance);
			} else if (constructorsRequiresAUnionType != null && instance instanceof ConstructorsRequiresAUnionType) {
				return constructorsRequiresAUnionType.apply((ConstructorsRequiresAUnionType<S, A>) instance);
			} else if (duplicateAlternative != null && instance instanceof DuplicateAlternative) {
				return duplicateAlternative.apply((DuplicateAlternative<S, A>) instance);
			} else if (fieldAnnotationMismatch != null && instance instanceof FieldAnnotationMismatch) {
				return fieldAnnotationMismatch.apply((FieldAnnotationMismatch<S, A>) instance);
			} else if (fieldCollision != null && instance instanceof FieldCollision) {
				return fieldCollision.apply((FieldCollision<S, A>) instance);
			} else if (fieldMismatch != null && instance instanceof FieldMismatch) {
				return fieldMismatch.apply((FieldMismatch<S, A>) instance);
			} else if (handlerInputTypeMismatch != null && instance instanceof HandlerInputTypeMismatch) {
				return handlerInputTypeMismatch.apply((HandlerInputTypeMismatch<S, A>) instance);
			} else if (handlerNotAFunction != null && instance instanceof HandlerNotAFunction) {
				return handlerNotAFunction.apply((HandlerNotAFunction<S, A>) instance);
			} else if (handlerOutputTypeMismatch != null && instance instanceof HandlerOutputTypeMismatch) {
				return handlerOutputTypeMismatch.apply((HandlerOutputTypeMismatch<S, A>) instance);
			} else if (ifBranchMismatch != null && instance instanceof IfBranchMismatch) {
				return ifBranchMismatch.apply((IfBranchMismatch<S, A>) instance);
			} else if (ifBranchMustBeTerm != null && instance instanceof IfBranchMustBeTerm) {
				return ifBranchMustBeTerm.apply((IfBranchMustBeTerm<S, A>) instance);
			} else if (invalidAlternative != null && instance instanceof InvalidAlternative) {
				return invalidAlternative.apply((InvalidAlternative<S, A>) instance);
			} else if (invalidAlternativeType != null && instance instanceof InvalidAlternativeType) {
				return invalidAlternativeType.apply((InvalidAlternativeType<S, A>) instance);
			} else if (invalidField != null && instance instanceof InvalidField) {
				return invalidField.apply((InvalidField<S, A>) instance);
			} else if (invalidFieldType != null && instance instanceof InvalidFieldType) {
				return invalidFieldType.apply((InvalidFieldType<S, A>) instance);
			} else if (invalidHandlerOutputType != null && instance instanceof InvalidHandlerOutputType) {
				return invalidHandlerOutputType.apply((InvalidHandlerOutputType<S, A>) instance);
			} else if (invalidInputType != null && instance instanceof InvalidInputType) {
				return invalidInputType.apply((InvalidInputType<S, A>) instance);
			} else if (invalidListElement != null && instance instanceof InvalidListElement) {
				return invalidListElement.apply((InvalidListElement<S, A>) instance);
			} else if (invalidListType != null && instance instanceof InvalidListType) {
				return invalidListType.apply((InvalidListType<S, A>) instance);
			} else if (invalidOptionalElement != null && instance instanceof InvalidOptionalElement) {
				return invalidOptionalElement.apply((InvalidOptionalElement<S, A>) instance);
			} else if (invalidOptionalType != null && instance instanceof InvalidOptionalType) {
				return invalidOptionalType.apply((InvalidOptionalType<S, A>) instance);
			} else if (invalidOutputType != null && instance instanceof InvalidOutputType) {
				return invalidOutputType.apply((InvalidOutputType<S, A>) instance);
			} else if (invalidPredicate != null && instance instanceof InvalidPredicate) {
				return invalidPredicate.apply((InvalidPredicate<S, A>) instance);
			} else if (invalidSome != null && instance instanceof InvalidSome) {
				return invalidSome.apply((InvalidSome<S, A>) instance);
			} else if (listAppendMismatch != null && instance instanceof ListAppendMismatch) {
				return listAppendMismatch.apply((ListAppendMismatch<S, A>) instance);
			} else if (mismatchedListElements != null && instance instanceof MismatchedListElements) {
				return mismatchedListElements.apply((MismatchedListElements<S, A>) instance);
			} else if (missingField != null && instance instanceof MissingField) {
				return missingField.apply((MissingField<S, A>) instance);
			} else if (missingHandler != null && instance instanceof MissingHandler) {
				return missingHandler.apply((MissingHandler<S, A>) instance);
			} else if (mustCombineARecord != null && instance instanceof MustCombineARecord) {
				return mustCombineARecord.apply((MustCombineARecord<S, A>) instance);
			} else if (mustMergeARecord != null && instance instanceof MustMergeARecord) {
				return mustMergeARecord.apply((MustMergeARecord<S, A>) instance);
			} else if (noDependentTypes != null && instance instanceof NoDependentTypes) {
				return noDependentTypes.apply((NoDependentTypes<S, A>) instance);
			} else if (notAFunction != null && instance instanceof NotAFunction) {
				return notAFunction.apply((NotAFunction<S, A>) instance);
			} else if (recordMismatch != null && instance instanceof RecordMismatch) {
				return recordMismatch.apply((RecordMismatch<S, A>) instance);
			} else if (recordTypeMismatch != null && instance instanceof RecordTypeMismatch) {
				return recordTypeMismatch.apply((RecordTypeMismatch<S, A>) instance);
			} else if (typeMismatch != null && instance instanceof TypeMismatch) {
				return typeMismatch.apply((TypeMismatch<S, A>) instance);
			} else if (unboundVariable != null && instance instanceof UnboundVariable) {
				return unboundVariable.apply((UnboundVariable<S, A>) instance);
			} else if (untyped != null && instance instanceof Untyped) {
				return untyped.apply((Untyped<S, A>) instance);
			} else if (unusedHandler != null && instance instanceof UnusedHandler) {
				return unusedHandler.apply((UnusedHandler<S, A>) instance);
			} else {
				return _any.apply(instance);
			} 
		}
		
    	@Override
        public R apply(TypeMessage<S, A> instance) {
            return match(instance);
        }
    }

}
