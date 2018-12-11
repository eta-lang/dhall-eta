package org.dhall.core;

import java.util.function.Function;

import org.dhall.core.expr.*;

public abstract class Expr<S,A> {
   
	public <T> T match(Matcher<S, A, T> m) {
		return m.match(this);
	}
	
	@Override
	public String toString() {
		return getClass().getSimpleName();
	}
	
	public static class Matcher<S,A,T> implements Function<Expr<S,A>,T>{

		private Function<ExprConst<S, A>, T> constant;
		private Function<ExprVar<S, A>, T> var;
		private Function<ExprLam<S, A>, T> lam;
		private Function<ExprPi<S, A>, T> pi;
		private Function<ExprApp<S, A>, T> app;
		private Function<ExprLet<S, A>, T> let;
		private Function<ExprAnnot<S, A>, T> annot;
		
		private Function<ExprBool<S, A>, T> bool;
		private Function<ExprBoolLit<S, A>, T> boolLit;
		private Function<ExprBoolAnd<S, A>, T> boolAnd;
		private Function<ExprBoolOr<S, A>, T> boolOr;
		private Function<ExprBoolEQ<S, A>, T> boolEq;
		private Function<ExprBoolNE<S, A>, T> boolNeq;
		private Function<ExprBoolIf<S, A>, T> boolIf;
		
		private Function<ExprNatural<S, A>, T> natural;
		private Function<ExprNaturalLit<S, A>, T> naturalLit;
		private Function<ExprNaturalFold<S, A>, T> naturalFold;
		private Function<ExprNaturalBuild<S, A>, T> naturalBuild;
		private Function<ExprNaturalIsZero<S, A>, T> naturalIsZero;
		private Function<ExprNaturalEven<S, A>, T> naturalEven;
		private Function<ExprNaturalOdd<S, A>, T> naturalOdd;
		private Function<ExprNaturalToInteger<S, A>, T> naturalToInteger;
		private Function<ExprNaturalShow<S, A>, T> naturalShow;
		private Function<ExprNaturalPlus<S, A>, T> naturalPlus;
		private Function<ExprNaturalTimes<S, A>, T> naturalTimes;
		
		private Function<ExprInteger<S, A>, T> integer;
		private Function<ExprIntegerLit<S, A>, T> integerLit;
		private Function<ExprIntegerShow<S, A>, T> integerShow;
		private Function<ExprIntegerToDouble<S, A>, T> integerToDouble;
		
		private Function<ExprDouble<S, A>, T> doubleTy;
		private Function<ExprDoubleLit <S, A>, T> doubleLit ;
		private Function<ExprDoubleShow<S, A>, T> doubleShow;
		
		private Function<ExprText<S, A>, T> text;
		private Function<ExprTextLit <S, A>, T> textLit ;
		private Function<ExprTextAppend <S, A>, T> textAppend ;
		
		private Function<ExprList<S, A>, T> list;
		private Function<ExprListLit <S, A>, T> listLit ;
		private Function<ExprListAppend <S, A>, T> listAppend ;
		private Function<ExprListBuild<S, A>, T> listBuild;
		private Function<ExprListFold<S, A>, T> listFold;
		private Function<ExprListLength<S, A>, T> listLength;
        private Function<ExprListHead<S, A>, T> listHead;
		private Function<ExprListLast<S, A>, T> listLast;
		private Function<ExprListIndexed<S, A>, T> listIndexed;
		private Function<ExprListReverse<S, A>, T> listReverse;
		
		private Function<ExprOptional<S, A>, T> optional;
		private Function<ExprOptionalLit<S, A>, T> optionalLit;
		private Function<ExprOptionalFold<S, A>, T> optionalFold;
		private Function<ExprOptionalBuild<S, A>, T> optionalBuild;
		
		private Function<ExprRecord<S, A>, T> record;
		private Function<ExprRecordLit<S, A>, T> recordLit;
		
		private Function<ExprUnion<S, A>, T> union;
		private Function<ExprUnionLit<S, A>, T> unionLit;
		private Function<ExprCombine<S, A>, T> combine;
		private Function<ExprCombineTypes<S, A>, T> combineTypes;
		private Function<ExprPrefer<S, A>, T> prefer;
		private Function<ExprMerge<S, A>, T> merge;
		
		private Function<ExprConstructors<S, A>, T> constructors;
		private Function<ExprField<S, A>, T> field;
		private Function<ExprProject<S, A>, T> project;
		private Function<ExprNote<S, A>, T> note;
		private Function<ExprImportAlt<S, A>, T> importAlt;
		private Function<ExprEmbed<S, A>, T> embed;
		
		private final Function<? super Expr<S,A>, T> any;

		public Matcher(Function<? super Expr<S,A>, T> any) {
			this.any = any;
		}

		public Matcher<S,A,T> Const(Function<ExprConst<S, A>, T> f) {
			this.constant = f;
			return this;
		}

		public Matcher<S,A,T> Var(Function<ExprVar<S, A>, T> f) {
			this.var = f;
			return this;
		}

		public Matcher<S,A,T> Lam(Function<ExprLam<S, A>, T> f) {
			this.lam = f;
			return this;
		}
		
		public Matcher<S,A,T> Pi(Function<ExprPi<S, A>, T> f) {
			this.pi = f;
			return this;
		}
		
		public Matcher<S,A,T> App(Function<ExprApp<S, A>, T> f) {
			this.app = f;
			return this;
		}
		
		public Matcher<S,A,T> Let(Function<ExprLet<S, A>, T> f) {
			this.let = f;
			return this;
		}
		
		public Matcher<S,A,T> Annot(Function<ExprAnnot<S, A>, T> f) {
			this.annot = f;
			return this;
		}
		
		public Matcher<S,A,T> Bool(Function<ExprBool<S, A>, T> f) {
			this.bool = f;
			return this;
		}
		
		public Matcher<S,A,T> BoolLit(Function<ExprBoolLit<S, A>, T> f) {
			this.boolLit = f;
			return this;
		}
		
		public Matcher<S,A,T> BoolAnd(Function<ExprBoolAnd<S, A>, T> f) {
			this.boolAnd = f;
			return this;
		}
		
		public Matcher<S,A,T> BoolOr(Function<ExprBoolOr<S, A>, T> f) {
			this.boolOr = f;
			return this;
		}
		
		public Matcher<S,A,T> BoolEQ(Function<ExprBoolEQ<S, A>, T> f) {
			this.boolEq = f;
			return this;
		}
		
		public Matcher<S,A,T> BoolNE(Function<ExprBoolNE<S, A>, T> f) {
			this.boolNeq = f;
			return this;
		}
		
		public Matcher<S,A,T> BoolIf(Function<ExprBoolIf<S, A>, T> f) {
			this.boolIf = f;
			return this;
		}
		
		public Matcher<S,A,T> Natural(Function<ExprNatural<S, A>, T> f) {
			this.natural = f;
			return this;
		}
		
		public Matcher<S,A,T> NaturalLit(Function<ExprNaturalLit<S, A>, T> f) {
			this.naturalLit = f;
			return this;
		}
		
		public Matcher<S,A,T> NaturalFold(Function<ExprNaturalFold<S, A>, T> f) {
			this.naturalFold = f;
			return this;
		}
		
		public Matcher<S,A,T> NaturalBuild(Function<ExprNaturalBuild<S, A>, T> f) {
			this.naturalBuild = f;
			return this;
		}
		
		public Matcher<S,A,T> NaturalIsZero(Function<ExprNaturalIsZero<S, A>, T> f) {
			this.naturalIsZero = f;
			return this;
		}
		
		public Matcher<S,A,T> NaturalEven(Function<ExprNaturalEven<S, A>, T> f) {
			this.naturalEven = f;
			return this;
		}
		
		public Matcher<S,A,T> NaturalOdd(Function<ExprNaturalOdd<S, A>, T> f) {
			this.naturalOdd = f;
			return this;
		}
		
		public Matcher<S,A,T> NaturalToInteger(Function<ExprNaturalToInteger<S, A>, T> f) {
			this.naturalToInteger = f;
			return this;
		}
		
		public Matcher<S,A,T> NaturalShow(Function<ExprNaturalShow<S, A>, T> f) {
			this.naturalShow = f;
			return this;
		}
		
		public Matcher<S,A,T> NaturalPlus(Function<ExprNaturalPlus<S, A>, T> f) {
			this.naturalPlus = f;
			return this;
		}
		
		public Matcher<S,A,T> NaturalTimes(Function<ExprNaturalTimes<S, A>, T> f) {
			this.naturalTimes = f;
			return this;
		}
		
		public Matcher<S,A,T> Integer(Function<ExprInteger<S, A>, T> f) {
			this.integer = f;
			return this;
		}
		
		public Matcher<S,A,T> IntegerLit(Function<ExprIntegerLit<S, A>, T> f) {
			this.integerLit = f;
			return this;
		}
		
		public Matcher<S,A,T> IntegerShow(Function<ExprIntegerShow<S, A>, T> f) {
			this.integerShow = f;
			return this;
		}
		
		public Matcher<S,A,T> IntegerToDouble(Function<ExprIntegerToDouble<S, A>, T> f) {
			this.integerToDouble = f;
			return this;
		}
		
		public Matcher<S,A,T> Double(Function<ExprDouble<S, A>, T> f) {
			this.doubleTy = f;
			return this;
		}
		
		public Matcher<S,A,T> DoubleLit(Function<ExprDoubleLit<S, A>, T> f) {
			this.doubleLit = f;
			return this;
		}
		
		public Matcher<S,A,T> DoubleShow(Function<ExprDoubleShow<S, A>, T> f) {
			this.doubleShow = f;
			return this;
		}
		
		public Matcher<S,A,T> Text(Function<ExprText<S, A>, T> f) {
			this.text = f;
			return this;
		}
		
		public Matcher<S,A,T> TextLit(Function<ExprTextLit<S, A>, T> f) {
			this.textLit = f;
			return this;
		}
		
		public Matcher<S,A,T> ExprTextAppend(Function<ExprTextAppend<S, A>, T> f) {
			this.textAppend = f;
			return this;
		}
		
		public Matcher<S,A,T> List(Function<ExprList<S, A>, T> f) {
			this.list = f;
			return this;
		}
		
		public Matcher<S,A,T> ListLit(Function<ExprListLit<S, A>, T> f) {
			this.listLit = f;
			return this;
		}
		
		public Matcher<S,A,T> ListAppend(Function<ExprListAppend<S, A>, T> f) {
			this.listAppend = f;
			return this;
		}
		
		public Matcher<S,A,T> ListBuild(Function<ExprListBuild<S, A>, T> f) {
			this.listBuild = f;
			return this;
		}
		
		public Matcher<S,A,T> ListFold(Function<ExprListFold<S, A>, T> f) {
			this.listFold = f;
			return this;
		}
		
		public Matcher<S,A,T> ListLength(Function<ExprListLength<S, A>, T> f) {
			this.listLength = f;
			return this;
		}
		
		public Matcher<S,A,T> ListLast(Function<ExprListLast<S, A>, T> f) {
			this.listLast = f;
			return this;
		}
		
		public Matcher<S,A,T> ListIndexed(Function<ExprListIndexed<S, A>, T> f) {
			this.listIndexed = f;
			return this;
		}
		
		public Matcher<S,A,T> ListReverse(Function<ExprListReverse<S, A>, T> f) {
			this.listReverse = f;
			return this;
		}
		
		public Matcher<S,A,T> Optional(Function<ExprOptional<S, A>, T> f) {
			this.optional = f;
			return this;
		}
		
		public Matcher<S,A,T> OptionalLit(Function<ExprOptionalLit<S, A>, T> f) {
			this.optionalLit = f;
			return this;
		}
		
		public Matcher<S,A,T> OptionalFold(Function<ExprOptionalFold<S, A>, T> f) {
			this.optionalFold = f;
			return this;
		}
		
		public Matcher<S,A,T> OptionalBuild(Function<ExprOptionalBuild<S, A>, T> f) {
			this.optionalBuild = f;
			return this;
		}

		public Matcher<S,A,T> Record(Function<ExprRecord<S, A>, T> f) {
			this.record = f;
			return this;
		}
		
		public Matcher<S,A,T> RecordLit(Function<ExprRecordLit<S, A>, T> f) {
			this.recordLit = f;
			return this;
		}
		
		public Matcher<S,A,T> Union(Function<ExprUnion<S, A>, T> f) {
			this.union = f;
			return this;
		}
		
		public Matcher<S,A,T> UnionLit(Function<ExprUnionLit<S, A>, T> f) {
			this.unionLit = f;
			return this;
		}
		
		public Matcher<S,A,T> Combine(Function<ExprCombine<S, A>, T> f) {
			this.combine = f;
			return this;
		}
		
		public Matcher<S,A,T> CombineTypes(Function<ExprCombineTypes<S, A>, T> f) {
			this.combineTypes = f;
			return this;
		}
		
		public Matcher<S,A,T> Prefer(Function<ExprPrefer<S, A>, T> f) {
			this.prefer = f;
			return this;
		}
		
		public Matcher<S,A,T> Merge(Function<ExprMerge<S, A>, T> f) {
			this.merge = f;
			return this;
		}
		
		public Matcher<S,A,T> Constructors(Function<ExprConstructors<S, A>, T> f) {
			this.constructors = f;
			return this;
		}
		
		public Matcher<S,A,T> Field(Function<ExprField<S, A>, T> f) {
			this.field = f;
			return this;
		}
		
		public Matcher<S,A,T> Project(Function<ExprProject<S, A>, T> f) {
			this.project = f;
			return this;
		}
		
		public Matcher<S,A,T> Note(Function<ExprNote<S, A>, T> f) {
			this.note = f;
			return this;
		}
		
		public Matcher<S,A,T> ImportAlt(Function<ExprImportAlt<S, A>, T> f) {
			this.importAlt = f;
			return this;
		}
		
		public Matcher<S,A,T> Embed(Function<ExprEmbed<S, A>, T> f) {
			this.embed = f;
			return this;
		}
		
		
		public T match(Expr<S,A> instance) {
			if (constant != null && instance instanceof ExprConst)
				return constant.apply((ExprConst<S,A>) instance);
			else if (var != null && instance instanceof ExprVar)
				return var.apply((ExprVar<S,A>) instance);
			else if (lam != null && instance instanceof ExprLam)
				return lam.apply((ExprLam<S,A>) instance);
			else if (pi != null && instance instanceof ExprPi)
				return pi.apply((ExprPi<S,A>) instance);
			else if (app != null && instance instanceof ExprApp)
				return app.apply((ExprApp<S,A>) instance);
			else if (let != null && instance instanceof ExprLet)
				return let.apply((ExprLet<S,A>) instance);
			else if (annot != null && instance instanceof ExprAnnot)
				return annot.apply((ExprAnnot<S,A>) instance);
			else if (bool != null && instance instanceof ExprBool)
				return bool.apply((ExprBool<S,A>) instance);
			else if (boolLit != null && instance instanceof ExprBoolLit)
				return boolLit.apply((ExprBoolLit<S,A>) instance);
			else if (boolAnd != null && instance instanceof ExprBoolAnd)
				return boolAnd.apply((ExprBoolAnd<S,A>) instance);
			else if (boolOr != null && instance instanceof ExprBoolOr)
				return boolOr.apply((ExprBoolOr<S,A>) instance);
			else if (boolEq != null && instance instanceof ExprBoolEQ)
				return boolEq.apply((ExprBoolEQ<S,A>) instance);
			else if (boolNeq != null && instance instanceof ExprBoolNE)
				return boolNeq.apply((ExprBoolNE<S,A>) instance);
			else if (boolIf != null && instance instanceof ExprBoolIf)
				return boolIf.apply((ExprBoolIf<S,A>) instance);
			else if (natural != null && instance instanceof ExprNatural)
				return natural.apply((ExprNatural<S,A>) instance);
			else if (naturalLit != null && instance instanceof ExprNaturalLit)
				return naturalLit.apply((ExprNaturalLit<S,A>) instance);
			else if (naturalFold != null && instance instanceof ExprNaturalFold)
				return naturalFold.apply((ExprNaturalFold<S,A>) instance);
			else if (naturalBuild != null && instance instanceof ExprNaturalBuild)
				return naturalBuild.apply((ExprNaturalBuild<S,A>) instance);
			else if (naturalIsZero != null && instance instanceof ExprNaturalIsZero)
				return naturalIsZero.apply((ExprNaturalIsZero<S,A>) instance);
			else if (naturalEven != null && instance instanceof ExprNaturalEven)
				return naturalEven.apply((ExprNaturalEven<S,A>) instance);
			else if (naturalOdd != null && instance instanceof ExprNaturalOdd)
				return naturalOdd.apply((ExprNaturalOdd<S,A>) instance);
			else if (naturalToInteger != null && instance instanceof ExprNaturalToInteger)
				return naturalToInteger.apply((ExprNaturalToInteger<S,A>) instance);
			else if (naturalShow != null && instance instanceof ExprNaturalShow)
				return naturalShow.apply((ExprNaturalShow<S,A>) instance);
			else if (naturalPlus != null && instance instanceof ExprNaturalPlus)
				return naturalPlus.apply((ExprNaturalPlus<S,A>) instance);
			else if (naturalTimes != null && instance instanceof ExprNaturalTimes)
				return naturalTimes.apply((ExprNaturalTimes<S,A>) instance);
			else if (integer != null && instance instanceof ExprInteger)
				return integer.apply((ExprInteger<S,A>) instance);
			else if (integerLit != null && instance instanceof ExprIntegerLit)
				return integerLit.apply((ExprIntegerLit<S,A>) instance);
			else if (integerShow != null && instance instanceof ExprIntegerShow)
				return integerShow.apply((ExprIntegerShow<S,A>) instance);
			else if (integerToDouble != null && instance instanceof ExprIntegerToDouble)
				return integerToDouble.apply((ExprIntegerToDouble<S,A>) instance);
			else if (doubleTy != null && instance instanceof ExprDouble)
				return doubleTy.apply((ExprDouble<S,A>) instance);
			else if (doubleLit != null && instance instanceof ExprDoubleLit)
				return doubleLit.apply((ExprDoubleLit<S,A>) instance);
			else if (doubleShow != null && instance instanceof ExprDoubleShow)
				return doubleShow.apply((ExprDoubleShow<S,A>) instance);
			else if (text != null && instance instanceof ExprText)
				return text.apply((ExprText<S,A>) instance);
			else if (textLit != null && instance instanceof ExprTextLit)
				return textLit.apply((ExprTextLit<S,A>) instance);
			else if (textAppend != null && instance instanceof ExprTextAppend)
				return textAppend.apply((ExprTextAppend<S,A>) instance);
			else if (list != null && instance instanceof ExprList)
				return list.apply((ExprList<S,A>) instance);
			else if (listLit != null && instance instanceof ExprListLit)
				return listLit.apply((ExprListLit<S,A>) instance);
			else if (listAppend != null && instance instanceof ExprListAppend)
				return listAppend.apply((ExprListAppend<S,A>) instance);
			else if (listBuild != null && instance instanceof ExprListBuild)
				return listBuild.apply((ExprListBuild<S,A>) instance);
			else if (listFold != null && instance instanceof ExprListFold)
				return listFold.apply((ExprListFold<S,A>) instance);
			else if (listLength != null && instance instanceof ExprListLength)
				return listLength.apply((ExprListLength<S,A>) instance);
			else if (listHead != null && instance instanceof ExprListHead)
				return listHead.apply((ExprListHead<S,A>) instance);
			else if (listLast != null && instance instanceof ExprListLast)
				return listLast.apply((ExprListLast<S,A>) instance);
			else if (listIndexed != null && instance instanceof ExprListIndexed)
				return listIndexed.apply((ExprListIndexed<S,A>) instance);
			else if (listReverse != null && instance instanceof ExprListReverse)
				return listReverse.apply((ExprListReverse<S,A>) instance);
			else if (optional != null && instance instanceof ExprOptional)
				return optional.apply((ExprOptional<S,A>) instance);
			else if (optionalLit != null && instance instanceof ExprOptionalLit)
				return optionalLit.apply((ExprOptionalLit<S,A>) instance);
			else if (optionalFold != null && instance instanceof ExprOptionalFold)
				return optionalFold.apply((ExprOptionalFold<S,A>) instance);
			else if (optionalBuild != null && instance instanceof ExprOptionalBuild)
				return optionalBuild.apply((ExprOptionalBuild<S,A>) instance);
			else if (record != null && instance instanceof ExprRecord)
				return record.apply((ExprRecord<S,A>) instance);
			else if (recordLit != null && instance instanceof ExprRecordLit)
				return recordLit.apply((ExprRecordLit<S,A>) instance);
			else if (union != null && instance instanceof ExprUnion)
				return union.apply((ExprUnion<S,A>) instance);
			else if (unionLit != null && instance instanceof ExprUnionLit)
				return unionLit.apply((ExprUnionLit<S,A>) instance);
			else if (combine != null && instance instanceof ExprCombine)
				return combine.apply((ExprCombine<S,A>) instance);
			else if (combineTypes != null && instance instanceof ExprCombineTypes)
				return combineTypes.apply((ExprCombineTypes<S,A>) instance);
			else if (prefer != null && instance instanceof ExprPrefer)
				return prefer.apply((ExprPrefer<S,A>) instance);
			else if (merge != null && instance instanceof ExprMerge)
				return merge.apply((ExprMerge<S,A>) instance);
			else if (constructors != null && instance instanceof ExprConstructors)
				return constructors.apply((ExprConstructors<S,A>) instance);
			else if (field != null && instance instanceof ExprField)
				return field.apply((ExprField<S,A>) instance);
			else if (project != null && instance instanceof ExprProject)
				return project.apply((ExprProject<S,A>) instance);
			else if (note != null && instance instanceof ExprNote)
				return note.apply((ExprNote<S,A>) instance);
			else if (importAlt != null && instance instanceof ExprImportAlt)
				return importAlt.apply((ExprImportAlt<S,A>) instance);
			else if (embed != null && instance instanceof ExprEmbed)
				return embed.apply((ExprEmbed<S,A>) instance);
			else
				return any.apply(instance);
		}

		@Override
		public T apply(Expr<S, A> t) {
			return match(t);
		}
	}
	
}
