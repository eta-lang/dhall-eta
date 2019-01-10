package org.dhall.eta.example;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.dhall.eta.RecordType;
import org.dhall.eta.TagValue;
import org.dhall.eta.Type;
import org.dhall.eta.UnionType;
import org.dhall.StandardVersion;
import org.dhall.binary.DecodingFailure;
import org.dhall.core.*;
import org.dhall.core.expr.ExprIntegerLit;
import org.dhall.core.expr.ExprIntegerShow;
import org.dhall.parser.*;
import org.dhall.parser.error.*;
import org.dhall.typecheck.TypeError;
import org.dhall.eta.Input;
import org.dhall.eta.Parser;
import org.dhall.eta.TypeCheck;
import org.dhall.eta.Types;
import org.dhall.eta.Binary;
import org.dhall.eta.Core;

import org.dhall.common.types.*;

public class Client {

	public static void prn(Object s) {
		System.out.println(s);
	}
	
	public static void prn() {
		System.out.println();
	}
	
    public static void main (String[] args) {
    	System.out.println("Dhall input examples");
        prn("====================");
        prn();
        prn("Simple types shorthands");
        prn("-----------------------");
        prn(Input.bool("True"));
        prn(Input.str("let str=\"dhall\" in \"Hello ${str}\""));
        prn(Input.bigInt("+1234567"));
        prn(Input.natural("2 * 3 + 4"));
        prn(Input.bigDecimal("1e100"));
        prn(Input.bigDecimal("0.0001"));
        prn(Input.doubleDecimal("Infinity"));
        prn(Input.doubleDecimal("0.0002"));
        prn();
        prn("Generic input method");
        prn("--------------------");
        prn(Input.type(Types.bool(), "True && False"));
        prn();
        prn("Standard compound types");
        prn("-----------------------");
        prn(Input.type(Types.list(Types.natural()), "[1, 2, 3, 4]"));
        prn(Input.type(Types.optional(Types.bigDecimal()), "Some 0.1"));
        prn(Input.type(Types.pair(Types.natural(), Types.bool()), 
        		"{ _1 = 42, _2 = False }"));
        prn(Input.type(Types.homMap(Arrays.asList("key","value"), Types.str()),
        		"{ key = \"key\", value = \"value\" }"));
        Map<String,Type<Object>> typeMap=new HashMap<>();
        typeMap.put("name",asObjTy(Types.str()));
        typeMap.put("nats",asObjTy(Types.list(Types.natural())));
        prn(Input.type(Types.objMap(typeMap), 
        		"{ name = \"name\", nats=[1, 2, 3] }"));
        prn();
        prn("Error in input throws an exception");
        try {
            prn(Input.bool("1"));
        } catch (Exception e) {
            prn(e.getLocalizedMessage());
        }
        prn();
        prn("User defined types");
        prn("------------------");
        prn("Dhall record to java class:");
        Project project = Input.type(Types.record(new ProjectType()),
        		"{ name = \"dhall\", description = \"desc\", stars = 123 }");
        prn(project);
        prn();
        prn("Dhall union to java enum:");
        String petDhall = "let Pet = constructors < Cat : Text | Dog : Text > in Pet.Cat \"Tom\"";
        Pet pet = Input.type(Types.union(new PetType()),petDhall);
        prn(pet);
        prn();
        prn("Parser/Core/Import/Typecheck API");
        prn("================================");
        
        Either<ParseError,Expr<Src,Import>> parsed = Parser.exprFromText("example","1");
        prn("Parsing \"1\":");
        prn(parsed);
        prn();
        Either<ParseError,Expr<Src,Import>> eParsedFun = 
        		Parser.exprFromText("example","\\ (t : Text) -> 1");
        prn("Parsing \"λ (t : Text) -> 1\":");
        prn(eParsedFun);
        prn();
        
        Either.Matcher<ParseError, Expr<Src,Import>, Expr<Src,Import>> matcher = 
        		new Either.Matcher<>(any -> null);
        matcher.Right(r -> r.getValue());
        Expr<Src,Import> parsedFun =  matcher.match(eParsedFun);
        prn("Pretty printing parsed fun");
        prn(Core.pretty(parsedFun));
        Expr<Void,Import> norm = Core.normalizeUnresolved(parsedFun);
        prn("Normalize fun");
        prn(norm);
        Expr<Void,Import> alphaNorm = Core.alphaNormalizeUnresolved(norm);
        prn("Alpha normalize fun");
        prn(alphaNorm);
        Expr<Src,Void> importedExpr = new ExprIntegerShow<>();
        Either<TypeError<Src, Void>,Expr<Src,Void>> checked = 
            TypeCheck.typeOfResolved(importedExpr);
        prn("Type of ExprIntegerShow");
        prn(checked);
        prn();
        prn("Compiling pets");
        prn("==============");
        Either<ParseError,Expr<Src,Import>> eParsedPet = 
        		Parser.exprFromText("example", petDhall);	
        Either.Matcher<ParseError, Expr<Src,Import>, Expr<Src,Import>> matcherPet = 
            new Either.Matcher<>(any -> null);
        matcherPet.Right(r -> r.getValue());
        Expr<Src,Import> parsedPet =  matcherPet.match(eParsedPet);
        prn("Parsing:");
        prn(parsedPet);
        Expr<Void,Import> denotedPet=Core.denote(parsedPet);
        prn("Denoting:");
        prn(denotedPet);
        Expr<Src,Void> loadedPet = org.dhall.eta.Import.load(parsedPet);
        prn("Loading:");
        prn(loadedPet);
        Either<TypeError<Src, Void>,Expr<Src,Void>> checkedPet = 
            TypeCheck.typeOfResolved(loadedPet);
        prn("Type checking:");
        prn(checkedPet);
        Expr<Void, Void> normPet = Core.normalizeResolved(loadedPet);
        prn("Beta Normalizing:");
        prn(normPet);
        Expr<Void,Void> alphaNormPet = Core.alphaNormalizeResolved(normPet);
        prn("Alpha Normalizing:");
        prn(alphaNormPet);
        prn();

        prn("Binary API");
        prn("==========");
        Expr<Unit, Import> expr = new ExprIntegerLit<Unit,Import>(new BigInteger("1"));
        List<Byte> b = Binary.encodeWithVersion(StandardVersion.defaultVersion(), expr);
        prn("Expr encoded: "+expr);
        prn("Bytes: "+b);
        Either<DecodingFailure,Expr<Unit,Import>> decoded = Binary.decodeWithVersion(b);
        prn("Expr decoded: "+decoded);
    }
    
    @SuppressWarnings("unchecked")
    public static <A> Type<Object> asObjTy(Type<A> type) {
        return (Type<Object>) type;
    }
    
    public static class Project {
    	private String name;
    	private String description;
        private Natural stars;
        public Project(String name, String description, Natural stars) {
            super();
            this.name = name;
            this.description = description;
            this.stars = stars;
        }
        public String getName() {
            return name;
        }
        public void setName(String name) {
            this.name = name;
        }
        public String getDescription() {
            return description;
        }
        public void setDescription(String description) {
            this.description = description;
        }
        public Natural getStars() {
            return stars;
        }
        public void setStars(Natural stars) {
            this.stars = stars;
        }
        @Override
        public String toString() {
            return "Project [name=" + name + 
            		", description=" + description + 
            		", stars=" + stars + "]";
        }
        
    }
    
    public static class ProjectType implements RecordType<Project> {
		
        public Map<String,Type<? extends Object>> getFieldsTypes() {
            LinkedHashMap<String,Type<? extends Object>> fields=
                new LinkedHashMap<>();
            fields.put("name", Types.str());
            fields.put("description", Types.str());
            fields.put("stars", Types.natural());
            return fields;
        }
	
        public Project fromFieldsValues(Map<String,Object> m) {
            return new Project((String)m.get("name") ,
                               (String)m.get("description"), 
                               (Natural)m.get("stars"));
        }
    }
    
    public static enum Pet {
    	Cat("unknown"), Dog("unknown");
    	
    	private String name;
        
        private Pet(String name) {
            this.name = name;
        }
        
        public String getName() {
            return name;
        }
        
        public void setName(String name) {
            this.name = name;
        }
    	
    	public static Pet byTag(String tag) {
            for (Pet p: values()) {
                if (p.name().equals(tag))
                    return p;
            }
            return null;
    	}
    	
    	
    }
    
    public static class PetType implements UnionType<Pet> {
        
        @Override
        public Map<String, Type<? extends Object>> getTagsTypes() {
            LinkedHashMap<String,Type<? extends Object>> fields= 
            		new LinkedHashMap<>();
            for (Pet p: Pet.values()) {
                fields.put(p.name(), Types.str());
            }
            return fields;
        }
        
        @Override
        public Pet fromTagValue(TagValue<Object> tagVal) {
            Pet p = Pet.byTag(tagVal.getTag());
            if (p!=null)
                p.setName((String) tagVal.getValue());
            return p;
        }
    	
    }
}
