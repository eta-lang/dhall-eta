package org.dhall.eta.example;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

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

import org.haskell.types.*;

public class Client {

    public static void main (String[] args) {
        System.out.println("Testing dhall input");
        System.out.println(Input.bool("True"));
        System.out.println(Input.bool("True && False"));
        try {
            System.out.println(Input.bool("1"));
        } catch (Exception e) {
            System.out.println(e.getLocalizedMessage());
        }
        System.out.println(Input.str("let str=\"dhall\" in \"Hello ${str}\""));
        System.out.println(Input.bigInt("+1234567"));
        System.out.println(Input.natural("2 * 3 + 4"));
        System.out.println("-----------------------------");

        Either<ParseError,Expr<Src,Import>> parsed = Parser.exprFromText("example","1");
        System.out.println("Parsing \"1\":");
        System.out.println(parsed);
        System.out.println("-----------------------------");

        Either<ParseError,Expr<Src,Import>> eParsedFun = 
        		Parser.exprFromText("example","\\ (t : Text) -> 1");
        System.out.println("Parsing \"λ (t : Text) -> 1\":");
        System.out.println(eParsedFun);
        System.out.println("-----------------------------");
        
        Either.Matcher<ParseError, Expr<Src,Import>, Expr<Src,Import>> matcher = 
        		new Either.Matcher<>(any -> null);
        matcher.Right(r -> r.getValue());
        Expr<Src,Import> parsedFun =  matcher.match(eParsedFun);
        System.out.println("Pretty printing parsed fun");
        System.out.println(Core.pretty(parsedFun));
        System.out.println("-----------------------------");
        
        Expr<Void,Import> norm = Core.normalize(parsedFun);
        System.out.println("Normalize fun");
        System.out.println(norm);
        System.out.println("-----------------------------");
        
        Expr<Void,Import> alphaNorm = Core.alphaNormalize(norm);
        System.out.println("Alpha normalize fun");
        System.out.println(alphaNorm);
        System.out.println("-----------------------------");
        
        Expr<Src,Void> importedExpr = new ExprIntegerShow<>();
        Either<TypeError<Src, Void>,Expr<Src,Void>> checked = 
            TypeCheck.typeOf(importedExpr);
        System.out.println("Type of ExprIntegerShow");
        System.out.println(checked);
        System.out.println("-----------------------------");
        
        System.out.println("Testing pets");
        String petDhall = "let Pet = constructors < Cat : Text | Dog : Text > in Pet.Cat \"Tom\"";
        Either<ParseError,Expr<Src,Import>> eParsedPet = 
        		Parser.exprFromText("example",petDhall);	
        Either.Matcher<ParseError, Expr<Src,Import>, Expr<Src,Import>> matcherPet = 
            new Either.Matcher<>(any -> null);
        matcherPet.Right(r -> r.getValue());
        Expr<Src,Import> parsedPet =  matcherPet.match(eParsedPet);
        System.out.println("Parsing Pet");
        System.out.println(parsedPet);
        Expr<Void,Import> denotedPet=Core.denote(parsedPet);
        System.out.println("Denoted Pet");
        System.out.println(denotedPet);
        Expr<Src,Void> importedPet = org.dhall.eta.Import.load(parsedPet);
        System.out.println("Imported Pet");
        System.out.println(importedPet);
        Either<TypeError<Src, Void>,Expr<Src,Void>> checkedPet = 
            TypeCheck.typeOf(importedPet);
        System.out.println("Checked Pet");
        System.out.println(checkedPet);
        Expr<Void, Void> normPet = Core.normalizeResolved(importedPet);
        System.out.println("Beta Normalized Pet");
        System.out.println(normPet);
        Expr<Void,Void> alphaNormPet = Core.alphaNormalizeResolved(normPet);
        System.out.println("Alpha Normalized Pet");
        System.out.println(alphaNormPet);
        
        System.out.println("-----------------------------");

        System.out.println("Testing dhall main module");
        Type<Boolean> tyBool = Types.bool();
        Boolean bool = Input.type(tyBool, "True");
        System.out.println(bool);
        Type<Optional<String>> optStrTy=Types.optional(Types.str());
        Optional<String> optStrIn = Input.type(optStrTy, "Some \"hello\"");
        System.out.println(optStrIn);
        
        Type<List<Natural>> nsTy = Types.list(Types.natural());
        List<Natural> ns = Input.type(nsTy, "[1, 2, 3]");
        System.out.println(ns);
        
        Map<String,Type<Object>> mapFieldsTy= new HashMap<>();
        mapFieldsTy.put("key1", asObjTy(optStrTy));
        mapFieldsTy.put("key2", asObjTy(nsTy));
        mapFieldsTy.put("key3", asObjTy(Types.str()));
        Type<Map<String,Object>> mapTy = Types.mapObj(mapFieldsTy);
        Map<String,Object> m = Input.type(mapTy, 
        		"{ key1 = Some \"val1\", key2 = [1, 2, 3], key3 = \"val2\" }");
        System.out.println(m);
        Expr<Unit, Import> expr = new ExprIntegerLit<Unit,Import>(new BigInteger("1"));
        List<Byte> b = Binary.encodeWithVersion(
        		StandardVersion.defaultVersion(), expr);
        System.out.println("Bytes: "+b);
        Either<DecodingFailure,Expr<Unit,Import>> decoded = Binary.decodeWithVersion(b);
        System.out.println("Expr decoded: "+decoded);
        Project p = Input.type(Types.record(new ProjectType()),
        		"{ name = \"dhall\", description = \"desc\", stars = 123 }");
        System.out.println(p);
        Pet pet = Input.type(Types.union(new PetType()),petDhall);
        System.out.println(pet);
        System.out.println("The end");
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
