<head>
  <meta charset="UTF-8">
</head> 

<p align="middle">
<img height="250" width="250" src="https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/img/dhall-icon.png" />
<img height="250" width="230" src="https://raw.githubusercontent.com/typelead/eta/master/eta_logo.png" />
</p>

[![CircleCI](https://circleci.com/gh/eta-lang/dhall-eta.svg?style=svg)](https://circleci.com/gh/eta-lang/dhall-eta)

`dhall-eta` is a [eta](https://eta-lang.org/) library that wraps the
[haskell implementation](https://github.com/dhall-lang/dhall-haskell/tree/master/dhall)
of [dhall configuration language](https://dhall-lang.org/) with a friendly java api.

Its main goal is to create a [language binding](https://github.com/dhall-lang/dhall-lang#language-bindings) to make possible configure `java` libraries and applications with `dhall` files.

We already can use `eta` to compile the `dhall haskell` implementation
and [etlas](https://eta-lang.org/docs/user-guides/etlas-user-guide) (a
specific build tool for `eta`) to install `dhall` and its tools ([`dhall-to-json`](https://github.com/dhall-lang/dhall-haskell/tree/master/dhall-json)
or
[`dhall-text`](https://github.com/dhall-lang/dhall-haskell/tree/master/dhall-text))
as java cli applications. However, the classes generated by eta can't
be used directly in java code in a easy way. Hence the need for this library.

## Table of contents

* [Api Overview](#api-overview)
  * [High level api](#high-level-api)
  * [Low level api](#low-level-api)
* [Building](#building)
  * [`etlas`](#etlas)
  * [**`gradle`**](#gradle)

## API Overview

* The quick way to explore the java api is use [`jshell`](https://docs.oracle.com/javase/9/tools/jshell.htm) the java repl tool present since java 9.
  * To start a session with the library in the classpath you can do:
```console
> jshell --class-path /path/to/dhall-eta-${version}-all.jar
```
NOTE: The version of java used is [openjdk-9.0.4](https://jdk.java.net/archive/). 

* To use the api in your project simply download from last release the `dhall-eta-all-${version}.jar` and put it in the project classpath.
* The project has an [executable class](./examples/src/main/java/org/dhall/eta/example/Client.java) with some more examples.

### High level api

The main class to compile dhall expressions and get java objects is
the class `org.dhall.eta.Input`.

* The class has methods to compile simple types and convert them to java:

```java
jshell> import org.dhall.eta.Input

jshell> Input.bool("True")
$2 ==> true

jshell> Input.str("\"This is a dhall Text\"")
$3 ==> "This is a dhall Text"

jshell> Input.bigInt("-1234567890")
$4 ==> -1234567890

jshell> Input.natural("123456789 * 123456789")
$5 ==> Natural [15241578750190521]

```

* Those are shorthands for the main method `Input.type`. To use it we
  need to build a `org.dhall.eta.Type<R>` used by the api to extract a
  java value (of type `R`) and typecheck the dhall expression. Methods for basic types are already defined in `org.dhall.eta.Types`: 

```java
jshell> import org.dhall.eta.Type

jshell> import org.dhall.eta.Types

jshell> Type<Boolean> boolTy = Types.bool()
boolTy ==> eta.org.dhall.eta.Type$Eta@67bf0d91

jshell> Input.type(boolTy, "True && False")
$6 ==> false
```
* The class `org.dhall.eta.Types` has methods to build standard "container" types as `List`, `Optional` or `Map`.
  * For extract a java `Map` from a dhall [record](https://github.com/dhall-lang/dhall-lang/wiki/Built-in-types%2C-functions%2C-and-operators#records), we need to build a `Map` with the keys and types of the fields expected and then use `Types.objMap` to get a `Type<Map<String, Object>>` suitable to use with `Input.type`:

```java
jshell> Map<String,Type<? extends Object>> fieldTypes=new HashMap<>();
fieldTypes ==> {}

jshell> fieldTypes.put("name",Types.str());
$9 ==> null

jshell> fieldTypes.put("nats",Types.list(Types.natural()))
$10 ==> null

jshell> Type<Map<String,Object>> mapTy=Types.objMap(FieldTypes.upcast(fieldTypes))
mapTy ==> eta.org.dhall.eta.Type$Eta@35884aff

jshell> Input.type(mapTy, "{ name = \"name\", nats=[1, 2, 3] }")
$11 ==> {name=name, nats=[Natural[1], Natural[2], Natural[3]]}
```

* Of course you can use the api to create an arbitrary java object from dhall code. We should use the generic `Input.type` method and create the appropiate type:
  * From a dhall record, using the method [`Types.record`](./examples/src/main/java/org/dhall/eta/example/Client.java#L87-L90) and implementing [`org.dhall.eta.RecordType`](./src/main/java/org/dhall/eta/RecordType.java)
    * See an example implementation [here](./examples/src/main/java/org/dhall/eta/example/Client.java#L210-L226).
  * From a dhall union, using the method [`Types.union`](./examples/src/main/java/org/dhall/eta/example/Client.java#L93-L95) and implementing [`org.dhall.eta.UnionType`](./src/main/java/org/dhall/eta/UnionType.java)
    * See an example implementation [here](./examples/src/main/java/org/dhall/eta/example/Client.java#L254-L273).
  * From arbitrary dhall code implementing the low-level [`org.dhall.eta.Type`](./src/main/java/org/dhall/eta/Type.java) interface and using directly `Input.type`.
  
### Low level api

* A more detailed overview of the low level api is coming soon.
* For now we can list the classes that can be used to handle each dhall compiler phase and a link to some example case uses:
  * Parsing: [`org.dhall.eta.Parser`](./examples/src/main/java/org/dhall/eta/example/Client.java#L101-L109)
  * Resolving imports: [`org.dhall.eta.Import`](./examples/src/main/java/org/dhall/eta/example/Client.java#L143)
  * Normalizing and pretty printing: [`org.dhall.eta.Core`](./examples/src/main/java/org/dhall/eta/example/Client.java#L150-L155)
  * Typechecking: [`org.dhall.eta.TypeCheck`](./examples/src/main/java/org/dhall/eta/example/Client.java#L146)
  * Binary encoding/decoding: [`org.dhall.eta.Binary`](./examples/src/main/java/org/dhall/eta/example/Client.java#L158-L168)


## Building

### Etlas

* ~~Download etlas from https://eta-lang.org/docs/user-guides/eta-user-guide/installation/etlas~~
  * **WARNING**: Actually `dhall-eta` can only be built using `eta` and `etlas` built themselves from master. We are in the cutting edge! 
* Run **`etlas build --enable-uberjar-mode`** to compile and generate an uberjar with all dependecies included in `./dist/build/eta-${version}/dhall-eta-${version}/x/dhall-eta-all/build/dhall-eta-all/dhall-eta-all.jar`
  * The output jar is not minimized and include an example executable with `etlas run dhall-eta-all`

* Run **`etlas test`** to execute the test suite against `dhall-lang` [acceptance tests](https://github.com/dhall-lang/dhall-lang/tree/master/tests)
  * As a previous step you'll need to checkout `dhall-lang` project in the same parent directory as `dhall-eta` 
  * Current dhall standard version supported is `4.0.0` and we have to use a fixed version of tests for `4.0.0` in https://github.com/jneira/dhall-lang
  * So the command to get them could be (being in the `dhall-eta` project dir):
```console
> git clone --branch v4.0.0 --depth=1 https://github.com/jneira/dhall-lang.git ../dhall-lang
```
  * The test suite does *not* check the validity of `dhall-haskell` (and it should not do it!), only that the `dhall-haskell` implementation compiled by `eta` and `dhall-eta` itself are consistent.

### Gradle

* **WARNING**: For now, the `gradle` build is configured to use the system wide versions of `eta` and `etlas` so you will must have them built from source and available in `PATH`. We are in the cutting edge here too!
* This is the preferred method to build the project and the used one to generate the artifacts needed for a release of the library.
* It uses the [`gradle` plugin for eta](https://eta-lang.org/docs/user-guides/eta-user-guide/installation/gradle) and the proguard and shadow ones to generate a minimized jar with all the dependencies included.
* It is recommended to use the `gradle` wrapper cause the eta `gradle` plugin will likely work with it.
* So **`./gradlew build`** will generate the following artifacts:
  * A minimized uberjar ready to be used standalone in `./lib/dhall-eta-${version}-all.jar`
  * An artefact with the objects representing the core business model of dhall. In this one will not be any code for actually parse, normalize o compile dhall code. 
    * The package root name has not reference to `eta`: it is simply `org.dhall`.
    * The code is in a `gradle` subproject named `dhall-model` in the folder `./java`
    * The artifact generated is `./java/build/libs/dhall-model-${version}.jar`
  * An artefact with the java executable example [`org.dhall.eta.examples.Client`](./examples/src/main/java/org/dhall/eta/example/Client.java).
    * The code is in a gradle subproject named `dhall-eta-examples` in the folder `examples` 
    * You can run it with **`./gradlew run`**. 
