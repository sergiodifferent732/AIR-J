# AIR-J Language Reference

This is the implementation-synchronized reference manual for AIR-J.

Use this document when you need one place that describes:
- what AIR-J is for
- what source forms exist
- what types and effects exist
- what the standard modules mean
- how AIR-J programs are built and run today

Related documents:
- [`notes.md`](/Users/unclebob/projects/AIR-J/notes.md): mission and design intent
- [`formal-v0-spec.md`](/Users/unclebob/projects/AIR-J/formal-v0-spec.md): formal contract for persisted AIR-J
- [`README.md`](/Users/unclebob/projects/AIR-J/README.md): project overview and quick start
- [`AGENTS.md`](/Users/unclebob/projects/AIR-J/AGENTS.md): repository workflow and verification rules

## Mission

AIR-J is an AI-first, JVM-targeting language whose primary artifact is a canonical, typed, effect-tracked intermediate representation.

Its purpose is to:
- reduce ambiguity
- reduce representation variance
- reduce re-analysis cost
- make transformation legality mechanically checkable
- preserve direct access to the JVM ecosystem

Principle: `one meaning, one representation`.

AIR-J is not meant to be a pleasant human language. It is meant to be a reliable language and runtime substrate for software agents.

## Core Model

AIR-J programs are persisted as canonical s-expressions.

A source file contains exactly one module:

```lisp
(module example/hello
  (imports)
  (export main)
  ...)
```

AIR-J modules are the namespace unit of the language. JVM package/class names are derived from module names:

- AIR-J module: `example/hello`
- JVM class: `example.hello`

## Canonicality

Persisted AIR-J is canonical:
- each construct has one persisted shape
- imports and exports are explicit
- local references use `(local name)`
- types and effects are explicit
- control flow is explicit
- mutation is explicit
- Java interop is explicit

AIR-J does not rely on multiple equivalent source spellings.

## Module Structure

Canonical module shape:

```lisp
(module module/name
  (host java.super.Class)?      ; optional
  (imports ...)
  (export ...)
  decl*)
```

Supported top-level declarations:
- `data`
- `enum`
- `union`
- `fn`

Example:

```lisp
(module alpha/math
  (imports)
  (export Counter next)
  (data Counter
    (field value Int))
  (fn next
    (params (counter Counter))
    (returns Counter)
    (effects ())
    (requires true)
    (ensures true)
    (construct Counter
      (int-add (record-get (local counter) value) 1))))
```

## Host-Backed Modules

Modules may optionally extend one JVM host superclass:

```lisp
(module app/sketch
  (host processing.core.PApplet)
  ...)
```

Meaning:
- the emitted class extends that Java superclass
- exported AIR-J functions whose first parameter accepts the host instance become public instance bridge methods
- the ordinary AIR-J implementation remains static and canonical internally

This is the mechanism used for callback-oriented frameworks such as Processing.

## Types

Implemented primitive types:
- `Bool`
- `Int`
- `Float`
- `Double`
- `String`
- `Unit`
- `Bytes`

Implemented canonical constructed types:
- `StringSeq`
- `(Seq T)`
- `(Map String T)`
- `(Option T)`
- `(Result Ok Err)`
- `data` declarations
- `enum` declarations
- `union` declarations
- `(Java fully.qualified.ClassName)`

Examples:

```lisp
Int
String
(Seq String)
(Map String Int)
(Option String)
(Result Int Diagnostic)
(Java java.util.Random)
```

Notes:
- `Bytes` is the canonical raw byte type
- AIR-J maps are canonical string-keyed maps
- `Option` and `Result` are canonical absence/failure carriers
- nominal types may be parameterized and are propagated through type checking and lowering

## Effects

Function effects are explicit in every function signature:

```lisp
(effects (Stdout.Write File.Read))
```

Important effect names currently used by the implementation:
- `Stdout.Write`
- `Stdin.Read`
- `File.Read`
- `File.Write`
- `Env.Read`
- `Process.Run`
- `Foreign.Throw`

Rules:
- pure functions use `(effects ())`
- boundary functions declare the effects they perform
- recoverable standard-module failures should prefer `Result` + `Diagnostic`
- raw boundary operations may still declare `Foreign.Throw`

## Contracts And Invariants

Functions support:
- `requires`
- `ensures`

Nominal types support:
- `invariants`

Example:

```lisp
(fn positive-inc
  (params (n Int))
  (returns Int)
  (effects ())
  (requires (int-gt (local n) 0))
  (ensures true)
  (int-add (local n) 1))
```

These are checked statically for type/effect discipline and enforced at runtime where the implementation supports them.

## Expressions

Important canonical expression forms:
- literals: `true`, `false`, `1`, `1.5`, `"text"`
- `(local name)`
- `(let ((name expr) ...) body)`
- `(if test then else)`
- `(match target (case pattern expr) ...)`
- `(call callee arg ...)`
- `(lambda ... )`
- `(construct Type arg ...)`
- `(variant Type Variant arg ...)`
- `(record-get expr field)`
- `(seq expr expr ...)`
- `(loop ((name expr) ...) body)`
- `(recur expr ...)`
- `(try expr (catch Type name expr) ... (finally expr)?)`
- `(raise expr)`
- `(var name Type expr)`
- `(set name expr)`

### Patterns

Supported match patterns include:
- wildcard: `_`
- binder: `value`
- literal patterns
- enum/variant patterns
- union payload patterns
- record patterns

Examples:

```lisp
(case None 0)
(case (Some value) (local value))
(case (Ok payload) 1)
(case (Err error) 0)
```

## Primitive Operators

AIR-J uses canonical built-ins instead of symbolic syntax.

Implemented integer operators:
- `int-add`
- `int-sub`
- `int-mul`
- `int-div`
- `int-mod`
- `int-eq`
- `int-ne`
- `int-lt`
- `int-le`
- `int-gt`
- `int-ge`

Implemented boolean operators:
- `bool-not`
- `bool-and`
- `bool-or`
- `bool-eq`

Implemented string/text operators:
- `string-eq`
- `string-concat`
- `string-length`
- `string-empty?`
- `string-trim`
- `string-split-on`
- `string-substring`
- `string->int`
- `int->string`

Implemented sequence operators:
- `seq-length`
- `seq-empty?`
- `seq-first`
- `seq-rest`
- `seq-get`
- `seq-concat`

Implemented map operators:
- `map-empty`
- `map-set`
- `map-get`

Implemented JSON/host boundary primitives:
- `json-parse`
- `json-write`
- `env-get`
- `process-run`

Implemented console I/O primitives:
- `io/read-line`
- `io/print`
- `io/println`

## Java Interop

AIR-J supports explicit interop forms:
- `java/new`
- `java/call`
- `java/static-call`
- `java/get-field`
- `java/set-field`
- `java/static-get-field`
- `java/static-set-field`

Example:

```lisp
(java/static-call
  java.lang.Integer
  parseInt
  (signature (String) Int)
  "42")
```

Interop is explicit and typed. Use it when you intentionally need host functionality outside the canonical AIR-J surface.

## Standard Modules

The standard modules define AIR-J's canonical boundary to the host environment.

### `airj/core`

Canonical carriers:
- `Diagnostic`
- `Interchange`
- `Option`
- `Result`
- `parse-int`

`Diagnostic` fields:
- `phase : String`
- `message : String`
- `detail : String`

`Interchange` variants:
- `Null`
- `BoolValue`
- `IntValue`
- `DoubleValue`
- `StringValue`
- `SeqValue`
- `MapValue`

### `airj/bytes`

Canonical byte operations:
- `utf8-encode`
- `utf8-decode`
- `length`

### `airj/env`

Canonical environment operations:
- `get : String -> (Option String)`
- `cwd : () -> String`

### `airj/file`

Canonical filesystem boundary:
- `exists?`
- `read-string`
- `read-string-result`
- `read-bytes`
- `read-bytes-result`
- `read-lines`
- `read-lines-result`
- `write-string`
- `write-string-result`
- `write-bytes`
- `write-bytes-result`
- `write-lines`
- `write-lines-result`

### `airj/json`

Canonical JSON boundary:
- `parse`
- `parse-result`
- `write`
- `write-result`

### `airj/process`

Canonical process boundary:
- `ProcessResult`
- `run`
- `run-result`

`ProcessResult` fields:
- `exit-code : Int`
- `stdout : Bytes`
- `stderr : Bytes`

## Failure Model

AIR-J uses two distinct boundary styles:

Raw boundary operations:
- may carry host effects
- may carry `Foreign.Throw`
- example: `airj/file.read-string`, `airj/process.run`, `airj/json.parse`

Wrapped standard-module operations:
- convert recoverable failures into `(Result _ Diagnostic)`
- keep failure transport machine-readable
- example: `read-string-result`, `write-result`, `run-result`, `parse-int`

Current policy:
- prefer `Result` + `Diagnostic` at canonical module boundaries
- reserve raw throws for lower-level operations and intentional foreign integration

`Diagnostic.message` should stay stable and machine-oriented.
`Diagnostic.detail` should identify the relevant path, input text, or boundary context.

## Build And Run

### Single-Source Commands

```bash
clj -M -m airj.cli parse path/to/program.airj
clj -M -m airj.cli normalize path/to/program.airj
clj -M -m airj.cli check path/to/program.airj
clj -M -m airj.cli lower path/to/program.airj
clj -M -m airj.cli build path/to/program.airj target/classes
clj -M -m airj.cli run path/to/program.airj arg1 arg2
```

### Runnable Jar For A Single Source File

```bash
clj -M -m airj.cli build --jar /tmp/program.jar path/to/program.airj
java -jar /tmp/program.jar
```

### Project Builds

AIR-J supports multi-module project builds from:
- `--project-sources-edn`
- `--project-dir`

Project class-directory build:

```bash
clj -M -m airj.cli build --project-dir path/to/project root/module target/classes
```

Project runnable jar build:

```bash
clj -M -m airj.cli build --project-dir path/to/project --jar /tmp/project.jar root/module
java -jar /tmp/project.jar
```

The jar path includes reachable AIR-J output plus required runtime/classpath support.

### Runtime Entry Policy

Generated JVM `main` behaves like this:
- if AIR-J `main` returns `Int`, that becomes the JVM process exit code
- AIR-J `main` is responsible for its own output
- returned non-`Int` values are not automatically printed by the generated wrapper

## Examples

In-repo examples:
- [`examples/HTW/README.md`](/Users/unclebob/projects/AIR-J/examples/HTW/README.md)
- [`examples/ToolWorkflow/README.md`](/Users/unclebob/projects/AIR-J/examples/ToolWorkflow/README.md)

## What This Reference Does Not Replace

This document is the best implementation-synced guide for using AIR-J today.

It does not replace:
- [`notes.md`](/Users/unclebob/projects/AIR-J/notes.md) for design rationale
- [`formal-v0-spec.md`](/Users/unclebob/projects/AIR-J/formal-v0-spec.md) for the formal persisted-language contract
- source/specs when you need exact implementation details
