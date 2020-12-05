# rjs_engine: ES 2020 complete js engine
## \*\*under construction** - not usable yet
### But do come back later :)
rjs_engine is (going to be) a complete js sandboxed and performance-counted engine using only safe rust, that provides
easy bindings for rust, C and thus other languages.  
Uses swc parser.

##Goals
- Feature complete ES 2020
- complete reliable sandboxing thanks to safe rust
- sources are loaded from a loader, allowing A) packaging and B) again, safety
- performance counting/stepping: js is run for specifiable amount of steps.
  This allow e.g. games to run code across multiple ticks in controllable portions.
- easy to use api for running and binding native code
- *acceptable* performance
- non-safe standard library such as fs is not provided by default, provided as optional module(s)
- js-codegen-macro (let users define inline rust macros)
- keeping a very permissive license
- clean code
- fully tested using js tests that test for correct behaviour

## Todos
- [ ] (Almost there) AST read and translated into code
- [ ] (Getting there) complete engine
- [ ] bash script to run V8 js - tests
- [ ] passes (most) of those tests
- [ ] refactor towards own, safe, more usable GC.
      GC is done but needs performance improvements and needs to be integrated.
- [ ] remove-refactor then obsolete GcDestr
- [ ] separate AST and in-engine representation
- [ ] extract god-files into smaller modules
- [ ] documentation
- [ ] make it known?

##Non-Goals
- V8-like performance. Not *yet* planned.
- Extensibility to js parsing. Unlikely.
- ts typechecking. swc DOES allow ts to be *parsed*.





