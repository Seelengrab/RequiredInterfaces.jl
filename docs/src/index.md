# RequiredInterfaces.jl Documentation

This is the documentation for RequiredInterfaces.jl, a small package intended to mark the parts of an interface
that implementors of that interface MUST implement in order to conform to it.

Check out the examples to get an introduction to interface specifications! If you're interested in the larger
philosophy behind this package, check out the section [About Interfaces](@ref).

```@contents
Pages = ["index.md", "examples/basic.md", "examples/testing.md", "interfaces.md", "api.md"]
Depth = 3
```

## Goals

 * Giving interface-writers the ability to declare "these are the required functions developers
   who would like to hook into this interface need to implement".
 * Treat abstract types like implicit interfaces.
 * Be precompilation friendly & zero-overhead.
 * Allow built-in testing for whether at least the required methods to conform to an interface are defined.
