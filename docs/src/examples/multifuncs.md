# Interfaces with multiple functions

Most interfaces are larger than just a single function, requiring multiple methods
to be implemented in order to conform to the interface. One example for this is
`AbstractArray`, which at minimum (for linear indexing) requires these methods to be implemented:

 * `size(::AbstractArray)`
 * `getindex(::AbstractArray, i::Int)`

This interface can be described like so:

```@example lineararray
using RequiredInterfaces
const RI = RequiredInterfaces

abstract type LinearArray{T,N} <: AbstractArray{T,N} end
@required LinearArray begin
    Base.size(::LinearArray)
    Base.getindex(::LinearArray, ::Int)
end
```

Importantly, we _don't need to subtype `LinearArray`_ in order to check whether a type *would* implement
the interface:

```@repl lineararray
using Test
@test RI.check_interface_implemented(LinearArray, Vector)
```

At the moment, this doesn't handle fallback definitions of abstract types well - for example, if we do
the same check with `Array`, which has a fallback `size` defined:

```@repl lineararray
using Test
@test RI.check_interface_implemented(LinearArray, Array)
```

While unfortunate, this is only a limitation of the current implementation, and should be remedied in the
future.

Another limitation is that we can't use `LinearArray` to dispatch `Array` objects, due to Julia not recognizing
that the concrete instantiations of `Array` (`Vector` etc.) do implement the interface correctly - this is
something that could be remedied with the solution presented in [About Interfaces](@ref), though there
are multiple other venues as well.

For now, checks like these can serve as information on whether a type does conform to the interface correctly,
even if it doesn't formally subtype the abstract type behind this interface.