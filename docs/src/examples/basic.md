# Basic Example

Say we have a package that provides an interface for other packages to hook into:

```julia
module A

export MyInterface, myfunc

"""
    MyInterface

I'm an abstract type, which means I have some required dispatches
that subtypes need to implement.

The interface to implement is:

 * myfunc(::MyInterface)
"""
abstract type MyInterface end

"""
    myfunc(::MyInterface)

I'm the sole interface function for `MyInterface`. Types that subtype
`MyInterface` must implement me on that subtype to conform to `MyInterface`.
"""
function myfunc end
end
```

A priori, a potential implementor only has a docstring to go on, to be able to decide whether they've
correctly implemented everything an interface requires to be implemented. Additionally, they can't easily
write regression tests to ensure that they don't accidentally remove a required definition, or keep up with
new additions to an interface in a breaking release of the interface they're depending on. This means
keeping up with interface definitions is a quite tedious chore, even though all information ought to be available
to (at least partially) automate this. What's worse, with just the bare `function myfunc end` definition above,
it's impossible to tell "this is unsupported" apart from "this is intended to be implemented".

This is where RequiredInterfaces.jl comes in. If the above module makes use of [`@required`](@ref) to mark
its interface requirements in terms of required dispatches, downstream packages that want to implement the interface
can preemptively check if they've even defined the correct methods (and not have their implementation work by
accident in an unsupported manner, e.g. by committing type piracy or implementing an entirely different set of methods
that just happen to work).

The change `A` has to make is simple:

```@example basic
module A

using RequiredInterfaces

export MyInterface, myfunc

"""
...omitted for brevity
"""
abstract type MyInterface end

"""
...omitted for brevity
"""
function myfunc end

@required MyInterface myfunc(::MyInterface)

end
```

That is, one additional `using`, as well as an invocation of `@required` with the abstract interface type
as well as the function and its signature that's part of the interface `MyInterface`.

With this small change, all issues mentioned above are solvable. First, the ambiguity between "should be implemented"
and "not supported" is solved:

```@repl basic
using .A
struct Foo <: A.MyInterface end
A.myfunc(Foo()) 
```

This is because [`@required`](@ref) defines a fallback method that dispatches to [`NotImplementedError`](@ref),
allowing users that encounter this error to notify the package maintainer that they have missed to implement a required
method. Further, because this is an actually thrown error, it's also discoverable through JET.jl, even without
explicit interface testing by implementors.

In the next section, we'll take a look at how package authors that would like to hook into an interface
can test that they have successfully done so, at least in terms of defining the correct methods.
