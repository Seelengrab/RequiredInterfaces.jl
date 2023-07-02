# Testing Required Interfaces

For example, consider a package `B` that would like to
implement the interface from the previous section on its type `MyImplementor`:

```@repl testing
module A # hide
using RequiredInterfaces # hide
export MyInterface, myfunc # hide
abstract type MyInterface end # hide
function myfunc end # hide
@required MyInterface myfunc(::MyInterface) # hide
end # hide
using .A # hide
struct MyImplementor <: MyInterface end

A.myfunc(::MyImplementor) = "Implemented!"
```

The authors of package B would (rightly so) like to test that they have conformed to the interface,
at least insofar that they are dispatching correctly. With RequiredInterfaces.jl, this too can be easily
done (think of this next block as placed in `runtests.jl`):

```@example testing
using Test, RequiredInterfaces
const RI = RequiredInterfaces

@test RI.check_interface_implemented(MyInterface, MyImplementor)
```

Compare this to what happens when a type doesn't implement the interface correctly:

```@repl testing
struct NonImplementor <: MyInterface end
@test RI.check_interface_implemented(MyInterface, NonImplementor)
```

`check_interface_implemented` not only detects that the interface wasn't fully implemented, it can also
report which signature was missed, and for which function.

If there are a lot of types implementing a specific interface, it's also possible to test all types
who claim to implement the interface, or only a subset of them, instead of doing that on per-type basis:

!!! warning "Julia Bug"
    The first testset below should in reality produce an error, due to not all subtypes of `MyInterface` actually
    implementing the interface. However, due to a bug in Julia ([see this issue](https://github.com/JuliaLang/julia/issues/50354)), `MyInterface` claims
    to not have any subtypes, in spite of the fact that the subtypes have `MyInterface` as their supertype, leading to an empty
    testset. As a workaround, there is a second testset using the explicit collection version to check the subtypes manually, to show the expected failure.
    This bug in Julia should not impact the functionality of this package.

```@repl testing
struct AnotherImplementor <: MyInterface end
A.myfunc(::AnotherImplementor) = "I'm different!"
@testset "Test all subtypes" RI.check_implementations(MyInterface);
@testset "Test all subtypes" RI.check_implementations(MyInterface, [AnotherImplementor, MyImplementor, NonImplementor]);
@testset "Test subset" RI.check_implementations(MyInterface, [AnotherImplementor, MyImplementor]);
```
