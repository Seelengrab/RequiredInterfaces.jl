# About Interfaces

This package is built around the assumption that an abstract type describes an informal interface.
In this page, we're going to look at why I think this is the expected interpretation for what an
abstract type is, as well as what this means for the (possible) future of interfaces and traits.

## Types in Julia

Before we get into the semantic meaning of abstract types, it's good to establish a baseline about
the kinds of types we have in Julia as part of the user facing API (deliberately _NOT_ what the compiler
internally uses!), how they relate to each other and what ought to hold in these relations.

So, these are the basic kinds of types Julia knows about:

 * `abstract` types
   * e.g. `Integer` or `Real`
   * This also includes the `Any` type
 * `struct` types
   * e.g. `Module` or `Base.TTY` (the type of `stdout`)
 * `primitive` types
   * e.g. `Int64` or `Float64`
 * `Union`s of types
   * e.g. `Union{Real, String}` or `Union{Module, Int64}`
   * This also includes the empty `Union{}`
 * `UnionAll`s over a type
   * e.g. `Array{T,N} where {T,N}` or `Val{N} where N`
   * This family of types more-or-less works like an abstract type for our purposes.

These five objects are our main point of interest. There are also some special types, like `Type` and
`DataType`, but those are (for the most part) internal implementation details for giving some type
to the type objects above, or are dispatch hacks and not relevant for this discussion.

In general, these types are arranged in a [lattice](https://en.wikipedia.org/wiki/Lattice_(order)), formed by ordering according to `<:`, the subtype relation.

This lattice has the following properties. The first five properties follow directly from being a lattice; the sixth is a result of our type system.

 * For all types `T`, `T <: Any` is `true`.
    * More informally, `Any` is the topmost type in the lattice (thus also called Top).
 * For no types `T` other than `T === Any` does `Any <: T` hold.
    * More informally, the only supertype of `Any` is `Any` itself.
 * For all types `T`, `Union{} <: T` is `true`.
    * More informally, `Union{}` is the bottommost type in the lattice (thus also called Bottom).
 * For no types `T` other than `T === Union{}` does `T <: Union{}` hold.
    * More informally, the only subtype of `Union{}` is `Union{}`.
 * For all types `T`, `T <: T` is `true`.
    * All types are their own subtype.
 * For all types `T`, if `isconcretetype(T)` is `true`, then the only types `S` for which `S <: T` is `true` are `S === Union{}` and `S === T`.
    * This is the requirement that concrete types cannot be subtyped; they are (almost) direct supertypes of `Union{}`, though `supertypes(Union{})` can't show us that (the set is not enumerable, because it grows with new struct & type definitions and `UnionAll`s make a mess of things through virtue of being infinitely large).
    * Note that if `!isconcretetype(T)`, this property has no effect - we can add a new abstract type below an existing abstract type, effectively inserting it in-between `Union{} <: T` like `Union{} <: S <: T`.

There are some other properties the type lattice has, but these are the ones relevant for this discussion,
so we'll leave it at that. An informal description of the lattice is "I can do potentially everything with an
`Any`", "I can do nothing at all with a `Union{}`" and "I can do *something* with any type in-between". 

Stated differently, we can expect nothing in particular of an `Any`, we can expect **exactly** that we can't do
anything with a `Union{}` and we can expect *something* of any type in-between.

Stated yet differently, we don't get any guarantees from an object whose type we only know as `Any`, we get
**exactly** the guarantee that we can do nothing from an object whose type we only know as `Union{}`, and we
can expect *some* guarantees from objects whose type lies in between `Any` and `Union{}`.

And stated yet differently, we can say that objects of type `Any` are minimally restrictive (they can do anything), objects of type
`Union{}` are maximally restrictive (they can do nothing), and objects in-between have *some* restriction.

What we're interested in here is that *something*, and how it relates to the notion of an interface. For these purposes,
`UnionAll`s behave exactly like abstract types (though with some additional features), so we're going to treat them as one entity.

## Abstract Types & Behavioral Subtyping

Imagine we didn't have any types in our type lattice. It would look like this:

```julia
Union{} <: Any  
```

and that's it. If we now define a new `abstract type Foo end`, it's implicitly placed beneath `Any`:

```julia
Union{} <: Foo <: Any
```

just as if we had written `abstract type Foo <: Any end` instead.

We can place more types, `abstract type Bar <: Foo end`, this time specifying `Foo` as the direct supertype.
This results in this lattice:

```julia
Union{} <: Bar <: Foo <: Any
```

Note that we still have `Union{} <: Foo`, either directly or by transitivity through `Bar`, due to `Union{} <: Bar` and
`Bar <: Foo` both being true.

Imagine now we have a function:

```julia
function myFunc(x::Any)
    x
end
```

which does nothing more than return its argument. The type signature annotates the argument `x` as `Any`, declaring to any caller
"I don't expect anything in particular from `x`, you can pass in anything". 

Now, our subtyping declarations in our lattice communicate that objects of type `Foo` should dispatch in function calls
just like `Any` does, so calling `myFunc` on an object of type `Foo` ought to work just the same as if the type
were only `Any`. The same is true for `Bar` - we'd expect objects of type `Bar` to dispatch, and thus behave, the same
as objects of all transitive supertypes. 

The astute reader will have noticed a slight inaccuracy in the above - in Julia, we can't actually have any objects
whose type is either `Foo`, `Bar`, `Union{}` or `Any`; every object has a concrete type, not an abstract, `Union` or
even `UnionAll` type. If we now introduce a concrete type `struct Concrete <: Bar end`:

```julia
Union{} <: Concrete <: Bar <: Foo <: Any
```

We can truly say that objects of type `Concrete` ought to be possible to pass into `myFunc`, and  similarly any function
that has an argument that's either unrestricted in its type or is explicitly typed `Any`. Said differently,
any function which has arguments that are typed `Any` must not throw an error upon trying to call it with any object (though
whether that call returns some form of a successful result is a different matter).

There are some implications with nesting function calls. For example, if we have a function `myAdd`:

```julia
function myAdd(a,b)
    a + b
end    
```

our function may _claim_ to accept anything, but the fact that we then call `+(a,b)` imposes an implicit constraint on both
`a` and `b` - they are no longer truly typed `Any`, but rather a type that we can't describe in our lattice.
We may say that `a` is typed "Anything that can be used as the first argument to `+`" and `b` is typed
"Anything that can be used as the second argument to `+`". A priori, this is ok - when we put two `Concrete`
objects into `myAdd`, the call to `myAdd` succeeds - it is only the call to `+` (which we haven't defined on `Concrete`, and no
fallback actually accepting plain `Any` exists) that actually fails.

So what we've effectively done is _restrict_ the abstract types of `a` and `b` to conform to some interface, that is
`+` must be defined on their types. We could encode this information in an abstract type, `abstract type Addable end`:

```julia
Union{} <: Addable <: Any
```

and annotate our `myAdd` with it, signaling to callers that they need to implement `+` in order to call `myAdd`:

```julia
function myAdd(a::Addable, b::Addable)
    a + b
end 
```

We've now successfully lifted the implicit requirement that `+` must be callable to `myAdd`, by restricting the
argument types of `myAdd`. Effectively, `(myFunc, +(::Addable, ::Addable))` form an interface, i.e. a function and the requirements
for using that function. In type parlance, we may say "`myFunc` has as preconditions on `a` and `b` that they subtype `Addable`" as well as
"`+` must implement a method that takes two `Addable`".

In the context of this package (and more generally, Julia as a whole) though, the main players really are `Addable` and `+`, not `myFunc`. There's nothing
we as users have to do to make `myFunc` work, once we have implemented `+` on our type `T <: Addable`, as that
is the sole requirement needed to be able to call `myFunc`. The important thing is though, because we subtype
`Addable` and thus claim to conform to the requirements of `myFunc`, we can treat the abstract type `Addable` *itself* as the interface
we need to be concerned with, as far as dispatch and not throwing a `MethodError` goes. Thus, we can say that
subtyping `Addable` is a statement of conformity to the requirements that `Addable` has, as well as an assurance
that our type can give at least the same, if not stronger guarantees, as other types who subtype `Addable`
can and/or need to be able to give.

Unfortunately, this kind of dependency is currently implicit in Julia. This is what [`@required`](@ref) allows a library
to change - by making the expected interface explicit, implementors can _check_ that they conform to the implicit
interface that `Addable` or any other abstract type at minimum requires.

Note that this does not preclude `Addable` from being used in other functions; for example a third party package
that's working with `Addable` objects may define a function like

```julia
function myThirdParty(::Addable)
    println("Got an Addable")
end
```

and while the guarantees provided by the `Addable` interface/type (there is a method on `+` taking two `Addable`) are not actually used by `myThirdParty`,
`myThirdParty` is free to require it anyway, without impacting what `Addable` requires of subtypes to implement in order
to conform to its specification.

### `Union`s of types

A `Union` of types more or less communicates "I can be one of these; assume only the intersection of their guarantees". In a concrete
example, if we have a function like

```julia
function myUnionFunc(sarr::Union{String,Vector{UInt8}})
    length(sarr)
end
```

we may only assume to be able to call functions that claim to be callable with objects of either type. In essence,
a `Union` widens the possible values of `sarr`, by weakening the guarantees we get from the type. In order
to write correct code, we mustn't assume more of `sarr` than we can infer from `Union{String, Vector{UInt8}}`,
unless we guard that assumption behind either an explicit type check, or a function barrier that disambiguates the
`Union`:

```julia
function myUnionFunc(sarr::Union{String,Vector{UInt8}})
    if sarr isa String
        sarr *= '!'
    else
        push!(sarr, UInt8('!'))
    end
end
```

or

```julia
function myUnionFunc(sarr::Union{String,Vector{UInt8}})
    pushpend!(sarr)
end

pushpend!(s::String) = s*'!'
pushpend!(a::Vector{UInt8}) = push!(a, UInt8('!'))
```

## Multiple Abstract Subtyping

Anyone cursorily familiar with abstract types and interfaces in Julia is going to come across the issue of
wanting to implement more than one interface, and being able to communicate to others that they have done so.
Most commonly, this comes up when implementing the `AbstractArray` interface, as well as the iteration
interface - most types that wish to be treated like an `AbstractArray` are also iterable in one form or another.
The situation can of course arise in any number of situations where more than one interface (or abstract type)
needs to be implemented.

This presents multiple issues:

  * While `AbstractArray` can be subtyped, it may not always be desirable to do so, perhaps because there is
    some other guarantees that a package author wants to give of their objects and/or has multiple similar objects
    that should conform to the same interface, grouping them under a common abstract supertype.
  * If there are multiple possible "variants" of the abstract supertype that are not quite mutually exclusive, we can't
    easily communicate the requirements of those not-quite mutually exclusive subtypes through either abstract types.
  * Barring being able to make the abstract supertype subtype `AbstractArray` (and by extension, the concrete subtypes
    as well), it won't be possible to subtype both `AbstractArray` and the custom abstract types at the same time.

So what to do?

The currently most often used solution to this issue is a pattern called Holy Types, after Tim Holy, who
first prototyped the design in [this issue](https://github.com/JuliaLang/julia/issues/2345#issuecomment-54537633), trying
to wrestle this exact issue for one of their types (only it was then `DenseArray` and `StridedArray`).
It has seen a lot of use over the years, but is not without issues, which we're going to explore shortly.

The general pattern goes like so; we first define an additional abstract type for our trait:

```julia
abstract type MyTrait end
```

followed by various variants of that trait, as well as some functions that decide whether an object
supports this trait, falling back to not supporting it:

```julia
struct IsTrait end
struct IsNotTrait end

isMyTrait(_) = IsNotTrait()
```

Now, instead of defining `struct Foo <: MyTrait end` and annotating our functions as `::MyTrait`, we do
the following:

```julia
struct Foo end

isMyTrait(::Foo) = IsTrait()

myInterfaceFunc(x) = _myInterfaceFunc(isMyTrait(x), x)

_myInterfaceFunc(::IsTrait, x) = "I implement the trait: $(typeof(x))"
_myInterfaceFunc(::IsNotTrait, x) = "I don't implement the trait: $(typeof(x))"
```

That is, leave the supertype declaration of `Foo` untyped, define an overload for the trait function `isMyTrait(::Foo)`
to say we support the trait, and finally call that trait function in our entry function to dispatch later on to
whether the argument supports the trait or not, and we're now free to have `Foo` subtype something else entirely.

So instead of 

```julia
abstract type MyTrait end

struct Foo <: MyTrait end

myInterfaceFunc(x::MyTrait) = "I implement the interface"
```

we have a layer of indirection, to be able to use dispatch while keeping `Foo` extensible to other interfaces.

So if in the trait example we want to have `Foo` implement another interface via traits:

```julia
abstract type AnotherTrait end

struct IsAnother <: AnotherTrait end
struct IsNotAnother <: Anothertrait end

isAnother(_) = IsNotAnother()

isAnother(::Foo) = IsAnother()
```

we can easily do so, making this approach very extensible in terms of allowing `Foo` to implement more than one interface.

As mentioned above though, there are downsides to this approach. First and foremost, if an interface has not been designed
with this pattern in mind, there is no place for other packages to hook into for providing specialized versions for their
own traits. That is, `myInterfaceFunc` needs to be written with this dispatch indirection of `_myInterfaceFunc` in mind.
Worse, `myInterfaceFunc` only knows about `isMyTrait`; so at best we can extend `MyTrait` with new variants,
but we cannot have `myInterfaceFunc` be aware of other traits without modifying the original function to either
dispatch on more traits, or replacing `isMyTrait` with a sort of meta-trait for other traits to hook themselves into.
None of this allows non-holy-traits based packages to be extended either way though.

The "way out" I'm envisioning comes back around to remembering what the meaning of subtyping an abstract type
is. Remember, when we subtype an abstract type we claim to give the same or stronger guarantees, and fulfill all
requirements expected of us from the abstract type. So, under that consideration, it ought to be natural to be
able to say that we can have types that declare a subtype relationship with more than one abstract super type,
effectively saying that we fulfill the requirements of more than one abstract type. This isn't currently possible in Julia,
but let's imagine it is and think about the consequences of what we might want to do.

Importantly, before we do - none of this means that these requirements & guarantees are machine checkable or formally verified.
Everything so far works just as well with "lazy" or only separate checking of those requirements as it does with
SAT based checking.

Without further ado, here's the `MyTrait` and `AnotherTrait` example from above. In order to facilitate multiple
subtyping, we're going to introduce a new kind of type, the dual to `Union` - `Meet`.

What is `Meet`? Well, in a lattice we can join different branches to find their common successor. This is `Union` -
the common successor of `String` and `Int` (both concrete types) is `Union{String,Int}`, i.e. a type that describes
either a `String` or an `Int`. Said differently, a `Union` describes an object of one of multiple types.

`Meet` is the dual to that - its the common predecessor in the lattice, describing that an object is of two or more types.
For example, if we have a `Meet{String, Int64}` we'd be claiming to have an object that is both a `String` and an `Int64`.
This may seem odd at first - after all, we can't actually have an object whose concrete type is both the concrete type `String`
and the concrete type `Int64`, so that type contains no values, like the empty `Union{}`.
However, once we consider creating the `Meet` of a number of abstract types, the interpretation makes a whole lot more
sense - e.g. `Meet{AbstractArray, Iterable}` would mean "A type that implements both the `AbstractArray` and `Iterable` interfaces".
This is exactly what we've been trying to do with Holy traits above. This allows us to fix the issues Holy Traits presented,
at least partially:

```julia
abstract type MyTrait end
abstract type AnotherTrait end

struct Foo <: Meet{MyTrait, AnotherTrait} end

myInterfaceFunc(::MyTrait) = "I'm a MyTrait!"
anotherInterface(::AnotherTrait) = "I'm AnotherTrait!"
```

and both calls to `myInterfaceFunc` and `anotherInterface` succeed without issues when a `Foo` is put in,
as a `Foo` is both a `MyTrait`, as well as a `AnotherTrait`. We can easily change `Foo` to implement a third
interface, without touching that interface definition at all, by simply implementing its required (or perhaps
even [`@required`](@ref) ;) ) methods and adding the interface type to the `Meet` in the struct declaration of `Foo`.
This is even semver compatible - no existing functionality ought to break by doing this, since the existing interface
must still be conformed to.

Similarly to the interpretation of `Meet` in structs, writing something like `abstract type Foo <: Meet{Bar,Baz} end`
can be interpreted as "subtypes of `Foo` need to implement/conform to both the `Bar` and `Baz` interfaces, as well as
any additional guarantees/requirements of `Foo`, if there are any".

Now, this isn't to say there aren't issues with `Meet` - for one, it doesn't allow third parties to extend
the supported interfaces of `Foo`, because which interfaces `Foo` implements is part of its type definition.
This is not an issue with Holy traits, due to their trait opt-in being disconnected from subtyping. This
is the flip-side of creating traits through dispatch, instead of through subtyping. However, how often this
is actually needed/a good idea in practice is questionable - after all, since this is just regular dispatch,
it's perfectly possible to just implement an interface on `Foo` directly, without explicitly subtyping the interface
type. The only issue is that this doesn't scale all too well, due to the fact that third party code then too
has to implement the interface explicitly for `Foo`, even if it could already be hooked into through dispatch,
were `Foo` aware of that subtyping relationship. Maybe that's an argument for allowing modification of subtyping
relationships after the type has been defined? My gut says that this is an even larger/more problematic change,
which would be worked around by upstreaming the implementation of the new interface, if it's of general interest.
Another alternative would be to couple the implicitness of the requirements imposed on abstract types closer to dispatch,
allowing types that simply implement a required interface to dispatch as if they had declared that subtyping relationship
in the first place.

Another argument I've previously heard against `Meet` was that it could increase ambiguities. For example:

```julia
abstract type MyTrait end
abstract type AnotherTrait end

struct Foo <: Meet{MyTrait, AnotherTrait} end

myInterfaceFunc(::MyTrait) = "I'm a MyTrait!"
myInterfaceFunc(::AnotherTrait) = "I'm AnotherTrait!"
```

When calling `myInterfaceFunc(Foo())`, there will be an ambiguity error. If there are more arguments to `myInterfaceFunc`,
there are many more opportunities for ambiguities. The only way to break this is to define either `myInterfaceFunc(::Foo)`
or `myInterfaceFunc(::Meet{MyTrait, AnotherTrait})`.

My counterpoint to this is that we have exactly the same issue with Holy traits, except worse. There we can't
even express this kind of relationship with the basic pattern. Consider the example from above again, introducing the Holy traits pattern:

```julia
abstract type MyTrait end
abstract type AnotherTrait end

struct IsMyTrait <: MyTrait end
struct IsNotMyTrait <: MyTrait end

struct IsAnother <: AnotherTrait end
struct IsNotAnother <: Anothertrait end

isMyTrait(_) = IsNotMyTrait()
isAnother(_) = IsNotAnother()

isMyTrait(::Foo) = IsMyTrait()
isAnother(::Foo) = IsAnother()

myInterfaceFunc(x) = _myInterfaceFunc(isMyTrait(x), x)
```

Now, without modifying `myInterfaceFunc`, we can't define `isMyTrait(::Foo) = IsAnotherTrait()` to
also support that kind of trait, because
that would require giving up on `MyTrait`. We could introduce a second layer of indirection, to perhaps
create a `Meet`-like of the supported traits, but that then exposes the true problem of the ambiguity between
which implementation of `_myInterfaceFunc` we'd like to use, if both exist. The only way out is yet again defining
`_myInterfaceFunc(::Foo)`, breaking the ambiguity.

In contrast, since both `MyTrait` and `AnotherTrait` share this trait function as part of their interface, `Foo` ought to have already been aware
of the  ambiguity, and implemented to specialized version `myInterfaceFunc(::Foo)` itself directly (or fallen back to `myInterfaceFunc(::Meet{MyTrait, AnotherTrait})`,
should that be available). The ambiguity needs to be broken some way or another, either by the `Foo` type for itself (it can't define the `Meet` version without piracy)
or by either `MyTrait` or `AnotherTrait` via a package extension (ideally in coordinate, as otherwise you may get conflicting definitions overwriting each other).

All of this will need to be discussed & thought through thoroughly though - there is no silver bullet.

## Relation to API stability

A related topic to "Abstract types are (implicit) interfaces" is how this interpretation relates to
version changes under Semver. If an abstract type declares an (implicit) interface, it follows that
changing that interface in a version bump requires considerations regarding stability, in order to
not create a breaking change where none was intended. Specifically, if a non-breaking change is
desired, at least the following must hold true:

 * A method that **wasn't** required to be implemented for subtypes must not become required to be implemented.
    * This is because adding a method to the required surface of the interface breaks existing implementations, by causing them
      to no longer be compliant with the interface.
    * I.e., you can't "grow" an interface. If such an extension is desired, it is better to
      have a new abstract type subtyping the existing abstract type, with the additional requirements added to that subtype.
 * A method that previously had certain requirements **must not** demand stronger requirements.
    * This again is because existing implementations may not be able to provide those stronger requirements.
    * An exception can be made if the newer, stronger requirements follow directly from the existing, weaker requirements; i.e.
      the "stronger" requirements were already there to begin with, but either undocumented or follow from deduction of the existing requirements.
 * A method previously giving some set of guarantees **can** weaken those guarantees.
    * This is because existing implementations, who provide the older, stronger guarantees, will still be compliant with
      the interface.
 * A method that was previously required in an interface, **can not** be made no longer required.
    * This is because code written with the old API surface and its assumptions of what names exist
      can break if it encounters a type working with the new surface, which may not implement a previously
      required method.
    * I.e., you can't "shrink" an interface. If such a change is desired, the options are either
        * to deprecate the functionality in a non-breaking change (being careful to keep existing code working!), and remove it in the next breaking change
          * This is easily coupled with moving a method to a subtype of the existing interface, directing users to subtype the subtype instead
            or by adding a direct supertype which has the weaker semantics (preferable, as that keeps user code the same).
        * or to add a new, distinct type providing the smaller interface and deprecating the old interface entirely

There are certainly more details regarding interface stability between versions - this list is not exhaustive.
An attempt at that exhaustiveness specific to Julia was recorded [here](https://github.com/JuliaLang/julia/issues/49973),
though there certainly is room for improvement and a more thorough calculus on what is permitted in terms of
a change in API. There is also the possibility of incorporating existing literature into this (most I could find
was in regards to empirical studies of API stability in Java, but even those results are bound to be useful). There
is also some existing work from the rust community about this - see the references list down below.

Finally, the large body of work on preconditions, postconditions & invariants is also related to this topic.

## Appendix

A large part of this discussion is inspired by looking at how other languages design their type system,
but especially poignant (and what ultimately sparked my attempt here to equate abstract types with interfaces)
was [Abstract types have existential type](https://dl.acm.org/doi/10.1145/44501.45065), by Mitchell & Plotkin, 1988.

It's a lucky coincidence that their work, combined with classic Liskov substitution and applied to the Julia
type system has this somewhat clean interpretation of abstract types as interfaces just dropping out - in spite
of multiple dispatch making things sometimes substantially harder. I attribute most of that lucky coincidence
to the design choice not to have abstract types have structure, i.e. the lack of structural inheritance in Julia.
Having abstract types only be relevant for dispatch means they map very cleanly to behavioral subtyping, as
that is all they can express - they have no structure other than the implicit requirements placed on them
by the functions they are passed to.

Admittedly, there are edge cases with `Meet`, in particular with
how `Meet` and `Union` interact in dispatch, that I have not yet had the time to fully describe here. I do hope that
their interactions fall cleanly out of the lattice geometry, though I haven't checked yet.

## References

 * [Tim Holy invents Holy traits](https://github.com/JuliaLang/julia/issues/2345#issuecomment-54537633)
 * [Abstract types have existential type](https://dl.acm.org/doi/10.1145/44501.45065)
 * [Measuring software library stability through historical version analysis](https://ieeexplore.ieee.org/abstract/document/6405296)
 * [APIDiff: Detecting API breaking changes](https://ieeexplore.ieee.org/abstract/document/8330249)
 * [cargo-semver-checks](https://github.com/obi1kenobi/cargo-semver-checks)
 * [cargo-public-api](https://github.com/Enselic/cargo-public-api)