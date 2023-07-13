module RequiredInterfaces

using Test, InteractiveUtils, Logging

export @required, NotImplementedError

"""
    isInterface(::Type{T}) -> Bool

Return whether the given (abstract) type is a recognized interface.

Throws an `ArgumentError` if the given type is not a registered interface.
"""
isInterface(_) = false

"""
    getInterface(::Type{T}) -> Interface

Return the [`Interface`](@ref) described by  the type `T`.

Throws a `MethodError` if the given type is not a registered interface.
"""
function getInterface end

"""
    @required MyInterface func(::Foo, ::MyInterface)
    @required MyInterface function func(::Foo, ::MyInterface) end
    @required MyInterface begin
        foo(::B, ::MyInterface)
        bar(::A, ::MyInterface)
    end

Marks all occurences of `MyInterface` in the given function signatures as part of the interface `MyInterface`.
Also defines fallback methods, which throw a [`NotImplementedError`](@ref) when called with an argument that
doesn't implement this mandatory function.

Throws an `ArgumentError` if `MyInterface` is not an abstract type or the given expression doesn't conform to the
three shown styles. The function body of the second style is expected to be empty - `@required` can't be used to
mark fallback implementations as part of a user-implementable interface. Its sole purpose is marking
parts of an API that a user needs to implement to be able to have functions expecting that interface work.
"""
macro required(T::Symbol, expr::Expr)
    escT = esc(T)
    abstrType = getproperty(__module__, T)
    isabstracttype(abstrType) || throw(ArgumentError("Given `$T` is not an abstract type!"))
    applicable(getInterface, abstrType) && throw(ArgumentError("`$T` is already registered as an interface.\nUse the `begin` block version to specify multiple methods as part of the interface `$T`."))

    # Normalize argument expression
    if expr.head === :function
        expr = Expr(:block, expr)
    elseif expr.head === :call
        expr = Expr(:block, Expr(:function, expr, :()))
    elseif expr.head === :block
        foreach(enumerate(expr.args)) do (i,e)
            e isa LineNumberNode && return
            e.head === :function && return
            e.head !== :call && throw(ArgumentError("Given block contains code not part of the interface definition!"))
            expr.args[i] = Expr(:function, e, :())
        end
    else 
        throw(ArgumentError("Given expression is not a valid interface definition!"))
    end

    arr = Expr(:vect)
    local escFunc

    for e in expr.args
        e isa LineNumberNode && continue
        funcsig = e.args[1]
        sig = Symbol[ a isa Symbol ? :Any : last(a.args) for a in funcsig.args[2:end] ]
        msg = error_msg(funcsig.args[1], sig)
        escFunc = esc(funcsig.args[1])
        e.args[1] = esc(e.args[1])
        push!(e.args[2].args, :(throw(NotImplementedError(string($escT), $msg))))
        res = ntuple(length(sig)) do i
            getproperty(__module__, sig[i])
        end
        push!(arr.args, :(($escFunc, $res)))
    end

    funcdefs = quote
        RequiredInterfaces.isInterface(::Type{$escT}) = true
        function RequiredInterfaces.getInterface(::Type{$escT})
            isInterface($escT) || throwNotAnInterface($escT)
            if !haskey(getInterfaceDict(), $escT)
                getInterfaceDict()[$escT] = Interface($escT, $arr)
            else
                getInterfaceDict()[$escT]
            end
        end
    end

    retcode = :(
        $expr;
        $funcdefs;
        $escFunc
    )
    return retcode
end

function error_msg(f, sig)
    msg = "$f(::"
    msg *= join(sig, ", ::")
    msg * ")"
end

"""
    Interface

A struct describing the notion of an (abstract-)type-based interface.
"""
struct Interface
    type::Type
    meths::Vector{Tuple{Any,Tuple}}
end

Base.:(==)(a::Interface, b::Interface) = a.type == b.type && all(splat(==), zip(a.meths, b.meths))
Base.hash(a::Interface, u::UInt) = hash(a.type, foldr(hash, a.meths; init=u))

"""
    functions(i::Interface)

Returns the functions that are required by this interface.
"""
function functions(i::Interface)
    unique!(map(methods(i)) do (f,_)
        f
    end)
end

"""
    methods(i::Interface)

Return the methods (i.e., functions and their signatures) required to be implemented by this interface.

See also [`functions`](@ref).
"""
methods(i::Interface) = i.meths

"""
    interfaceType(i::Interface)

Return the (abstract) type associated with this interface.
"""
interfaceType(i::Interface) = i.type

const INTERFACES = Dict{Type, Interface}()
getInterfaceDict() = INTERFACES

"""
    NotImplementedError(interface::String, method::String) <: Exception

Describes that a given method (in form of a `String` describing the signature) that's part of the
given `interface` has not been implemented.

The given method should have the form `method(..., ::T, ...)`, where `...` are other arguments.
The message displayed by this Exception refers to `T` directly, so there's no need to give any particular
subtype here.

Compared to `MethodError`, a `NotImplementedError` communicates "there should be a method to call here,
but it needs to be implemented by the developer making use of the interface". This is mostly used through
the [`@required`](@ref) macro, which handles the message generation for you.

## Examples

Note how `MethodError` in the example below indicates that the method isn't intended to be called.

```jldoctest; filter = r"#unused#"
julia> using RequiredInterfaces: NotImplementedError

julia> abstract type Foo end

julia> bar(::Foo, ::Int) = throw(NotImplementedError("Foo", "bar(::T, ::Int)"))
bar (generic function with 1 method)

julia> struct Baz <: Foo end

julia> bar(Baz(), 1)
ERROR: NotImplementedError: The called method is part of a fallback definition for the `Foo` interface.
Please implement `bar(::T, ::Int)` for your type `T <: Foo`.
Stacktrace:
 [1] bar(#unused#::Baz, #unused#::Int64)
   @ Main ./none:1
 [2] top-level scope
   @ none:1

julia> bar(1, Baz())
ERROR: MethodError: no method matching bar(::Int64, ::Baz)
Stacktrace:
 [1] top-level scope
   @ none:1
```
"""
struct NotImplementedError <: Exception
    interface::String
    func::String
end

function Base.showerror(io::IO, nie::NotImplementedError)
    printstyled(io, "NotImplementedError: "; color=:red)
    print(io, "The called method is part of a fallback definition for the `", nie.interface, "` interface.\n",
              "Please implement `", nie.func, "` for your type `T <: ", nie.interface, "`.")
end

function nonabstract_subtypes(T=Any)
    isabstracttype(T) || throw(ArgumentError("Only abstract types are supported! Got unsupported type: `$T`"))
    subs = subtypes(T)
    ret = filter(!isabstracttype, subs)
    filter!(isabstracttype, subs)

    while !isempty(subs)
        ntype = popfirst!(subs)
        ntype == Any && continue
        nsubs = subtypes(ntype)
        append!(ret, Iterators.filter(!isabstracttype, nsubs))
        append!(subs, Iterators.filter(isabstracttype, nsubs))
    end

    ret
end

function interface_supertypes(T::Type)
    (T isa Union || T === Union{}) && throw(ArgumentError("Can't get interface superttypes of a union: `$T`"))
    interface_supertypes!(Type[], T)
end

function interface_supertypes!(res, T::Type)
    T === Any && return res
    isInterface(T) && push!(res, T)
    interface_supertypes!(res, supertype(T))
end

throwNotAnInterface(interface) = throw(ArgumentError("`$interface` is not a registered interface."))

function check_implementations(interface::Type, types=nonabstract_subtypes(interface))
    isInterface(interface) || throwNotAnInterface(interface)
    @testset "Interface Check: $interface" begin
    @testset "$implementor" for implementor in types
        @test check_interface_implemented(interface, implementor)
    end
    end
end

valid_globalref(gr) = gr.mod === RequiredInterfaces && gr.name === :NotImplementedError

function check_interface_implemented(interface::Type, implementor::Type)
    isInterface(interface) || throwNotAnInterface(interface)
    isabstracttype(implementor) && throw(ArgumentError("Checking abstract types for compliance is currently unsupported."))
    sigs = mapreduce(methods ∘ getInterface, hcat, interface_supertypes(interface))
    failures = Tuple{Any, Tuple}[]
    for sig in sigs
        func, interfacetypes = sig
        argtypes = ntuple(length(interfacetypes)) do i
            itype = interfacetypes[i]
            interface <: itype ? implementor : itype
        end
        matches = Base.methods(func, argtypes)
        if length(matches) != 1
            found = map(matches) do m
                typs = if m.sig isa DataType
                    m.sig.types
                elseif m.sig isa UnionAll
                    m.sig.body.types
                end
                (typs[2:end]...,)
            end
            filter!(!=(interfacetypes), found)
            @warn "Not all signatures required matching $func$argtypes are implemented." Found=found
            push!(failures, (func, argtypes))
            continue
        end
        ct, rettype = only(code_typed(func, argtypes))
        rettype !== Union{} && continue # if it infers, we can't throw our error
        isempty(ct.code) && continue # empty function
        offset = ct.code[1] isa Expr && ct.code[1].head === :code_coverage_effect
        length(ct.code) < 2 && continue # function with only one expr - not our code
        offset += ct.code[2] isa Expr && ct.code[2].head === :code_coverage_effect
        length(ct.code) < (offset + 2) && continue # function with 2 expr not from us
        errorExpr = ct.code[offset + 2]
        errorExpr isa Expr || continue # not our Error? could be a change in IR
        errorExpr.head === :call || continue
        isempty(errorExpr.args) && continue # weird Expr?
        gr =  errorExpr.args[1]
        if gr isa GlobalRef && valid_globalref(gr)
            push!(failures, (func, argtypes)) # found one!
        end
    end

    return isempty(failures) || return failures
end

end # module RequiredInterfaces