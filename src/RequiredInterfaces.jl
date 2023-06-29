module RequiredInterfaces

using Test, InteractiveUtils

export @required, NotImplementedError

"""
    isInterface(::Type{T}) -> Bool

Return whether the given (abstract) type is a recognized interface.

Throws an `ArgumentError` if the given type is not a registered interface.
"""
isInterface(_) = false

"""
    getInterfaceFuncs(::Type{T}) -> Vector{Tuple{F, NTuple{N, <:Type}}}

Return the list of functions and their signatures that are considered required for the given interface `T`.

Throws an `ArgumentError` if the given type is not a registered interface.
"""
function getInterfaceFuncs end

"""
    @required Interface func(::Foo, ::Interface)
    @required Interface function func(::Foo, ::Interface) end

Marks all occurences of `Interface` in the given function signature as part of the interface `Interface`.
Also defines a fallback method which throws a [`NotImplementedError`](@ref) when called with an argument that
doesn't implement this mandatory function.

Throws an `ArgumentError` if `Interface` is not an abstract type or the given expression doesn't conform to the
two shown styles. The function body of the second style is expected to be empty - `@required` can't be used to
mark fallback implementations as part of a user-implementable interface. Its sole purpose is marking
parts of an API that a user needs to implement to be able to have functions expecting that interface work.
"""
macro required(T::Symbol, expr::Expr)
    if expr.head === :function 
        funcsig = expr.args[1]
    elseif expr.head === :call
        funcsig = expr
        expr = Expr(:function, expr, :())
    else 
        throw(ArgumentError("Given expression is not a valid interface definition!"))
    end
    sig = Symbol[ a isa Symbol ? :Any : last(a.args) for a in funcsig.args[2:end] ]
    msg = error_msg(funcsig.args[1], sig)
    escT = esc(T)
    isabstracttype(getproperty(__module__, T)) || throw(ArgumentError("Given `$T` is not an abstract type!"))
    escFunc = esc(funcsig.args[1])
    expr.args[1] = esc(expr.args[1])
    push!(expr.args[2].args, :(throw(NotImplementedError(string($escT), $msg))))
    res = ntuple(length(sig)) do i
        getproperty(__module__, sig[i])
    end

    funcdefs = quote
        RequiredInterfaces.isInterface(::Type{$escT}) = true
        function RequiredInterfaces.getInterfaceFuncs(::Type{$escT})
            isInterface($escT) || throwNotAnInterface($escT)
            if !haskey(getInterfaceDict(), $escT)
                arr = getInterfaceDict()[$escT] = Tuple{Any, Tuple}[]
                push!(arr, ($escFunc, $res))
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
    # @info "macro expanded"  retcode
    return retcode
end

function error_msg(f, sig)
    msg = "$f(::" 
    msg *= join(sig, ", ::")
    msg * ")"
end

const INTERFACES = Dict{Type, Vector{Tuple{Any, Tuple}}}()
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

```jldoctest
julia> abstract type Foo end

julia> bar(::Foo, ::Int) = throw(NotImplementedError("Foo", "bar(::T, ::Int)"))

julia> struct Baz <: Foo end

julia> bar(Baz(), 1)
ERROR: NotImplementedError: The called method is part of a fallback definition for the `Foo` interface.
Please implement `bar(::T, ::Int)` for your type `T <: Foo`.
Stacktrace:
 [1] bar(::Baz, ::Int64)
   @ Main ./REPL[4]:1
 [2] top-level scope
   @ REPL[8]:1

# note how `MethodError` indicates that this isn't intended to be called
julia> bar(1, Baz())
ERROR: MethodError: no method matching bar(::Int64, ::Baz)
Stacktrace:
 [1] top-level scope
   @ REPL[6]:1
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

function concrete_subtypes(T=Any)::Vector{DataType}
    isabstracttype(T) || throw(ArgumentError("Only abstract types are supported! Got unsupported type: `$T`"))
    subs = subtypes(T)
    ret = filter(isconcretetype, subs)
    filter!(isabstracttype, subs)

    while !isempty(subs)
        ntype = popfirst!(subs)
        ntype == Any && continue
        nsubs = subtypes(ntype)
        append!(ret, Iterators.filter(isconcretetype, nsubs))
        append!(subs, Iterators.filter(isabstracttype, nsubs))
    end

    ret
end

throwNotAnInterface(interface) = throw(ArgumentError("`$interface` is not a registered interface."))

function check_implementations(interface::Type, types=concrete_subtypes(interface))
    isInterface(interface) || throwNotAnInterface(interface)
    @testset "Interface Check: $implementor" for implementor in types
        @testset let interface = interface, implementor = implementor
            @test check_interface_implemented(interface, implementor)
        end
    end
end

function check_interface_implemented(interface::Type, implementor::Type)
    isInterface(interface) || throwNotAnInterface(interface)
    isconcretetype(implementor) || throw(ArgumentError("Checking abstract types for compliance is currently unsupported."))
    sigs = getInterfaceFuncs(interface)
    failures = []
    for sig in sigs
        func, interfacetypes = sig
        argtypes = ntuple(length(interfacetypes)) do i
            itype = interfacetypes[i]
            itype === interface ? implementor : itype
        end
        ct, rettype = only(code_typed(func, argtypes))
        rettype !== Union{} && continue # if it infers, we can't throw our error
        length(ct.code) >= 2 || continue # likely implemented
        errorExpr = ct.code[2]
        errorExpr isa Expr || continue # not our Error? could be a change in IR
        errorExpr.head === :call || continue
        isempty(errorExpr.args) && continue # weird Expr?
        gr =  errorExpr.args[1]
        gr isa GlobalRef && gr.binding.value === NotImplementedError && push!(failures, sig) # found one! 
    end

    return isempty(failures) || return failures
end

end # module RequiredInterfaces
