module TestExt

using Test, InteractiveUtils, RequiredInterfaces
const RI = RequiredInterfaces

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

throwNotAnInterface(interface) = throw(ArgumentError("`$interface` is not a registered interface."))

function RI.check_implementations(interface::Type, types=nonabstract_subtypes(interface))
    RI.isInterface(interface) || throwNotAnInterface(interface)
    @testset "Interface Check: $interface" begin
    @testset "$implementor" for implementor in types
        @test RI.check_interface_implemented(interface, implementor)
    end
    end
end

valid_globalref(gr) = gr.mod === RequiredInterfaces && gr.name === :NotImplementedError

function RI.check_interface_implemented(interface::Type, implementor::Type)
    RI.isInterface(interface) || throwNotAnInterface(interface)
    isabstracttype(implementor) && throw(ArgumentError("Checking abstract types for compliance is currently unsupported."))
    sigs = RI.methods(RI.getInterface(interface))
    failures = Tuple{Any,Tuple}[]
    for sig in sigs
        func, interfacetypes = sig
        argtypes = ntuple(length(interfacetypes)) do i
            itype = interfacetypes[i]
            itype === interface ? implementor : itype
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

end
