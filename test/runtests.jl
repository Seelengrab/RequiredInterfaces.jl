using Test, RequiredInterfaces
const RI = RequiredInterfaces

abstract type TestInterface end
@required TestInterface testfunc(::Int, ::TestInterface)

struct TestImpl <: TestInterface end
testfunc(a::Int, ::TestImpl) = a

struct TestViolator <: TestInterface end

abstract type TestParametric{T} end
@required TestParametric paramfunc(::TestParametric)

struct ParametricImpl{T} <: TestParametric{T} end
paramfunc(::ParametricImpl{T}) where T = T

abstract type TestSubParametric{T} <: TestParametric{T} end
# this type provides a default fallback for `paramfunc`!
paramfunc(::TestSubParametric{T}) where T = T
@required TestSubParametric subparamfunc(::TestSubParametric)

abstract type TestSubNoFallback{T} <: TestParametric{T} end
@required TestSubNoFallback nofallback(::TestSubNoFallback)

struct SubParametricImpl{T} <: TestSubParametric{T} end
# only the additional interface needs to be implemented
subparamfunc(::SubParametricImpl{T}) where T = T

struct SubNoFallbackViolator{T} <: TestSubNoFallback{T} end

struct SubNoFallbackImpl{T} <: TestSubNoFallback{T} end
nofallback(::SubNoFallbackImpl{T}) where T = T
paramfunc(::SubNoFallbackImpl{T}) where T = T

abstract type TestMultiFunc end
@required TestMultiFunc begin
    multifunc1(::TestMultiFunc)
    multifunc2(::TestMultiFunc)
end

struct SubMultiFuncImpl <: TestMultiFunc end
multifunc1(::SubMultiFuncImpl) = 1
multifunc2(::SubMultiFuncImpl) = 2

module DRMod
    using RequiredInterfaces
    abstract type DoubleRequired end
    @required DoubleRequired drfunc1(::DoubleRequired)
end

module TypeMod
    using RequiredInterfaces
    abstract type TypeInterface end
end

module LinArrayMod
    using RequiredInterfaces
    abstract type LinearArray{T,N} <: AbstractArray{T,N} end
end

module CallableMod
    using RequiredInterfaces
    abstract type CallableAbstract end
end

const interfaces = (
    ("Basic",                        TestInterface,     TestImpl,          [testfunc],                [(Int, TestInterface)]),
    ("Parametric",                   TestParametric,    ParametricImpl,    [paramfunc],               [(TestParametric,)]),
    ("TestParametric+SubParametric", TestParametric,    SubParametricImpl, [paramfunc],               [(TestParametric,)]),
    ("SubParametric",                TestSubParametric, SubParametricImpl, [subparamfunc, paramfunc], [(TestSubParametric,), (TestParametric,)]),
    ("MultiFunc",                    TestMultiFunc,     SubMultiFuncImpl,  [multifunc1, multifunc2],  [(TestMultiFunc,), (TestMultiFunc,)]),
    ("NoFallback",                   TestSubNoFallback, SubNoFallbackImpl, [nofallback, paramfunc],   [(TestSubNoFallback,), (TestParametric,)])
)

@testset "All tests" begin
    @testset "Interface Equality & hashing" begin
        a = RI.getInterface(TestInterface)
        b = RI.getInterface(TestParametric)
        @test a == a && hash(a) == hash(a)
        @test a != b && hash(a) != hash(b)
        # ensure differing lengths lead to different results
        c = RI.Interface(a.type, copy(a.meths))
        push!(c.meths, (:baz, ()))
        @test a != c && hash(a) != hash(c)
    end
    
    @testset "Correct implementation" begin
        @testset "$s" for (s, interface, impl, funcs, interface_sigs) in interfaces
            intr = RI.getInterface(interface)
            @test Set(RI.functions(intr)) == Set(funcs)
            @test Set(RI.methods(intr)) == Set(zip(funcs, interface_sigs))
            @test RI.check_interface_implemented(interface, impl)
        end
    end
    @testset "Not completely implemented" begin
        @testset "Basic" begin
            intr = RI.check_interface_implemented(TestInterface, TestViolator)
            @test intr isa Vector{Tuple{Any, Tuple}}
            func, sig = only(intr)
            @test func === testfunc
            @test sig == (Int, TestViolator)
        end
        @testset "Inherited interfaces" begin
            intr = RI.check_interface_implemented(TestSubNoFallback, SubNoFallbackViolator)
            @test intr isa Vector{Tuple{Any, Tuple}}
            comp = Set([ (nofallback, (SubNoFallbackViolator,)),
                         (paramfunc, (SubNoFallbackViolator,))])
            @test Set(intr) == comp
        end
    end

    @testset "Double `@required`" begin
        try
            @eval DRMod @required DoubleRequired drfunc2(::DoubleRequired)
        catch e
            @test e isa LoadError
            @test e.error isa ArgumentError
        end
    end

    @testset "Interface taking a type" begin
        @eval TypeMod begin
            @required TypeInterface foo(::Type{TypeInterface})
            struct TypeViolator <: TypeInterface end
            struct TypeImpl <: TypeInterface end
            foo(::Type{TypeImpl}) = "implemented!"
        end
        @test RI.check_interface_implemented(TypeMod.TypeInterface, TypeMod.TypeImpl)
        intr = RI.check_interface_implemented(TypeMod.TypeInterface, TypeMod.TypeViolator)
        @test intr isa Vector{Tuple{Any, Tuple}}
        func, sig = only(intr)
        @test func === TypeMod.foo
        @test sig == (Type{TypeMod.TypeViolator},)
    end

    @testset "Extending Base" begin
        @eval LinArrayMod begin
            @required LinearArray begin
                Base.size(::LinearArray)
                Base.getindex(::LinearArray, ::Int)
            end
        end
        meths = RI.methods(RI.getInterface(LinArrayMod.LinearArray))
        @test meths isa Vector{Tuple{Any,Tuple}}
        @test (Base.size, (LinArrayMod.LinearArray,)) in meths
        @test (Base.getindex, (LinArrayMod.LinearArray,Int)) in meths
        @test length(meths) == 2
    end


    @testset "Callable as interface" begin
        @eval CallableMod begin
            @required CallableAbstract begin
                (c::CallableAbstract)()
            end
            struct CallableViolator <: CallableAbstract end
            struct CallableImpl <: CallableAbstract end
            (::CallableImpl)() = "implemented"
        end
        @test RI.check_interface_implemented(CallableMod.CallableAbstract, CallableMod.CallableImpl)
        intr = RI.check_interface_implemented(CallableMod.CallableAbstract, CallableMod.CallableViolator)
        @test intr isa Vector{Tuple{Any, Tuple}}
        func, sig = only(intr)
        @test func === CallableMod.CallableAbstract
        @test sig == ()
    end
end
