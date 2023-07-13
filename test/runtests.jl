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

@testset "All tests" begin
    @testset "Correct implementation" begin
        @testset "$s" for (s, interface, impl, funcs, interface_sigs) in
                (("Basic",         TestInterface,     TestImpl,          [testfunc],               [(Int, TestInterface)]),
                 ("Parametric",    TestParametric,    ParametricImpl,    [paramfunc],              [(TestParametric,)]),
                 ("SubParametric", TestParametric,    SubParametricImpl, [paramfunc],              [(TestParametric,)]),
                 ("SubParametric", TestSubParametric, SubParametricImpl, [subparamfunc],           [(TestSubParametric,)]),
                 ("MultiFunc",     TestMultiFunc,     SubMultiFuncImpl,  [multifunc1, multifunc2], [(TestMultiFunc,), (TestMultiFunc,)]))
            intr = RI.getInterface(interface)
            @test RI.functions(intr) == funcs
            @test all(Base.splat(==), zip(RI.methods(intr), zip(funcs, interface_sigs)))
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
            comp = [ (nofallback, (SubNoFallbackViolator,)),
                     (paramfunc, (SubNoFallbackViolator,))]
            @test intr == comp
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
end
