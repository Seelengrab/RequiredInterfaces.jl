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

struct SubParametricImpl{T} <: TestSubParametric{T} end
# only the additional interface needs to be implemented
subparamfunc(::SubParametricImpl{T}) where T = T

@testset "All tests" begin
    @testset "Correct implementation" begin
        @testset "$s" for (s, interface, impl, func, interface_sig) in
                (("Basic", TestInterface, TestImpl, testfunc, (Int, TestInterface)),
                 ("Parametric", TestParametric, ParametricImpl, paramfunc, (TestParametric,)),
                 ("SubParametric", TestParametric, SubParametricImpl, paramfunc, (TestParametric,)),
                 ("SubParametric", TestSubParametric, SubParametricImpl, subparamfunc, (TestSubParametric,)))
            intr = RI.getInterface(interface)
            @test RI.functions(intr) == [func]
            @test RI.methods(intr) == Tuple{Any,Tuple}[(func, interface_sig)]
            @test RI.check_interface_implemented(interface, impl)
        end
    end
    @testset "Not completely implemented" begin
        @test RI.check_interface_implemented(TestInterface, TestViolator) isa Vector{Tuple{Any,Tuple}}
        intr = RI.check_interface_implemented(TestInterface, TestViolator)
        func, sig = only(intr)
        @test func === testfunc
        @test sig === (Int, TestViolator)
    end 
end
