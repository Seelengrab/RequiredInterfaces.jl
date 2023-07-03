using Test, RequiredInterfaces
const RI = RequiredInterfaces

abstract type TestInterface end
@required TestInterface testfunc(::Int, ::TestInterface)

struct TestImpl <: TestInterface end
testfunc(a::Int, ::TestImpl) = a

struct TestViolator <: TestInterface end

@testset "All tests" begin
    @testset "Correct implementation" begin
        intr = RI.getInterface(TestInterface)
        @test RI.functions(intr) == [testfunc]
        @test RI.methods(intr) == Tuple{Any,Tuple}[(testfunc, (Int, TestInterface))]
        @test RI.check_interface_implemented(TestInterface, TestImpl)
    end
    @testset "Not completely implemented" begin
        @test RI.check_interface_implemented(TestInterface, TestViolator) isa Vector{Tuple{Any,Tuple}}
        intr = RI.check_interface_implemented(TestInterface, TestViolator)
        func, sig = only(intr)
        @test func === testfunc
        @test sig === (Int, TestViolator)
    end 
end
