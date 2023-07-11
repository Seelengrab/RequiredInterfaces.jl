liveserver = "liveserver" in ARGS
if liveserver
    using Revise
    Revise.revise()
end

using Documenter 
using RequiredInterfaces

DocMeta.setdocmeta!(RequiredInterfaces, :DocTestSetup, :(using RequiredInterfaces); recursive=true)

function builddocs(clear=false)
    clear && rm(joinpath(@__DIR__, "build"), force=true, recursive=true)
    makedocs(
        sitename="RequiredInterfaces.jl",
        format = Documenter.HTML(
            prettyurls = get(ENV, "CI", nothing) == true
        ),
        pages = [
            "Main Page" => "index.md",
            "Examples" => [
                "examples/basic.md",
                "examples/multifuncs.md",
                "examples/testing.md"
            ],
            "About Interfaces" => "interfaces.md",
            "API Reference" => "api.md"
        ]
    )
end

builddocs()

!isinteractive() && !liveserver && deploydocs(
   repo = "github.com/Seelengrab/RequiredInterfaces.jl.git",
   devbranch = "main",
   push_preview = true
)