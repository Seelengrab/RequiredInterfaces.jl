using Revise

using Documenter 
using RequiredInterfaces

DocMeta.setdocmeta!(RequiredInterfaces, :DocTestSetup, :(using RequiredInterfaces); recursive=true)

function builddocs(clear=false)
    clear && rm(joinpath(@__DIR__, "build"), force=true, recursive=true)
    makedocs(
        sitename="RequiredInterfaces.jl Documentation",
        format = Documenter.HTML(
            prettyurls = get(ENV, "CI", nothing) == true
        ),
        pages = [
            "Main Page" => "index.md",
            "Examples" => [
                "Basic Example" =>  "examples/basic.md"
            ]
            "API Reference" => "api.md"
        ]
    )
end

builddocs()

!isinteractive() && deploydocs(
   repo = "github.com/Seelengrab/RequiredInterfaces.jl.git",
   devbranch = "main",
   push_preview = true
)