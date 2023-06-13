# test.type = 'pass'
let ctr = {
  Derivation 
    | doc m%"
            A package natively specified in Nickel.
          "%
          = {
    name | doc m%"
              The package name.
           "%
         | String,

    buildInputs | doc m%"
                  The list of inputs for this package, specified as a
                  `NixPackage`."%
                | Array NixPackage,
    # Many more missing
  },

  Shell = Derivation & {},

  NixPackage | doc m%"
               Interchange format representing a package only accessible on the
               nix side. The nix interop code then translate this back to an
               actual package.

               # Example

               ```
               myPkg = {
                 name  = \"myPkg\",
                 buildInputs | Array NixPackage = [{package = \"hello\"}]\",
               }
               ```
               This will give the following attribute sets once imported in Nix:
               ```
               {
                 name = \"myPkg\",
                 buildInputs = [nixpkgs.hello],
               }
               ```"%
             = {
    package | String
            | doc m%"
              The package name, given as a string. Dot-separated paths are not yet
              supported"%,

    input | String
          | doc m%"
            The inputs where to fetch the package from. Must be a variable name
            that is in scope on the Nix side."%
          | default = "nixpkgs",

    _type | doc m%"
            Used by the interop Nix code. Forced to a fixed value. Please
            do not override."%
          = "package",
  }
} in
std.serialize 'Json (
  {
    name = "nickel",
    buildInputs = [{package = "hello"}],
  } | ctr.Shell
) == std.serialize 'Json {
  name = "nickel",
  buildInputs = [{input = "nixpkgs", package = "hello", "_type" = "package"}],
}
