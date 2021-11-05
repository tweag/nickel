let ctr = {
  Derivation | doc "
            A package natively specified in Nickel."
          = {
    name | doc "
           The package name."
         | Str,

    buildInputs | doc "
                  The list of inputs for this package, specified as a
                  `NixPackage`."
                | List #NixPackage,
    // Many more missing
  },

  Shell = Derivation & {},

  NixPackage | doc "
               Interchange format representing a package only accessible on the
               nix side. The nix interop code then translate this back to an
               actual package.

               # Example

               ```
               myPkg = {
                 name  = \"myPkg\",
                 buildInputs | List #NixPackage = [{package = \"hello\"}]\",
               }
               ```
               This will give the following attribute sets once imported in Nix:
               ```
               {
                 name = \"myPkg\",
                 buildInputs = [nixpkgs.hello],
               }
               ```"
             = {
    package | Str
            | doc "
              The package name, given as a string. Dot-separated paths are not yet
              supported",

    input | Str
          | doc "
            The inputs where to fetch the package from. Must be a variable name
            that is in scope on the Nix side."
          | default = "nixpkgs",

    _type | doc "
            Used by the interop Nix code. Forced to a fixed value. Please
            do not override."
          = "package",
  }
} in
builtins.serialize `Json (
  {
    name = "nickel",
    buildInputs = [{package = "hello"}],
  } | #ctr.Shell
) == builtins.serialize `Json {
  name = "nickel",
  buildInputs = [{input = "nixpkgs", package = "hello", "_type" = "package"}],
}
