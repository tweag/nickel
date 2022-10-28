
let Assert = fun l x => x || %blame% l in
(import "nix/basics.nix" | Array Bool)
|> array.foldl (fun x y => (x | Assert) && y) true
