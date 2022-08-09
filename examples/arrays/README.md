# Arrays

This example shows how to write generic, statically typed functions in Nickel.
The code is illustrative: in practice, you should use the array stdlib functions
`array.map` and `array.fold` instead of redefining your own.

## Run

```console
nickel -f arrays.ncl export
```

## Playground

To see the effect of static typing, you can alter any of the `map` or `fold`
function to see the program fails before it is even executed.

Another aspect is how Nickel guard typed function from untyped code by using
contracts: at the end of the example, you can provide wrong arguments in the
calls to `my_array_lib.map` or `my_array_lib.fold` to see contracts error appear:

```diff
-    let l = my_array_lib.map (fun x => x+1) [1, 2, 3, 4, 5, 6] in
+    let l = my_array_lib.map [1, 2, 3, 4, 5, 6] (fun x => x+1) in
```
