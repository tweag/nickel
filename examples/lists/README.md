# Lists

This example shows how to write generic, statically typed functions in Nickel.
The code is illustrative: in practice, you should use the list stdlib functions
`list.map` and `list.fold` instead of redefining your own.

## Run

```
$ nickel -f lists.ncl export
```

## Playground

To see the effect of static typing, you can alter any of the `map` or `fold`
function to see the program fails before it is even executed.

Another aspect is how Nickel guard typed function from untyped code by using
contracts: at the end of the example, you can provide wrong arguments in the
calls to `myListLib.map` or `myListLib.fold` to see contracts error appear:

```
-    let l = myListLib.map (fun x => x+1) [1, 2, 3, 4, 5, 6] in
+    let l = myListLib.map [1, 2, 3, 4, 5, 6] (fun x => x+1) in
```
