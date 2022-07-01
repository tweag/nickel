```
[{x = "hello" ++ "foo"}] | Array MyRec2

let %2 =
(%assume% ($array MyRec2)
    # <label>
    )
  (let %1 = let %0 = "hello" ++ "foo" in { x = %0, } in [ %1 ])
in

%2 | Array MyRec2

#1
%2

#2
(%assume% ($array MyRec2)
    # <label>
    )
  (let %1 = let %0 = "hello" ++ "foo" in { x = %0, } in [ %1 ])

#3
Stack: CONT1 ASSUME

$array MyRec2

#4
Stack: Arg1,Arg2,Arg3
CONT1 Assume

$array

#5
Stack:
Typeof
Ifte
CONT1 Assume

Env:
elt <- MyRec2
l <- label
t <- (let ...)

(let %1 = let %0 = "hello" ++ "foo" in { x = %0, } in [ %1 ])

PUSH1
PUSH0 ("hello" ++ "foo")

[ %1 ]
Update frame

#6

Stack:
ArrayLazyAssumeCont
...

Env:
elt <- MyRec2
l <- label
t <- ([ %1 ], %1 <- ...)
```
