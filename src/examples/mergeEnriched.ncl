let or = Promise(Bool -> Bool -> Bool, fun a b =>
    if a then true else b) in
let and = Promise(Bool -> Bool -> Bool, fun a b =>
    if a then (if b then true else false) else false) in
let oneOrTwo = fun l x =>
    if and (isNum x)
           (or (isZero (x + (-1))) (isZero (x + (-2)))) then
        x
    else blame l in
let confDefault = {
    pathLibc = Docstring("Installation path of the libc",
               Default("/lib/x86_64-linux-gnu/libc.so"));
    gcc = Docstring("Gcc command",
          Default("gcc -Werror -03"));
    specialFlag = Docstring("Set 1 for default, 2 for optimized",
                  Default(1));
} in
let confValidation = {
    pathLibc = Contract(Str);
    gcc = Contract(Str);
    specialFlag = Contract(#oneOrTwo);
} in
let confValAndDef = confValidation & confDefault in
let myConf = {
    gcc = "gcc -02";
} in
let result = myConf & confValAndDef in
result.specialFlag
