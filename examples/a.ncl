let func: Num -> Num -> Num -> Num = fun x y z => 
    x +
    y +
    z 
in 

let EqualsTo = fun referenceValue label value =>
  if referenceValue == value then
    value 
  else
    contracts.blame label in

let IsTrue = EqualsTo true in

let Checked = {
  b.string | #IsTrue
} in

let config: {
  b: {
        string :  Bool,
  },
  sum : Num,
  jump: Bool,
} = {
  b."string" = true,
  jump = b.string,
  sum = func 1 1 1
}  in
let rec : { a: Str, b: Str }= {

  a ="String",
  b = a,

}

in


(config | #Checked).b.string
