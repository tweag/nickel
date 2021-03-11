{
  records = {
    map : forall a b. (Str -> a -> b) -> {_: a} -> {_: b} = fun f r => %recordMap% r f,

    fieldsOf | { | Dyn} -> List Str = fun r => %fieldsOf% r,

    valuesOf | { | Dyn} -> List  = fun r => %valuesOf% r,

    hasField : Str -> Dyn -> Bool = fun r field => %hasField% r field,
  }
}
