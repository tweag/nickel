{
  records = {
    map : forall a b. (Str -> a -> b) -> {_: a} -> {_: b} = fun f r => %mapRec% f r;

    fieldsOf : Dyn -> List = fun r => %fieldsOf% r;

    hasField : Str -> Dyn -> Bool = fun r field => %hasField% r field;
  }
}
