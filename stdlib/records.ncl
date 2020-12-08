{
  records = {
    map : forall a b. (Str -> a -> b) -> {_: a} -> {_: b} = fun f r => %recordMap% f r;

    // TODO: change Dyn to { | Dyn} once the PR introducing open contracts lands
    fieldsOf : Dyn -> List = fun r => %fieldsOf% r;

    hasField : Str -> Dyn -> Bool = fun r field => %hasField% r field;
  }
}
