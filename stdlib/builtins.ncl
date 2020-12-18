{
  builtins = {
    isNum : Dyn -> Bool = fun x => %isNum% x;
    isBool : Dyn -> Bool = fun x => %isBool% x;
    isStr : Dyn -> Bool = fun x => %isStr% x;
    isFun : Dyn -> Bool = fun x => %isFun% x;
    isList : Dyn -> Bool = fun x => %isList% x;
    isRecord : Dyn -> Bool = fun x => %isRecord% x;

    seq : forall a. Dyn -> a -> a = fun x y => %seq% x y;
    deepSeq : forall a. Dyn -> a -> a = fun x y => %deepSeq% x y;
  }
}
