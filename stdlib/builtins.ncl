{
  builtins = {
    isNum : Dyn -> Bool = fun x => %isNum% x,
    isBool : Dyn -> Bool = fun x => %isBool% x,
    isStr : Dyn -> Bool = fun x => %isStr% x,
    isFun : Dyn -> Bool = fun x => %isFun% x,
    isList : Dyn -> Bool = fun x => %isList% x,
    isRecord : Dyn -> Bool = fun x => %isRecord% x,
    typeOf : Dyn -> <
      TypeNum,
      TypeBool,
      TypeStr,
      TypeFun,
      TypeList,
      TypeRecord,
      Other> = fun x =>
      if %isNum% x then `TypeNum
      else if %isBool% x then `TypeBool
      else if %isStr% x then `TypeStr
      else if %isFun% x then `TypeFun
      else if %isList% x then `TypeList
      else if %isRecord% x then `TypeRecord
      else `Other,

    seq : forall a. Dyn -> a -> a = fun x y => %seq% x y,
    deepSeq : forall a. Dyn -> a -> a = fun x y => %deepSeq% x y,

    id : forall a. a -> a = fun x => x,

    hash : <Md5, Sha1, Sha256, Sha512> -> Str -> Str =
      fun type s => %hash% type s,

    serialize : <Json, Toml, Yaml> -> Dyn -> Str = fun format x =>
      %serialize% format (%deepSeq% x x),

    deserialize : <Json, Toml, Yaml> -> Str -> Dyn = fun format x =>
      %deserialize% format x,
  }
}
