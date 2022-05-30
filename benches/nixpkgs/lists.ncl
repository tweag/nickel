# General list operations.

{
  singleton: forall a. a -> (Array a)
  | doc m%"
      Create a list consisting of a single element.  `singleton x` is
      sometimes more convenient with respect to indentation than `[x]`
      when x spans multiple lines.

      Example:
        singleton "foo"
        >> [ "foo" ]
    "%m
    = fun x => [x],

  forEach: forall a b. (Array a) -> (a -> b) -> (Array b)
  | doc m%"
      Apply the function to each element in the list. Same as `map`, but arguments
      flipped.

      Example:
        forEach [ 1, 2 ] (fun x =>
          toString x
        )
        >> [ "1" "2" ]
    "%m
  = fun xs f => array.map f xs,

  foldr: forall a b. (a -> b -> b) -> b -> (Array a) -> b
  | doc m%"
      “right fold” a binary function `op` between successive elements of
     `list` with `nul' as the starting value, i.e.,
     `foldr op nul [x_1 x_2 ... x_n] == op x_1 (op x_2 ... (op x_n nul))`.

     Example:
       concat = foldr (fun a b => a @ b) "z"
       concat [ "a" "b" "c" ]
       => "abcz"
       # different types
       strange = foldr (fun int str => toString (int + 1) @@ str) "a"
       strange [ 1 2 3 4 ]
       => "2345a"
  "%m
  = fun op nul list =>
    let len = array.length list in
    let rec fold_ = fun n =>
        if n == len
        then nul
        else op (array.elem_at n list) (fold_ (n + 1))
    in fold_ 0,

  fold: forall a b. (a -> b -> b) -> b -> (Array a) -> b
  | doc m%"
      `fold` is an alias of `foldr` for historic reasons "%m
  # FIXME(Profpatsch): deprecate?
  = foldr,


  foldl: forall a b. (b -> a -> b) -> b -> (Array a) -> b
  | doc m%"
      “left fold”, like `foldr`, but from the left:
     `foldl op nul [x_1 x_2 ... x_n] == op (... (op (op nul x_1) x_2) ... x_n)`.

     Example:
       lconcat = foldl (fun a b => a @ b) "z"
       lconcat [ "a" "b" "c" ]
       >> "zabc"
       # different types
       lstrange = foldl (str: int: str + toString (int + 1)) "a"
       lstrange [ 1 2 3 4 ]
       => "a2345"
  "%m
  = fun op nul list =>
    let foldl_ = fun n =>
        if n == -1
        then nul
        else op (foldl_ (n - 1)) (array.elem_at n list)
    in foldl_ (array.length list - 1),

  foldl_: forall a b. (b -> a -> b) -> b -> (Array a) -> b
  | doc m%"
      Strict version of `foldl`.

     The difference is that evaluation is forced upon access. Usually used
     with small whole results (in contrast with lazily-generated list or large
     lists where only a part is consumed.)
  "%m
  = fun op nul list => %deep_seq% (foldl op nul list),

  imap0: forall a b. (Num -> a -> b) -> (Array a) -> (Array b)
  | doc m%%"
      Map with index starting from 0

     Example:
       imap0 (fun i v => "%{v}-%{%to_string% i}") ["a", "b"]
       >> [ "a-0", "b-1" ]
  "%%m
  = fun f list => array.generate (fun n => f n (array.elem_at n list)) (array.length list),

  imap1: forall a b. (Num -> a -> b) -> (Array a) -> (Array b)
  | doc m%%"
      Map with index starting from 1

     Example:
       imap1 (fun i v => "%{v}-%{toString i}") ["a", "b"]
       >> [ "a-1", "b-2" ]
  "%%m
  = fun f list => array.generate (fun n => f (n + 1) (array.elem_at n list)) (array.length list),

  concatMap: forall a b. (a -> (Array b)) -> (Array a) -> (Array b)
  | doc m%"
      Map and concatenate the result.

     Example:
       concatMap (fun x => [x] @ ["z"]) ["a", "b"]
       >> [ "a", "z", "b", "z" ]
  "%m
  = (fun f list => array.flatten (array.map f list)),
}
