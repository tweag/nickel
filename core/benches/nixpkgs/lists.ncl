# General list operations.
{
  singleton
    : forall a. a -> (Array a)
    | doc m%"
      Create a list consisting of a single element.  `singleton x` is
      sometimes more convenient with respect to indentation than `[x]`
      when x spans multiple lines.

      Example:
        singleton "foo"
        >> [ "foo" ]
    "%
    = fun x => [x],

  forEach
    : forall a b. (Array a) -> (a -> b) -> (Array b)
    | doc m%"
      Apply the function to each element in the list. Same as `map`, but arguments
      flipped.

      Example:
        forEach [ 1, 2 ] (fun x =>
          toString x
        )
        >> [ "1" "2" ]
    "%
    = fun xs f => std.array.map f xs,

  foldr
    : forall a b. (a -> b -> b) -> b -> (Array a) -> b
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
  "%
    = fun op nul list =>
      let len = std.array.length list in
      let rec fold_ = fun n =>
        if n == len then
          nul
        else
          op (std.array.at n list) (fold_ (n + 1))
      in fold_ 0,

  fold
    : forall a b. (a -> b -> b) -> b -> (Array a) -> b
    | doc m%"
      `fold` is an alias of `foldr` for historic reasons "%
    # FIXME(Profpatsch): deprecate?
    = foldr,

  foldl
    : forall a b. (b -> a -> b) -> b -> (Array a) -> b
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
  "%
    = fun op nul list =>
      let rec foldl_ = fun n =>
        if n == -1 then
          nul
        else
          op (foldl_ (n - 1)) (std.array.at n list)
      in foldl_ (std.array.length list - 1),

  foldl_
    : forall a b. (b -> a -> b) -> b -> (Array a) -> b
    | doc m%"
      Strict version of `foldl`.

     The difference is that evaluation is forced upon access. Usually used
     with small whole results (in contrast with lazily-generated list or large
     lists where only a part is consumed.)
  "%
    = fun op nul list => std.array.fold_left op nul list,

  imap0
    : forall a b. (Number -> a -> b) -> (Array a) -> (Array b)
    | doc m%%"
      Map with index starting from 0

     Example:
       imap0 (fun i v => "%{v}-%{%to_string% i}") ["a", "b"]
       >> [ "a-0", "b-1" ]
  "%%
    = fun f list => std.array.generate (fun n => f n (std.array.at n list)) (std.array.length list),

  imap1
    : forall a b. (Number -> a -> b) -> (Array a) -> (Array b)
    | doc m%%"
      Map with index starting from 1

     Example:
       imap1 (fun i v => "%{v}-%{toString i}") ["a", "b"]
       >> [ "a-1", "b-2" ]
  "%%
    = fun f list => std.array.generate (fun n => f (n + 1) (std.array.at n list)) (std.array.length list),

  concatMap
    : forall a b. (a -> (Array b)) -> (Array a) -> (Array b)
    | doc m%"
      Map and concatenate the result.

     Example:
       concatMap (fun x => [x] @ ["z"]) ["a", "b"]
       >> [ "a", "z", "b", "z" ]
  "%
    = (fun f list => std.array.flatten (std.array.map f list)),

  flatten
    : Dyn -> Array Dyn
    | doc m%"
      Flatten the argument into a single list, that is, nested lists are
     spliced into the top-level lists.

     Example:
       flatten [1, [2, [3], 4], 5]
       >> [1, 2, 3, 4, 5]
       flatten 1
       >> [1]
  "%
    = fun x =>
      if std.is_array x then
        concatMap (fun y => flatten y) (x | Array Dyn)
      else
        [x],

  remove
    : forall a. a -> Array a -> Array a
    | doc m%"
      Remove elements equal to 'e' from a list.  Useful for buildInputs.

     Example:
       remove 3 [ 1, 3, 4, 3 ]
       >> [ 1, 4 ]
  "%
    = # Element to remove from the list
      fun e => std.array.filter ((!=) e),

  findSingle
    : forall a. (a -> Bool) -> a -> a -> Array a -> a
    | doc m%"
      Find the sole element in the list matching the specified
     predicate, returns `default` if no such element exists, or
     `multiple` if there are multiple matching elements.

     Example:
       findSingle (fun x => x == 3) "none" "multiple" [ 1, 3, 3 ]
       >> "multiple"
       findSingle (fun x => x == 3) "none" "multiple" [ 1, 3 ]
       >> 3
       findSingle (fun x => x == 3) "none" "multiple" [ 1, 9 ]
       >> "none"
  "%
    = fun
    # Predicate
    pred
    # Default value to return if element was not found.
    def
    # Default value to return if more than one element was found
    multiple
    # Input list
    list =>
      let found = std.array.filter pred list in
      let len = std.array.length found in
      if len == 0 then
        def
      else if len != 1 then
        multiple
      else
        std.array.first found,

  findFirst
    : forall a. (a -> Bool) -> a -> Array a -> a
    | doc m%"
      Find the first element in the list matching the specified
     predicate or return `default` if no such element exists.

     Example:
       findFirst (fun x => x > 3) 7 [ 1, 6, 4 ]
       >> 6
       findFirst (fun x => x > 9) 7 [ 1, 6, 4 ]
       >> 7
  "%
    = fun
    # Predicate
    pred
    # Default value to return
    def
    # Input list
    list =>
      let found = std.array.filter pred list in
      if found == [] then def else std.array.first found,

  any
    : forall a. (a -> Bool) -> Array a -> Bool
    | doc m%"
      Return true if function `pred` returns true for at least one
     element of `list`.

     Example:
       any builtin.is_string [ 1, "a", { } ]
       >> true
       any builtin.is_string [ 1, { } ]
       >> false
  "%
    = fun pred => foldr (fun x y => if pred x then true else y) false,

  all
    : forall a. (a -> Bool) -> Array a -> Bool
    | doc m%"
      Return true if function `pred` returns true for all elements of
     `list`.

     Example:
       all (fun x => x < 3) [ 1, 2 ]
       >> true
       all (fun x => x < 3) [ 1, 2, 3 ]
       >> false
  "%
    = fun pred => foldr (fun x y => if pred x then y else false) true,

  count
    : forall a. (a -> Bool) -> Array a -> Number
    | doc m%"
      Count how many elements of `list` match the supplied predicate
     function.

     Example:
       count (fun x => x == 3) [ 3, 2, 3, 4, 6 ]
       >> 2
  "%
    = fun
    # Predicate
    pred => foldl_ (fun c x => if pred x then c + 1 else c) 0,

  optional'
    : forall a. Bool -> a -> Array a
    | doc m%"
      Return a singleton list or an empty list, depending on a boolean
     value.  Useful when building lists with optional elements
     (e.g. `++ optional' (system == "i686-linux") firefox').

     Example:
       optional' true "foo"
       >> [ "foo" ]
       optional' false "foo"
       >> [ ]
  "%
    = fun cond elem => if cond then [elem] else [],

  optionals
    : forall a. Bool -> Array a -> Array a
    | doc m%"
      Return a list or an empty list, depending on a boolean value.

     Example:
       optionals true [ 2, 3 ]
       >> [ 2, 3 ]
       optionals false [ 2, 3 ]
       >> [ ]
  "%
    = fun
    # Condition
    cond
    # List to return if condition is true
    elems => if cond then elems else [],

  toList
    : Dyn -> Array Dyn
    | doc m%"
      If argument is a list, return it, else, wrap it in a singleton
     list.  If you're using this, you should almost certainly
     reconsider if there isn't a more "well-typed" approach.

     Example:
       toList [ 1, 2 ]
       >> [ 1, 2 ]
       toList "hi"
       >> [ "hi "]
  "%
    = fun x => if std.is_array x then (x | Array Dyn) else [x],

  range
    : Number -> Number -> Array Number
    | doc m%"
      Return a list of integers from `first' up to and including `last'.

     Example:
       range 2 4
       >> [ 2, 3, 4 ]
       range 3 2
       >> [ ]
  "%
    = fun
    # First integer in the range
    first
    # Last integer in the range
    last =>
      if first > last then
        []
      else
        std.array.generate (fun n => first + n) (last - first + 1),

  partition
    : forall a. (a -> Bool) -> Array a -> { right : Array a, wrong : Array a }
    | doc m%"
      Splits the elements of a list in two lists, `right` and
     `wrong`, depending on the evaluation of a predicate.

     Example:
       partition (fun x => x > 2) [ 5, 1, 2, 3, 4 ]
       >> { right = [ 5, 3, 4 ], wrong = [ 1, 2 ] }
  "%
    = fun pred =>
      foldr
        (
          fun h t =>
            if pred h then
              { right = [h] @ t.right, wrong = t.wrong }
            else
              { right = t.right, wrong = [h] @ t.wrong }
        )
        { right = [], wrong = [] },

  # can not be staticaly checked (see issue #423)
  groupBy
    | forall a. (a -> String) -> Array a -> { _ : Array a }
    | doc m%"
     Splits the elements of a list into many lists, using the return value of a predicate.
     Predicate should return a string which becomes keys of attrset `groupBy' returns.

     Example:
       groupBy (fun x => boolToString (x > 2)) [ 5, 1, 2, 3, 4 ]
       >> { true = [ 5, 3, 4 ], false = [ 1, 2 ] }
       groupBy (fun x => x.name) [ {name = "icewm", script = "icewm &"},
                             {name = "xfce",  script = "xfce4-session &"},
                             {name = "icewm", script = "icewmbg &"},
                             {name = "mate",  script = "gnome-session &"},
                           ]
       >> { icewm = [ { name = "icewm", script = "icewm &" },
                      { name = "icewm", script = "icewmbg &" } ],
            mate  = [ { name = "mate",  script = "gnome-session &" } ],
            xfce  = [ { name = "xfce",  script = "xfce4-session &" } ],
          }
  "%
    = fun pred =>
      foldl_
        (
          fun r e =>
            let key = pred e in
            { "%{key}" = (r."%{key}" @ [e]) } & (std.record.remove key r)
        )
        {},

  groupBy_
    : forall a b. (b -> a -> b) -> b -> (a -> String) -> Array a -> { _ : b }
    | doc m%"
     as `groupBy` and allows to customise the combining function and initial value

     Example:
       groupBy_ builtins.add 0 (fun x => boolToString (x > 2)) [ 5, 1, 2, 3, 4 ]
       >> { true = 12, false = 3 }
  "%
    = fun op nul pred lst => std.record.map (fun name value => foldl op nul value) (groupBy pred lst),

  zipListsWith
    : forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c
    | doc m%"
      Merges two lists of the same size together. If the sizes aren't the same
     the merging stops at the shortest. How both lists are merged is defined
     by the first argument.

     Example:
       zipListsWith (fun a b => a @@ b) ["h", "l"] ["e", "o"]
       >> ["he", "lo"]
  "%
    = fun
    # Function to zip elements of both lists
    f
    # First list
    fst
    # Second list
    snd => std.array.generate (fun n => f (std.array.at n fst) (std.array.at n snd)) (std.number.min (std.array.length fst) (std.array.length snd)),

  zipLists
    : forall a b. Array a -> Array b -> Array { fst : a, snd : b }
    | doc m%"
      Merges two lists of the same size together. If the sizes aren't the same
     the merging stops at the shortest.

     Example:
       zipLists [ 1, 2 ] [ "a", "b" ]
       >> [ { fst = 1, snd = "a" }, { fst = 2, snd = "b" } ]
  "%
    = zipListsWith (fun fst snd => { fst = fst, snd = snd }),

  reverseList
    : forall a. Array a -> Array a
    | doc m%"
      Reverse the order of the elements of a list.

     Example:
       reverseList [ "b", "o", "j" ]
       >> [ "j", "o", "b" ]
  "%
    = fun xs =>
      let l = std.array.length xs in
      std.array.generate (fun n => std.array.at (l - n - 1) xs) l,

  #TODO: is there a way to type statically?
  listDfs
    | forall a. Bool -> (a -> a -> Bool) -> Array a -> { visited : Array a, rest : Array a; Dyn }
    | doc m%"
      Depth-First Search (DFS) for lists `list != []`.

     `before a b == true` means that `b` depends on `a` (there's an
     edge from `b` to `a`).

     Example:
         listDfs true hasPrefix [ "/home/user" "other" "/" "/home" ]
           == { minimal = "/",                  # minimal element
                visited = [ "/home/user" ],     # seen elements (in reverse order)
                rest    = [ "/home" "other" ],  # everything else
              }

         listDfs true hasPrefix [ "/home/user" "other" "/" "/home" "/" ]
           == { cycle   = "/",                  # cycle encountered at this element
                loops   = [ "/" ],              # and continues to these elements
                visited = [ "/" "/home/user" ], # elements leading to the cycle (in reverse order)
                rest    = [ "/home" "other" ],  # everything else

   "%
    = fun stopOnCycles before list =>
      let rec dfs_ = fun us visited_ rest_ =>
        let c = std.array.filter (fun x => before x us) visited_ in
        let b = partition (fun x => before x us) rest_ in
        if stopOnCycles && (std.array.length c > 0) then
          { cycle = us, loops = c, visited = visited_, rest = rest_ }
        else if std.array.length b.right == 0 then
          # nothing is before us
          { minimal = us, visited = visited_, rest = rest_ }
        else
          # grab the first one before us and continue
          (
            dfs_
              (std.array.first b.right)
              ([us] @ visited_)
              (std.array.drop_first b.right @ b.wrong)
          )
      in
      dfs_ (std.array.first list) [] (std.array.drop_first list),

  toposort
    : forall a. (a -> a -> Bool) -> Array a -> { _ : Array Dyn }
    | doc m%"
      Sort a list based on a partial ordering using DFS. This
     implementation is O(N^2), if your ordering is linear, use `sort`
     instead.

     `before a b == true` means that `b` should be after `a`
     in the result.

     Example:

         toposort hasPrefix [ "/home/user" "other" "/" "/home" ]
           == { result = [ "/", "/home", "/home/user", "other" ], }

         toposort hasPrefix [ "/home/user", "other", "/", "/home", "/" ]
           == { cycle = [ "/home/user", "/", "/" ], # path leading to a cycle
                loops = [ "/" ], }                # loops back to these elements

         toposort hasPrefix [ "other", "/home/user", "/home", "/" ]
           == { result = [ "other", "/", "/home", "/home/user" ], }

         toposort (a: b: a < b) [ 3, 2, 1 ] == { result = [ 1, 2, 3 ], }
  "%
    = fun before list =>
      let dfsthis = listDfs true before list in
      let toporest = toposort before (dfsthis.visited @ dfsthis.rest) in

      # The repeated `{_ : Array Dyn}` annotations are verbose, and due to
      # the handling of dictionary types being cumbersome with the current
      # typechecker.
      #
      # RFC004 is supposed to fix those kind of use-cases by introducing a limited
      # form of subtyping.
      if std.array.length list < 2 then
        # finish
        { result | Array Dyn = list, } : { _ : Array Dyn }
      else if std.record.has_field "cycle" (dfsthis | { _ : Dyn }) then
        # there's a cycle, starting from the current vertex, return it
        {
          cycle = reverseList ([(dfsthis | { cycle : Dyn }).cycle] @ dfsthis.visited | Array Dyn),
          loops = (dfsthis | { loops : Array Dyn }).loops,
        } : { _ : Array Dyn }
      else if std.record.has_field "cycle" toporest then
        # there's a cycle somewhere else in the graph, return it
        toporest
        # Slow, but short. Can be made a bit faster with an explicit stack.
      else
        # there are no cycles
        {
          result =
            [(dfsthis | { minimal : Dyn }).minimal]
            @ (toporest | { result : Array Dyn }).result
        } : { _ : Array Dyn },

  #TODO: can it be statically typed?
  sort
    | forall a. (a -> a -> Bool) -> Array a -> Array a
    | doc m%"
      Sort a list based on a comparator function which compares two
     elements and returns true if the first argument is strictly below
     the second argument.  The returned list is sorted in an increasing
     order.  The implementation does a quick-sort.

     Example:
       sort (a: b: a < b) [ 5, 3, 7 ]
       >> [ 3, 5, 7 ]
  "%
    = fun strictLess list =>
      let len = std.array.length list in
      let first = std.array.first list in
      let rec pivot_ = (
        fun n acc @ { left = left_, right = right_ } =>
          let el = std.array.at n list in
          let next = pivot_ (n + 1) in
          if n == len then
            acc
          else if strictLess first el then
            next { left = left_, right = [el] @ right_ }
          else
            next { left = [el] @ left_, right = right_ }
      )
      in
      let pivot = pivot_ 1 { left = [], right = [] } in
      if len < 2 then
        list
      else
        (sort strictLess pivot.left) @ [first] @ (sort strictLess pivot.right),

  compareLists
    : forall a. (a -> a -> Number) -> Array a -> Array a -> Number
    | doc m%"
      Compare two lists element-by-element.

     Example:
       compareLists compare [] []
       >> 0
       compareLists compare [] [ "a" ]
       >> -1
       compareLists compare [ "a" ] []
       >> 1
       compareLists compare [ "a", "b" ] [ "a", "c" ]
       >> 1
  "%
    = fun cmp a b =>
      if a == [] then
        if b == [] then
          0
        else
          -1
      else if b == [] then
        1
      else
        let rel = cmp (std.array.first a) (std.array.first b) in
        if rel == 0 then
          compareLists cmp (std.array.drop_first a) (std.array.drop_first b)
        else
          rel,

  # naturalSort: Array String -> Array String
  # | doc m%"
  #     Sort list using "Natural sorting".
  #    Numeric portions of strings are sorted in numeric order.

  #    Example:
  #      naturalSort ["disk11", "disk8", "disk100", "disk9"]
  #      >> ["disk8", "disk9", "disk11", "disk100"]
  #      naturalSort ["10.46.133.149", "10.5.16.62", "10.54.16.25"]
  #      >> ["10.5.16.62" "10.46.133.149" "10.54.16.25"]
  #      naturalSort ["v0.2", "v0.15", "v0.0.9"]
  #      >> [ "v0.0.9", "v0.2", "v0.15" ]
  # "%
  # # TODO: broken. how to implement it in nickel?
  # = fun lst =>
  #   let vectorise = fun s => std.array.map (fun x => if builtin.is_array x then (std.array.first x) | Number else x | Number) (std.string.split "(0|[1-9][0-9]*)" s) | Array Dyn
  #   in
  #     let prepared = std.array.map (fun x => [ (vectorise x), x ]) lst in # remember vectorised version for O(n) regex splits
  #     let less = fun a b => (compareLists compare (std.array.first a) (std.array.first b)) < 0 in
  #     std.array.map (fun x => std.array.at 1 x) (sort less prepared),

  take
    : forall a. Number -> Array a -> Array a
    | doc m%"
      Return the first (at most) N elements of a list.


     Example:
       take 2 [ "a", "b", "c", "d" ]
       >> [ "a", "b" ]
       take 2 [ ]
       >> [ ]
  "%
    = fun
    # Number of elements to take
    count => sublist 0 count,

  drop
    : forall a. Number -> Array a -> Array a
    | doc m%"
      Remove the first (at most) N elements of a list.


     Example:
       drop 2 [ "a", "b", "c", "d" ]
       >> [ "c", "d" ]
       drop 2 [ ]
       >> [ ]
  "%
    = fun
    # Number of elements to drop
    count
    # Input list
    lst => sublist count (std.array.length lst) lst,

  sublist
    : forall a. Number -> Number -> Array a -> Array a
    | doc m%"
      Return a list consisting of at most `count` elements of `list`,
     starting at index `start`.


     Example:
       sublist 1 3 [ "a", "b", "c", "d", "e" ]
       >> [ "b", "c", "d" ]
       sublist 1 3 [ ]
       >> [ ]
  "%
    = fun
    # Index at which to start the sublist
    start
    # Number of elements to take
    count
    # Input list
    lst =>
      let len = std.array.length lst in
      std.array.generate
        (fun n => std.array.at (n + start) lst)
        (
          if start >= len then
            0
          else if start + count > len then
            len - start
          else
            count
        ),

  last
    : forall a. Array a -> a
    | doc m%"
      Return the last element of a list.

     This function throws an error if the list is empty.


     Example:
       last [ 1, 2, 3 ]
       >> 3
  "%
    = fun lst =>
      #assert lib.assertMsg (lst != []) "lists.last: list must not be empty!",
      std.array.at (std.array.length lst - 1) lst,

  init
    : forall a. Array a -> Array a
    | doc m%"
      Return all elements but the last.

     This function throws an error if the list is empty.

     Example:
       init [ 1, 2, 3 ]
       >> [ 1, 2 ]
  "%
    = fun lst =>
      #assert lib.assertMsg (lst != []) "lists.init: list must not be empty!",
      take (std.array.length lst - 1) lst,

  unique
    : Array Dyn -> Array Dyn
    | doc m%"
      Remove duplicate elements from the list. O(n^2) complexity.

     Example:
       unique [ 3, 2, 3, 4 ]
       >> [ 3, 2, 4 ]
   "%
    = foldl_ (fun acc e => if std.array.elem e acc then acc else acc @ [e]) [],

  intersectLists
    : Array Dyn -> Array Dyn -> Array Dyn
    | doc m%"
      Intersects list 'e' and another list. O(nm) complexity.

     Example:
       intersectLists [ 1, 2, 3 ] [ 6, 3, 2 ]
       >> [ 3, 2 ]
  "%
    = fun e => std.array.filter (fun x => std.array.elem x e),

  subtractLists
    : Array Dyn -> Array Dyn -> Array Dyn
    | doc m%"
      Subtracts list 'e' from another list. O(nm) complexity.

     Example:
       subtractLists [ 3, 2 ] [ 1, 2, 3, 4, 5, 3 ]
       >> [ 1, 4, 5 ]
  "%
    = fun e => std.array.filter (fun x => !(std.array.elem x e)),

  mutuallyExclusive
    : Array Dyn -> Array Dyn -> Bool
    | doc m%"
      Test if two lists have no common element.
     It should be slightly more efficient than (intersectLists a b == [])
  "%
    = fun a b => std.array.length a == 0 || !(any (fun x => std.array.elem x a) b),
}
