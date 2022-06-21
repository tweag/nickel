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
    let rec foldl_ = fun n =>
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
  = fun op nul list => array.foldl op nul list,

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

  flatten: Dyn -> Array Dyn
  | doc m%"
      Flatten the argument into a single list, that is, nested lists are
     spliced into the top-level lists.

     Example:
       flatten [1, [2, [3], 4], 5]
       >> [1, 2, 3, 4, 5]
       flatten 1
       >> [1]
  "%m
  = fun x =>
    if builtin.is_array x
    then concatMap (fun y => flatten y) (x | Array Dyn)
    else [x],

  remove: forall a. a -> Array a -> Array a
  | doc m%"
      Remove elements equal to 'e' from a list.  Useful for buildInputs.

     Example:
       remove 3 [ 1, 3, 4, 3 ]
       >> [ 1, 4 ]
  "%m
  = # Element to remove from the list
    fun e => array.filter ((!=) e),

  findSingle: forall a. (a -> Bool) -> a -> a -> Array a -> a
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
  "%m
  = fun
    # Predicate
    pred
    # Default value to return if element was not found.
    def
    # Default value to return if more than one element was found
    multiple
    # Input list
    list
    => let found = array.filter pred list in
       let len = array.length found in
       if len == 0 then def
       else if len != 1 then multiple
       else array.head found,

  findFirst: forall a. (a -> Bool) -> a -> Array a -> a
  | doc m%"
      Find the first element in the list matching the specified
     predicate or return `default` if no such element exists.

     Example:
       findFirst (fun x => x > 3) 7 [ 1, 6, 4 ]
       >> 6
       findFirst (fun x => x > 9) 7 [ 1, 6, 4 ]
       >> 7
  "%m
  = fun
    # Predicate
    pred
    # Default value to return
    def
    # Input list
    list
    => let found = array.filter pred list in
       if found == [] then def else array.head found,

  any: forall a. (a -> Bool) -> Array a -> Bool
  | doc m%"
      Return true if function `pred` returns true for at least one
     element of `list`.

     Example:
       any builtin.is_str [ 1, "a", { } ]
       >> true
       any builtin.is_str [ 1, { } ]
       >> false
  "%m
  = fun pred => foldr (fun x y => if pred x then true else y) false,

  all: forall a. (a -> Bool) -> Array a -> Bool
  | doc m%"
      Return true if function `pred` returns true for all elements of
     `list`.

     Example:
       all (fun x => x < 3) [ 1, 2 ]
       >> true
       all (fun x => x < 3) [ 1, 2, 3 ]
       >> false
  "%m
  = fun pred => foldr (fun x y => if pred x then y else false) true,

  count: forall a. (a -> Bool) -> Array a -> Num
  | doc m%"
      Count how many elements of `list` match the supplied predicate
     function.

     Example:
       count (fun x => x == 3) [ 3, 2, 3, 4, 6 ]
       >> 2
  "%m
  = fun
    # Predicate
    pred => foldl_ (fun c x => if pred x then c + 1 else c) 0,

  optional: forall a. Bool -> a -> Array a
  | doc m%"
      Return a singleton list or an empty list, depending on a boolean
     value.  Useful when building lists with optional elements
     (e.g. `++ optional (system == "i686-linux") firefox').

     Example:
       optional true "foo"
       >> [ "foo" ]
       optional false "foo"
       >> [ ]
  "%m
  = fun cond elem => if cond then [elem] else [],

  optionals: forall a. Bool -> Array a -> Array a
  | doc m%"
      Return a list or an empty list, depending on a boolean value.

     Example:
       optionals true [ 2, 3 ]
       >> [ 2, 3 ]
       optionals false [ 2, 3 ]
       >> [ ]
  "%m
  = fun
    # Condition
    cond
    # List to return if condition is true
    elems
    => if cond then elems else [],


  toList: Dyn -> Array Dyn
  | doc m%"
      If argument is a list, return it, else, wrap it in a singleton
     list.  If you're using this, you should almost certainly
     reconsider if there isn't a more "well-typed" approach.

     Example:
       toList [ 1, 2 ]
       >> [ 1, 2 ]
       toList "hi"
       >> [ "hi "]
  "%m
  = fun x => if builtin.is_array x then (x | Array Dyn) else [x],

  range: Num -> Num -> Array Num
  | doc m%"
      Return a list of integers from `first' up to and including `last'.

     Example:
       range 2 4
       >> [ 2, 3, 4 ]
       range 3 2
       >> [ ]
  "%m
  = fun
    # First integer in the range
    first
    # Last integer in the range
    last
    => if first > last then
      []
    else
      array.generate (fun n => first + n) (last - first + 1),

  partition: forall a. (a -> Bool) -> Array a -> { right : Array a, wrong : Array a }
  | doc m%"
      Splits the elements of a list in two lists, `right` and
     `wrong`, depending on the evaluation of a predicate.

     Example:
       partition (fun x => x > 2) [ 5, 1, 2, 3, 4 ]
       >> { right = [ 5, 3, 4 ], wrong = [ 1, 2 ] }
  "%m
  = fun pred =>
    foldr (fun h t =>
      if pred h
      then { right = [h] @ t.right, wrong = t.wrong }
      else { right = t.right, wrong = [h] @ t.wrong }
    ) { right = [], wrong = [] },

  # can not be staticaly checked (see issue #423)
  groupBy | forall a. (a -> Str) -> Array a -> { _: Array a}
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
  "%m
  = fun pred => foldl_ (fun r e =>
       let key = pred e in
         { "%{key}" = (r."%{key}" @ [e]) } & (record.remove key r)
    ) {},

  groupBy_ : forall a b. (b -> a -> b) ->  b -> (a -> Str) -> Array a -> { _: b}
  | doc m%"
     as `groupBy` and allows to customise the combining function and initial value

     Example:
       groupBy_ builtins.add 0 (fun x => boolToString (x > 2)) [ 5, 1, 2, 3, 4 ]
       >> { true = 12, false = 3 }
  "%m
  = fun op nul pred lst => record.map (fun name value => foldl op nul value) (groupBy pred lst),

  zipListsWith: forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c
  | doc m%"
      Merges two lists of the same size together. If the sizes aren't the same
     the merging stops at the shortest. How both lists are merged is defined
     by the first argument.

     Example:
       zipListsWith (fun a b => a @@ b) ["h", "l"] ["e", "o"]
       >> ["he", "lo"]
  "%m
  = fun
    # Function to zip elements of both lists
    f
    # First list
    fst
    # Second list
    snd
    => array.generate (fun n => f (array.elem_at n fst) (array.elem_at n snd)) (num.min (array.length fst) (array.length snd)),

  zipLists: forall a b. Array a -> Array b -> Array { fst: a, snd: b}
  | doc m%"
      Merges two lists of the same size together. If the sizes aren't the same
     the merging stops at the shortest.

     Example:
       zipLists [ 1, 2 ] [ "a", "b" ]
       >> [ { fst = 1, snd = "a" }, { fst = 2, snd = "b" } ]
  "%m
  = zipListsWith (fun fst snd => { fst = fst, snd = snd }),

  reverseList: forall a. Array a -> Array a
  | doc m%"
      Reverse the order of the elements of a list.

     Example:
       reverseList [ "b", "o", "j" ]
       >> [ "j", "o", "b" ]
  "%m
  = fun xs =>
    let l = array.length xs in
    array.generate (fun n => array.elem_at (l - n - 1) xs) l,

  #TODO: is there a way to type staticaly?
  listDfs |  forall a. Bool -> (a -> a -> Bool) -> Array a -> {visited: Array a, rest: Array a ; Dyn}
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

   "%m
  = fun stopOnCycles before list =>
    let rec dfs_ = fun us visited_ rest_ =>
        let c = array.filter (fun x => before x us) visited_ in
        let b = partition (fun x => before x us) rest_ in
        if stopOnCycles && (array.length c > 0)
        then { cycle = us, loops = c, visited = visited_, rest = rest_ }
        else if array.length b.right == 0
        then # nothing is before us
          { minimal = us, visited = visited_, rest = rest_ }
        else # grab the first one before us and continue
          (dfs_ (array.head b.right)
                ([ us ] @ visited_)
                (array.tail b.right @ b.wrong)
    ) in
    dfs_ (array.head list) [] (array.tail list),

  toposort: forall a. (a -> a -> Bool) -> Array a -> {_: Array Dyn}
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
  "%m
  = fun before list =>
    let dfsthis = listDfs true before list in
    let toporest = toposort before (dfsthis.visited @ dfsthis.rest) in
    if array.length list < 2
    then # finish
      { result| Array Dyn =  list, }
    else if record.has_field "cycle" (dfsthis | {_: Dyn})
    then # there's a cycle, starting from the current vertex, return it
      { cycle = reverseList ([ (dfsthis | {cycle: Dyn}).cycle ] @ dfsthis.visited | Array Dyn),
        loops = (dfsthis | {loops: Array Dyn}).loops, }
    else if record.has_field "cycle" toporest
    then # there's a cycle somewhere else in the graph, return it
      toporest
    # Slow, but short. Can be made a bit faster with an explicit stack.
    else # there are no cycles
      { result = [ (dfsthis| {minimal:Dyn}).minimal ] @ (toporest | {result: Array Dyn}).result, },

  #TODO: can it be staticaly typed?
  sort | forall a. (a -> a -> Bool) -> Array a -> Array a
  | doc m%"
      Sort a list based on a comparator function which compares two
     elements and returns true if the first argument is strictly below
     the second argument.  The returned list is sorted in an increasing
     order.  The implementation does a quick-sort.

     Example:
       sort (a: b: a < b) [ 5, 3, 7 ]
       >> [ 3, 5, 7 ]
  "%m
  = fun strictLess list =>
    let len = array.length list in
    let first = array.head list in
    let rec pivot_ = (fun n acc@{ left=left_, right=right_ } =>
      let el = array.elem_at n list in
      let next = pivot_ (n + 1) in
      if n == len
      then acc
      else if strictLess first el
      then next { left = left_, right = [ el ] @ right_ }
      else next { left = [ el ] @ left_, right = right_ }
    ) in
    let pivot = pivot_ 1 { left = [], right = [] } in
    if len < 2 then list
    else (sort strictLess pivot.left) @ [ first ] @ (sort strictLess pivot.right),

  compareLists: forall a. (a -> a -> Num) -> Array a -> Array a -> Num
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
  "%m
  = fun cmp a b =>
    if a == []
    then if b == []
         then 0
         else -1
    else if b == []
         then 1
         else let rel = cmp (array.head a) (array.head b) in
              if rel == 0
              then compareLists cmp (array.tail a) (array.tail b)
              else rel,

# naturalSort: Array Str -> Array Str
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
# "%m
# # TODO: broken. how to implement it in nickel?
# = fun lst =>
#   let vectorise = fun s => array.map (fun x => if builtin.is_array x then (array.head x) | Num else x | Num) (string.split "(0|[1-9][0-9]*)" s) | Array Dyn
#   in
#     let prepared = array.map (fun x => [ (vectorise x), x ]) lst in # remember vectorised version for O(n) regex splits
#     let less = fun a b => (compareLists compare (array.head a) (array.head b)) < 0 in
#     array.map (fun x => array.elem_at 1 x) (sort less prepared),

  take: forall a. Num -> Array a -> Array a
  | doc m%"
      Return the first (at most) N elements of a list.


     Example:
       take 2 [ "a", "b", "c", "d" ]
       >> [ "a", "b" ]
       take 2 [ ]
       >> [ ]
  "%m
  = fun 
    # Number of elements to take
    count => sublist 0 count,

  drop: forall a. Num -> Array a -> Array a
  | doc m%"
      Remove the first (at most) N elements of a list.


     Example:
       drop 2 [ "a", "b", "c", "d" ]
       >> [ "c", "d" ]
       drop 2 [ ]
       >> [ ]
  "%m
  = fun
    # Number of elements to drop
    count
    # Input list
    lst => sublist count (array.length lst) lst,

  sublist: forall a. Num -> Num -> Array a -> Array a
  | doc m%"
      Return a list consisting of at most `count` elements of `list`,
     starting at index `start`.


     Example:
       sublist 1 3 [ "a", "b", "c", "d", "e" ]
       >> [ "b", "c", "d" ]
       sublist 1 3 [ ]
       >> [ ]
  "%m
  = fun
    # Index at which to start the sublist
    start
    # Number of elements to take
    count
    # Input list
    lst =>
    let len = array.length lst in
    array.generate
      (fun n => array.elem_at (n + start) lst)
      (if start >= len then 0
       else if start + count > len then len - start
       else count),

  last: forall a. Array a -> a
  | doc m%"
      Return the last element of a list.

     This function throws an error if the list is empty.


     Example:
       last [ 1, 2, 3 ]
       >> 3
  "%m
  = fun lst =>
    #assert lib.assertMsg (lst != []) "lists.last: list must not be empty!",
    array.elem_at (array.length lst - 1) lst,

  init: forall a. Array a -> Array a
  | doc m%"
      Return all elements but the last.

     This function throws an error if the list is empty.

     Example:
       init [ 1, 2, 3 ]
       >> [ 1, 2 ]
  "%m
  = fun lst =>
    #assert lib.assertMsg (lst != []) "lists.init: list must not be empty!",
    take (array.length lst - 1) lst,

  unique: Array Dyn -> Array Dyn
  | doc m%"
      Remove duplicate elements from the list. O(n^2) complexity.

     Example:
       unique [ 3, 2, 3, 4 ]
       >> [ 3, 2, 4 ]
   "%m
  = foldl_ (fun acc e => if array.elem e acc then acc else acc @ [ e ]) [],

  intersectLists: Array Dyn -> Array Dyn -> Array Dyn
  | doc m%"
      Intersects list 'e' and another list. O(nm) complexity.

     Example:
       intersectLists [ 1, 2, 3 ] [ 6, 3, 2 ]
       >> [ 3, 2 ]
  "%m
  = fun e => array.filter (fun x => array.elem x e),

  subtractLists: Array Dyn -> Array Dyn -> Array Dyn
  | doc m%"
      Subtracts list 'e' from another list. O(nm) complexity.

     Example:
       subtractLists [ 3, 2 ] [ 1, 2, 3, 4, 5, 3 ]
       >> [ 1, 4, 5 ]
  "%m
  = fun e => array.filter (fun x => !(array.elem x e)),

  mutuallyExclusive: Array Dyn -> Array Dyn -> Bool
  | doc m%"
      Test if two lists have no common element.
     It should be slightly more efficient than (intersectLists a b == [])
  "%m
  = fun a b => array.length a == 0 || !(any (fun x => array.elem x a) b),

}
