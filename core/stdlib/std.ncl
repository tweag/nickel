# Nickel standard library. This file is included at compile-time in the nickel
# binary and populates the `std` entry in the initial environment.
#
# It would be nice to split this module file into separate files, such as
# `array.ncl`, `contract.ncl`, etc. However, we can't rely on normal imports for
# that, because Nickel would need to find those file at run-time (the whole
# point of embedding the stdlib into the binary is to avoid having to think
# about where to find those files).
#
# One possibility would be to assemble the std module from the files
# programmatically, but then error reporting and the LSP would be impacted in a
# bad way (in particular, breaking the contract of a stdlib function would point
# to generated code that is nowhere accessible to the user).
#
# Another solution is to have special imports, like `<std/array.ncl>`. This is a
# possible direction.
#
# In the meantime, we choose to have this one big monolithic `std` module. This
# choice doesn't negatively impact Nickel users (the file can be browsed looking
# for error locations, typechecking and the LSP are happy because no code is
# programmatically generated). It's just less nice to maintain.
#
# ## Typing
#
# Stdlib modules shouldn't recursively refer to other modules directly, they
# should use the stdlib provided in the environment instead. This means using
# `std.array` from within the `record` module instead of just `array`. While the
# latter could work in principle, typechecking mutually recursive records is
# just harder, and the chances are this won't typecheck.
#
# On the other hand, the `std` module in the environment is properly typed, as
# the typed interface is elaborated upfront.
#
# ## Both a type annotation and a contract annotation
#
# [^type-and-contract]: Some functions have both a static type annotation and
# an additional contract. The additional contract annotation is ignored by
# typechecking, and with respect to static typing, only the static type
# annotation matters.
#
# However, static types are often not enough to check more advanced
# preconditions (e.g. that the argument of `array.drop_first` is a non-empty
# array). In this case, a second contract is used to only check those additional
# properties at runtime, while preserving the static typechecking behavior.
#
# cf https://github.com/tweag/nickel/issues/1354
{
  array = {
    NonEmpty
      | doc m%"
          Enforces that an array is not empty.

          # Examples

          ```nickel multiline
          ([] | std.array.NonEmpty)
          # => error: contract broken by a value

          ([ 1 ] | std.array.NonEmpty)
          # => [ 1 ]
          ```
        "%
      =
        %contract/custom% (fun _label value =>
          if %typeof% value == 'Array then
            if %array/length% value != 0 then
              'Ok value
            else
              'Error { message = "empty array" }
          else
            'Error { message = "not a array" }
        ),

    first
      : forall a. Array a -> a
      | NonEmpty -> Dyn
      | doc m%"
          Returns the first element of an array.

          # Examples

          ```nickel
          std.array.first [ "this is the head", "this is not" ]
          # => "this is the head"
          ```
        "%
      = fun array => %array/at% array 0,

    last
      : forall a. Array a -> a
      | NonEmpty -> Dyn
      | doc m%"
          Returns the last element of an array.

          # Examples

          ```nickel
          std.array.last [ "this is the head", "this is not" ]
          # => "this is not"
          ```
        "%
      = fun array => %array/at% array (%array/length% array - 1),

    drop_first
      : forall a. Array a -> Array a
      | NonEmpty -> Dyn
      | doc m%"
          Returns the given array without its first element.

          # Examples

          ```nickel
          std.array.drop_first [ 1, 2, 3 ]
          # => [ 2, 3 ]
          ```
        "%
      = fun array => %array/slice% 1 (%array/length% array) array,

    drop_last
      : forall a. Array a -> Array a
      | NonEmpty -> Dyn
      | doc m%"
          Returns the given array without its last element.

          # Examples

          ```nickel
          std.array.drop_last [ 1, 2, 3 ]
          # => [ 1, 2 ]
          ```
        "%
      = fun array => %array/slice% 0 (%array/length% array - 1) array,

    length
      : forall a. Array a -> Number
      | doc m%"
          Returns the length of an array.

          # Examples

          ```nickel
          std.array.length [ "Hello,", " World!" ]
          # => 2
          ```
        "%
      = fun l => %array/length% l,

    map
      : forall a b. (a -> b) -> Array a -> Array b
      | doc m%"
          Applies a function to every element in the given array. That is,
          `map f [ x1, x2, ..., xn ]` is `[ f x1, f x2, ..., f xn ]`.

          # Examples

          ```nickel
          std.array.map (fun x => x + 1) [ 1, 2, 3 ]
          # => [ 2, 3, 4 ]
          ```
        "%
      = fun f l => %array/map% l f,

    at
      : forall a. Number -> Array a -> a
      | std.contract.unstable.IndexedArrayFun 'Index
      | doc m%"
          Retrieves the n-th element from an array, with indices starting at 0.

          # Examples

          ```nickel
          std.array.at 3 [ "zero", "one", "two", "three", "four" ]
          # => "three"
          ```
        "%
      = fun n l => %array/at% l n,

    at_or
      : forall a. Number -> a -> Array a -> a
      | std.number.Nat -> Dyn
      | doc m%"
          Retrieves the n-th element from an array, with indices starting at 0
          or the provided default value if the index is greater than the length
          of the array.

          # Examples

          ```nickel multiline
          std.array.at_or 3 "default" [ "zero", "one", "two", "three" ]
          # => "three"

          std.array.at_or 3 "default" [ "zero", "one" ]
          # => "default"
          ```
        "%
      = fun n default_value array =>
        if n < %array/length% array then
          %array/at% array n
        else
          default_value,

    concat
      : forall a. Array a -> Array a -> Array a
      | doc m%"
          Appends the second array to the first one.

          # Examples

          ```nickel
            std.array.concat [ 1, 2, 3 ] [ 4, 5, 6 ]
            # => [ 1, 2, 3, 4, 5, 6 ]
          ```
        "%
      = fun l1 l2 => l1 @ l2,

    fold_left
      : forall a b. (a -> b -> a) -> a -> Array b -> a
      | doc m%"
          Folds a function over an array. In a functional language like Nickel,
          folds serve a similar purpose to loops or iterators. `fold_left`
          iterates over an array, by repeatedly applying a function to each
          element, threading an additional arbitrary state (the accumulator, of
          type `a` in the signature) through the chain of applications.

          `fold_left f init [x1, x2, ..., xn]` results in `f (... (f (f init x1) x2) ...) xn`.

          This function is strict in the intermediate accumulator.

          # Left vs right

          Folds come in two variants, left and right. How to decide which one to
          use?

          - If the folded function isn't associative (such as subtraction), then
            each variant will give a different result. The choice is dictacted by
            which one you need. For example:

            ```nickel
            std.array.fold_right (-) 0 [1, 2, 3, 4]
            # => -2
            ```

            ```nickel
            std.array.fold_left (-) 0 [1, 2, 3, 4]
            # => -10
            ```
          - If the folded function is associative, both `fold_right` and
            `fold_left` return the same result. In that case, **`fold_left` is
            generally preferred**, because it forces the evaluation of the
            intermediate results resulting in less memory consumption and overall
            better performance (outside of pathological cases).
            `fold_left` also iterates from the start of the array, which
            correponds to the usual behavior of loops and iterators in most
            programming languages. There is one case where `fold_right` might be
            preferred, see the next point.
          - If the folded function is associative but _(left) short-circuiting_,
            meaning that it can sometimes determine the result without using the
            right argument, then `fold_right` provides early return. An example is
            the boolean AND operator `&&`: when evaluating `left && right`, if
            `left` is `false`, the whole expression will evaluate to `false`
            without even evaluating `right`. Consider the following expression:

            ```nickel
            std.array.replicate 1000 true
            # gives [false, .. true 1000 times]
            |> std.array.prepend false
            |> std.array.fold_right (&&) [false]
            ```

            Here, `fold_right` will stop at the first element, and the operation
            runs in constant time, given the definition of `fold_right` and the
            lazy evaluation of Nickel. If we had used `fold_left` instead, which
            is closer to a standard iterator, we would have iterated over all of
            the 1000 elements of the array.

          # Examples

          ```nickel
          fold_left (fun acc e => acc + e) 0 [ 1, 2, 3 ]
          # => (((0 + 1) + 2) + 3)
          # => 6
          ```
        "%
      = fun f acc array =>
        let length = %array/length% array in
        if length == 0 then
          acc
        else
          let rec go = fun acc n =>
            if n == length then
              acc
            else
              let next_acc =
                %array/at% array n
                |> f acc
              in
              go next_acc (n + 1)
              |> %seq% next_acc
          in
          go acc 0,

    try_fold_left
      : forall a b c. (a -> c -> [| 'Ok a, 'Error b |]) -> a -> Array c -> [| 'Ok a, 'Error b |]
      | doc m%"
          Folds a function over an array from left to right, possibly stopping early.

          This differs from `fold_left` in that the function being folded returns either
          `'Error y` (meaning that the folding should terminate, immediately returning
          `'Error y`) or `'Ok acc` (meaning that the folding should continue as usual,
          with the new accumulator `acc`). This early return can be used as an optimization,
          to avoid evaluating the whole array.

          # Examples

          This defines a function that returns the first element satisfying a predicate.

          ```nickel
          let find_first: forall a. (a -> Bool) -> Array a -> [| 'Some a, 'None |]
            = fun pred xs =>
              # Our fold function, which just ignores the accumulator and immediately
              # returns an element if it satisfies the predicate. Note that `'Error`
              # (which is the short-circuiting branch) means that we found something.
              let f = fun _acc x => if pred x then 'Error x else 'Ok null in
              try_fold_left f null xs |> match {
                'Ok _ => 'None,
                'Error x => 'Some x,
              }
          in
          let even = fun x => x % 2 == 0 in
          find_first even [1, 3, 4, 5, 2]
          # => 'Some 4
          ```
        "%
      = fun f acc array =>
        let length = %array/length% array in
        if length == 0 then
          'Ok acc
        else
          let rec go = fun acc n =>
            if n == length then
              'Ok acc
            else
              %array/at% array n
              |> f acc
              |> match {
                'Error e => 'Error e,
                'Ok next_acc =>
                  go next_acc (n + 1)
                  |> %seq% next_acc
              }
          in
          go acc 0,

    fold_right
      : forall a b. (a -> b -> b) -> b -> Array a -> b
      | doc m%"
          Folds a function over an array. Folds serve a similar purpose to loops or
          iterators in a functional language like Nickel. `fold_right` iterates
          over an array by repeatedly applying a function to each element and
          threading an additional arbitrary state (the accumulator of type `a` in
          the signature) through the chain of applications.

          `fold_right f init [x1, x2, ..., xn]` results in `f x1 (f x2 (... (f xn init) ...))`.

          # Left vs right

          Folds come in two variants, left and right. How to decide which one to
          use? Please refer to the documentation of `fold_left`.

          # Examples

          ```nickel
          std.array.fold_right (fun e acc => acc @ [e]) [] [ 1, 2, 3 ]
          # => ((([] @ [3]) @ [2]) @ [1])
          # => [ 3, 2, 1 ]
          ```
        "%
      = fun f fst l =>
        let length = %array/length% l in
        let rec go = fun n =>
          if n == length then
            fst
          else
            go (n + 1)
            |> f (%array/at% l n)
        in go 0,

    prepend
      : forall a. a -> Array a -> Array a
      | doc m%"
          Builds an array given the first element and the rest of the array.

          # Examples

          ```nickel
          std.array.prepend 1 [ 2, 3 ]
          # => [ 1, 2, 3 ]
          ```
        "%
      = fun x l => [x] @ l,

    append
      : forall a. a -> Array a -> Array a
      | doc m%"
          Builds an array given the last element and the rest of the array.

          # Examples

          ```nickel
          std.array.append 3 [ 1, 2 ]
          # => [ 1, 2, 3 ]
          ```
        "%
      = fun x l => l @ [x],

    reverse
      : forall a. Array a -> Array a
      | doc m%"
          Reverses an array.

          # Examples

          ```nickel
          std.array.reverse [ 1, 2, 3 ]
          # => [ 3, 2, 1 ]
          ```
        "%
      = fun l => fold_right (fun e acc => acc @ [e]) [] l,

    filter
      : forall a. (a -> Bool) -> Array a -> Array a
      | doc m%"
          `filter f xs` returns an array containing all elements from `xs` that satisfy `f`.

          # Examples

          ```nickel
          std.array.filter (fun x => x <= 3) [ 4, 3, 2, 5, 1 ]
          # => [ 3, 2, 1 ]
          ```
        "%
      = fun pred l => fold_left (fun acc x => if pred x then acc @ [x] else acc) [] l,

    flatten
      : forall a. Array (Array a) -> Array a
      | doc m%"
          Concatenates all elements of an array of arrays.

          # Examples

          ```nickel
          std.array.flatten [[1, 2], [3, 4]]
          # => [1, 2, 3, 4]
          ```
        "%
      = fun l => fold_left (fun acc l => acc @ l) [] l,

    all
      : forall a. (a -> Bool) -> Array a -> Bool
      | doc m%"
          Returns `true` if all elements in the given array satisfy the predicate,
          `false` otherwise.

          # Examples

          ```nickel multiline
          std.array.all (fun x => x < 3) [ 1, 2 ]
          # => true

          std.array.all (fun x => x < 3) [ 1, 2, 3 ]
          # => false
          ```
        "%
      = fun pred l => fold_right (fun x acc => if pred x then acc else false) true l,

    any
      : forall a. (a -> Bool) -> Array a -> Bool
      | doc m%"
          Returns `true` if at least one element in the given array satisfies
          the predicate, `false` otherwise.

          # Examples

          ```nickel multiline
          std.array.any (fun x => x < 3) [ 1, 2, 3, 4 ]
          # => true

          std.array.any (fun x => x < 3) [ 5, 6, 7, 8 ]
          # => false
          ```
        "%
      = fun pred l => fold_right (fun x acc => if pred x then true else acc) false l,

    # **Warning**: unless you know what you're doing, please don't change the
    # type of `elem` to be polymorphic.
    #
    # `elem` must operate on elements of type `Dyn` only, because `elem` performs
    # equality tests, and those aren't allowed on generic variables (as it
    # breaks parametricity).
    #
    # However, as a current work-around to use equality easily in typed code,
    # `(==)` has type `forall a. a -> a -> Bool`. We could
    # be tempted to assign the type `elem : forall a. a -> Array a -> Bool`, but
    # that would be lying (as `==` is currently lying as well), and more
    # importantly, the contract will fail on any non-trivial call at run-time.
    elem
      : Dyn -> Array Dyn -> Bool
      | doc m%"
          Returns `true` if the given value appears in the array, `false` otherwise.

          # Examples

          ```nickel
          std.array.elem 3 [ 1, 2, 3, 4, 5 ]
          # => true
          ```
        "%
      = fun elt => any (fun x => x == elt),

    partition
      : forall a. (a -> Bool) -> Array a -> { right : Array a, wrong : Array a }
      | doc m%"
          Partitions an array into two new arrays. `right` will contain all
          elements that satisfy the predicate, while `wrong` will contain those
          that do not.

          # Examples

          ```nickel
          std.array.partition (fun x => x < 5) [ 2, 4, 5, 3, 7, 8, 6 ]
          # => { right = [ 2, 4, 3 ], wrong = [ 5, 7, 8, 6 ] }
          ```
        "%
      = fun pred l =>
        let aux = fun acc x =>
          if pred x then
            { right = acc.right @ [x], wrong = acc.wrong }
          else
            { right = acc.right, wrong = acc.wrong @ [x] }
        in
        fold_left aux { right = [], wrong = [] } l,

    generate
      : forall a. (Number -> a) -> Number -> Array a
      | Dyn -> std.number.Nat -> Dyn
      | doc m%"
          `generate f n` returns an array of length `n` by applying `f` to the
          integers from `0` to `n-1`. That is, `generate f n` is
          `[ f 0, f 1, ..., f (n - 1)]`

          # Examples

          ```nickel
          std.array.generate (fun x => x * x) 4
          # => [ 0, 1, 4, 9 ]
          ```
        "%
      = fun f n => %array/generate% n f,

    compare
      : forall a. (a -> a -> [| 'Lesser, 'Equal, 'Greater |]) -> Array a -> Array a -> [| 'Lesser, 'Equal, 'Greater |]
      | doc m%"
          `compare order a b` lexicographically compares `a` and `b`, using `order` to
          compare the individual elements.

          # Examples

          ```nickel multiline
          std.array.compare std.number.compare [3, 1, 4] [6, 2, 8]
          # => 'Lesser

          std.array.compare std.string.compare ["hello", "world"] ["hello"]
          # => 'Greater
          ```
        "%
      =
        let refine = fun left right =>
          left
          |> match {
            'Equal => right,
            _ => left,
          }
        in
        fun order a b =>
          zip_with order a b
          @ [std.number.compare (length a) (length b)]
          |> fold_right refine 'Equal,

    sort
      : forall a. (a -> a -> [| 'Lesser, 'Equal, 'Greater |]) -> Array a -> Array a
      | doc m%"
          Sorts an array based on the provided comparison operator.

          # Examples

          ```nickel
          std.array.sort (fun x y =>
            if x < y then
              'Lesser
            else if (x == y) then
              'Equal
            else
              'Greater)
            [ 4, 5, 1, 2 ]
          # => [ 1, 2, 4, 5 ]
          ```
        "%
      #TODO: maybe inline partition to avoid contract checks?
      = fun cmp array =>
        let length = %array/length% array in
        let first = %array/at% array 0 in
        let rest = %array/slice% 1 length array in
        let parts = partition (fun x => (cmp x first == 'Lesser)) rest in
        if length <= 1 then
          array
        else
          (sort cmp (parts.right)) @ [first] @ (sort cmp (parts.wrong)),

    flat_map
      : forall a b. (a -> Array b) -> Array a -> Array b
      | doc m%"
          First `map` the given function over the array and then `flatten` the
          result.

          # Examples

          ```nickel
          std.array.flat_map (fun x => [x, x]) [1, 2, 3]
          # => [1, 1, 2, 2, 3, 3]
          ```
        "%
      = fun f xs => map f xs |> flatten,

    intersperse
      : forall a. a -> Array a -> Array a
      | doc m%"
          Intersperses a value between the elements of an array.

          # Examples

          ```nickel multiline
          std.array.intersperse ", " [ "Hello", "wonderful", "world!" ]
          # => [ "Hello", ", ", "wonderful", ", ", "world!" ]

          std.array.intersperse ", " [ "Hello" ]
          # => [ "Hello" ]

          std.array.intersperse ", " []
          # => []
          ```
        "%
      = fun v array =>
        let length = %array/length% array in
        if length <= 1 then
          array
        else
          let
            first = %array/at% array 0,
            rest = %array/slice% 1 length array,
          in
          [first] @ (flat_map (fun a => [v, a]) rest),

    slice
      : forall a. Number -> Number -> Array a -> Array a
      | std.contract.unstable.ArraySliceFun
      | doc m%"
          `slice start end array` returns the slice of `array` between `start` (included) and
          `end` (excluded).

          # Preconditions

          In `slice start end value`, `start` and `end` must be positive
          integers such that `0 <= start <= end <= std.array.length value`.

          # Examples

          ```nickel multiline
          std.array.slice 1 3 [ 0, 1, 2, 3, 4, 5]
          # => [ 1, 2 ]

          std.array.slice 0 3 [ "Hello", "world", "!" ]
          # => [ "Hello", "world", "!" ]

          std.array.slice 2 3 [ "Hello", "world", "!" ]
          # => [ "!" ]
           ```
        "%
      = fun start end value => %array/slice% start end value,

    split_at
      : forall a. Number -> Array a -> { left : Array a, right : Array a }
      | std.contract.unstable.IndexedArrayFun 'Split
      | doc m%"
          Splits an array in two at a given index and puts all the elements
          to the left of the element at the given index (excluded) in the
          `left` field, and the rest of the array in the `right` field.

          # Preconditions

          In `split_at index value`, `index` must be a positive integer such
          that `0 <= index <= std.array.length value`.

          # Examples

          ```nickel multiline
          std.array.split_at 2 [ 0, 1, 2, 3, 4, 5]
          # => { left = [ 0, 1 ], right = [ 2, 3, 4, 5 ] }

          std.array.split_at 0 [ "Hello", "world", "!" ]
          # => { left = [  ], right = [ "Hello", "world", "!" ] }

          std.array.split_at 3 [ "Hello", "world", "!" ]
          # => { left = [ "Hello", "world", "!" ], right = [  ] }
          ```
        "%
      = fun index value =>
        {
          left = %array/slice% 0 index value,
          right = %array/slice% index (%array/length% value) value
        },

    replicate
      : forall a. Number -> a -> Array a
      | std.number.Nat -> Dyn
      | doc m%"
          `replicate n x` creates an array containing `x` exactly `n` times.

          # Preconditions

          `n` must be an integer greater or equal to `0`.

          # Examples

          ```nickel multiline
          std.array.replicate 0 false
          # => [ ]

          std.array.replicate 5 "x"
          # => [ "x", "x", "x", "x", "x" ]
          ```
        "%
      = fun n x => %array/generate% n (fun _i => x),

    range_step
      : Number -> Number -> Number -> Array Number
      | std.contract.unstable.RangeFun (std.contract.unstable.RangeStep -> Dyn)
      | doc m%"
          `range_step start end step` generates the array of numbers
          `[start, start + step, start + 2*step, ..]` up to the first element
          (excluded) larger than or equal to `end`

          # Preconditions

          In `range_step start end step`, `start` and `end` must satisfy `start
          <= end`. `step` must be strictly greater than `0`.

          # Examples

          ```nickel
          std.array.range_step (-1.5) 2 0.5
           # => [ -1.5, -1, -0.5, 0, 0.5, 1, 1.5 ]
          ```
        "%
      = fun start end step =>
        %array/generate%
          ((end - start) / step |> std.number.floor)
          (fun i => start + i * step),

    range
      : Number -> Number -> Array Number
      | std.contract.unstable.RangeFun Dyn
      | doc m%"
          `range start end` generates the array of numbers
          `[start, start + 1, start + 2, ..]` up to the first element
          (excluded) larger than or equal to `end`.

          `range start end` is equivalent to `range_step start end 1`.

          # Preconditions

          In `range_step start end`, `start` and `end` must satisfy
          `start <= end`.

          # Examples

          ```nickel
          std.array.range 0 5
          # => [ 0, 1, 2, 3, 4 ]
          ```
        "%
      = fun start end => range_step start end 1,

    reduce_left
      : forall a. (a -> a -> a) -> Array a -> a
      | Dyn -> NonEmpty -> Dyn
      | doc m%"
          Reduces the elements to a single one, by repeatedly applying a
          reducing operation.

          `reduce_left` associates to the left, that is
          `reduce_left op [x1, x2, ..., xn]` results in `op (... (op (op x1 x2) x3) ...) xn`.

          `reduce_left` is the same as `fold_left`, but uses the first element as
          the initial accumulator.

          # Preconditions

          The provided array must be non-empty.

          # Left vs right

          The rationale to decide between `fold_left` and `fold_right` applies to
          `reduce_left` and `reduce_right` as well. See the documentation of
          `fold_left`.

          # Examples

          ```nickel multiline
          std.array.reduce_left (@) [ [1, 2], [3], [4,5] ]
          # => (([1, 2] @ [3]) @ [4,5])
          # => [ 1, 2, 3, 4, 5 ]

          std.array.reduce_left (-) [ 1, 2, 3, 4]
          # => ((1 - 2) - 3) - 4
          # => -8
          ```
        "%
      = fun f array =>
        let first = %array/at% array 0 in
        let rest = %array/slice% 1 (%array/length% array) array in
        fold_left f first rest,

    reduce_right
      : forall a. (a -> a -> a) -> Array a -> a
      | Dyn -> NonEmpty -> Dyn
      | doc m%"
          Reduces the elements to a single one, by repeatedly applying a reducing
          operation.

          `reduce_right` associates to the right, that is
          `reduce_right op [x1, x2, ..., xn]` results in
          `op x1 (op x2 (... (op xn-1 xn) ...))`.

          `reduce_right` is the same as `fold_right`, but uses the last element as
          the initial element.

          # Preconditions

          The provided array must be non-empty.

          # Left vs right

          The rationale to decide between `fold_left` and `fold_right` applies to
          `reduce_left` and `reduce_right` as well. See the documentation of
          `fold_left`.

          # Examples

          ```nickel multiline
          std.array.reduce_right (@) [ [1, 2], [3], [4,5] ]
          # => [1, 2] @ ([3] @ [4,5])
          # => [ 1, 2, 3, 4, 5 ]

          std.array.reduce_right (-) [ 1, 2, 3, 4]
          # => 1 - (2 - (3 - 4))
          # => -2
          ```
        "%
      = fun f array =>
        let last_index = %array/length% array - 1 in
        let
          last = %array/at% array last_index,
          rest = %array/slice% 0 last_index array,
        in
        fold_right f last rest,

    zip_with
      : forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c
      | doc m%"
          `zip_with f xs ys` combines the arrays `xs` and `ys` using the
          operation `f`. The resulting array's length will be the smaller of the
          lengths of `xs` and `ys`.

          # Examples

          ```nickel multiline
          std.array.zip_with (+) [1, 2, 3] [4, 5, 6]
          # => [5, 7, 9]

          std.array.zip_with (*) [1, 2] [4, 5, 6]
          # => [4, 10]

          std.array.zip_with (-) [1, 2, 3] [4, 5]
          # => [-3, -3]
          ```
        "%
      = fun f xs ys =>
        std.array.generate
          (fun i => f (std.array.at i xs) (std.array.at i ys))
          (std.number.min (std.array.length xs) (std.array.length ys)),

    map_with_index
      : forall a b. (Number -> a -> b) -> Array a -> Array b
      | doc m%"
          Applies a function to every element in the given array while passing
          its (0-based) index. That is,
          `map_with_index f [ x1, x2, ... ]` is `[ f 0 x1, f 1 x2, ... ]`.

          # Examples

          ```nickel
          std.array.map_with_index (fun i x => i + x + 1) [ 1, 2, 3 ]
          # => [ 2, 4, 6 ]
          ```
        "%
      = fun f xs =>
        xs
        |> std.array.length
        |> std.array.generate (fun i => f i (std.array.at i xs)),

    filter_map
      : forall a b. (a -> [| 'Some b, 'None |]) -> Array a -> Array b
      | doc m%"
        Applies a function to every element in the given array, filtering out
        `'None` results. `filter_map` combines `std.array.map` and
        `std.array.filter` in a single pass.

        # Examples

        ```nickel
        ["1", "hello", "2", "world"]
        |> std.array.filter_map (fun x =>
          if std.string.is_match "^[+-]?\\d+$" x then
            'Some (std.string.to_number x)
          else
            'None
        )
        # => [ 1, 2 ]
        ```
      "%
      = fun f array =>
        fold_left
          (fun acc x =>
            f x
            |> match {
              'Some y => acc @ [y],
              'None => acc,
            }
          )
          []
          array,

    dedup
      : Array Dyn -> Array Dyn
      | doc m%"
        Removes duplicates from an array.

        # Performance

        This function relies on equality and has a quadratic complexity in the
        length of the array. It might thus be slow for large arrays or if
        called repeatedly. Prefer `std.array.sort_dedup` or
        `std.array.dedup_sorted` if you have efficiency concerns.

        # Examples

        ```nickel multiline
        std.array.dedup [ 4, 2, 1, 3, 5, 2, 1, 4 ]
        # => [ 4, 2, 1, 3, 5 ]

        std.array.dedup [ "hello", "world", "hello" ]
        # => [ "hello", "world" ]
        ```
      "%
      = fun array =>
        let length = %array/length% array in

        fold_left
          (fun acc x =>
            if elem x acc then
              acc
            else
              acc @ [x]
          )
          []
          array,

    dedup_sorted
      : forall a. (a -> a -> [| 'Lesser, 'Equal, 'Greater |]) -> Array a -> Array a
      | doc m%"
        Given an array already sorted by the given ordering function, remove duplicates.

        # Preconditions

        The input array must be sorted using the same order as provided to this
        function. Otherwise, some duplicates might not be removed.

        # Performance

        As opposed to `std.array.dedup`, this function has a linear time
        complexity, which should improve performance especially on large arrays.

        If you sort the array only to remove duplicates, you can use
        `std.array.sort_dedup` directly (which handles both sorting and
        deduplication).

        # Examples

        ```nickel multiline
        std.array.dedup_sorted std.number.compare [ 1, 2, 2, 3, 4, 4, 4, 5 ]
        # => [ 1, 2, 3, 4, 5 ]

        std.array.dedup_sorted std.string.compare [ "hello", "world", "world" ]
        # => [ "hello", "world" ]
        ```
      "%
      = fun cmp array =>
        let length = %array/length% array in

        let rec go = fun acc n =>
          if n == length then
            acc
          else
            let x = %array/at% array n in

            # we would prefer to use the primitive equality here instead of
            # relying on `cmp` returning `'Equal`. The latter 1. requires that
            # the user-provided function is correct and 2. is more costly.
            #
            # Unfortunately, that would break parametricity and require to
            # substitute `a` for `Dyn` in the type of `sort_dedup`, which makes
            # it more annoying to use with otherwise statically typed comparison
            # function. So for now, we rely on the comparison function.
            let acc =
              if n == 0
              || cmp (%array/at% array (n - 1)) x != 'Equal then
                acc @ [x]
              else
                acc
            in

            go acc (n + 1)
        in

        go [] 0,

    sort_dedup
      : forall a. (a -> a -> [| 'Lesser, 'Equal, 'Greater |]) -> Array a -> Array a
      | doc m%"
        Sorts an array based on the provided comparison operator and removes
        duplicates.

        This is a combination of `std.array.sort` and `std.array.dedup_sorted`.

        # Performance

        As opposed to `std.array.dedup`, this function has a better time
        complexity (`O(n*log(n))` where `n` is the size of the array), which
        should improve performance especially on large arrays.

        # Examples

        ```nickel multiline
        std.array.sort_dedup std.number.compare [ 4, 2, 1, 3, 5, 2, 1, 4 ]
        # => [ 1, 2, 3, 4, 5 ]

        std.array.sort_dedup std.string.compare [ "world", "hello", "world" ]
        # => [ "hello", "world" ]
        ```
      "%
      = fun cmp array =>
        array
        |> sort cmp
        |> dedup_sorted cmp,
  },

  contract = {
    Equal
      | doc m%"
        `Equal` is a delayed contract enforcing equality to a given constant.

        `Equal` is lazy over arrays and records. When checking such values,
        `Equal` doesn't test for equality of elements right away (it just tests
        that the size is the same for arrays and that fields are the same for
        records), but returns a new value where equality subcontracts have been
        pushed inside each element.

        # Example

        ```nickel multiline
        1 + 4 | std.contract.Equal 5
        # => 5

        4 | std.contract.Equal 5
        # => error: contract broken by a value
        ```
      "%
      =
        let blame_fields_differ = fun qualifier fields ctr_label =>
          let plural = if %array/length% fields == 1 then "" else "s" in
          ctr_label
          |> label.with_message "%{qualifier} field%{plural} `%{std.string.join ", " fields}`"
          |> label.append_note "`std.contract.Equal some_record` requires that the checked value is equal to the record `some_record`, but the sets of their fields differ."
          |> blame
        in

        fun constant =>
          let constant_type = %typeof% constant in
          let check_typeof_eq = fun ctr_label value =>
            let value_type = %typeof% value in
            if value_type == constant_type && value_type == 'Enum then
              if %enum/is_variant% value != %enum/is_variant% constant then
                let enum_kind = fun x =>
                  if %enum/is_variant% x then
                    "enum variant"
                  else
                    "enum tag"
                in

                ctr_label
                |> label.with_message "expected an %{enum_kind constant}, got an %{enum_kind value}"
                |> label.append_note "`std.contract.Equal some_enum` requires that the checked value is equal to the enum `some_enum`, but they are different variants."
                |> blame
              else
                value
            else if value_type == constant_type then
              value
            else
              ctr_label
              |> label.with_message "expected `%{%to_string% constant_type}`, got `%{%to_string% value_type}`"
              |> label.append_note "`std.contract.Equal some_value` requires that the checked value is equal to `some_value`, but they don't have the same type."
              |> blame
          in
          constant_type
          |> match {
            'Record =>
              # we map the constant from {field1 = val1, .., fieldn = valn} to
              # {field1 = Equal val1, .., fieldn = Equal valn}, building a
              # dictionary of equality contracts
              let contract_map = %record/map% constant (fun _key => Equal) in
              fun ctr_label value =>
                let value = check_typeof_eq ctr_label value in
                let split_result = %record/split_pair% constant value in
                if split_result.right_only != {} then
                  blame_fields_differ
                    "extra"
                    (%record/fields% split_result.right_only)
                    ctr_label
                else if split_result.left_only != {} then
                  blame_fields_differ
                    "missing"
                    (%record/fields% split_result.left_only)
                    ctr_label
                else
                  %contract/record_lazy_apply%
                    ctr_label
                    value
                    (fun field =>
                      contract_map."%{field}"
                    ),
            'Array =>
              fun ctr_label value =>
                let value = check_typeof_eq ctr_label value in
                let value_length = %array/length% value in
                if value_length == %array/length% constant then
                  %array/generate%
                    value_length
                    (fun i =>
                      %array/at% value i
                      |> std.contract.apply (Equal (%array/at% constant i)) ctr_label
                    )
                else
                  ctr_label
                  |> label.with_message "array length mismatch (expected `%{%to_string% (%array/length% constant)}`, got `%{%to_string% value_length})`"
                  |> label.append_note "`std.contract.Equal some_array` requires that the checked value is equal to the array `some_array`, but their lengths differ."
                  |> blame,

            # Outside of lazy data structures, we just use (==)
            _ =>
              fun ctr_label value =>
                let value = check_typeof_eq ctr_label value in
                if value == constant then
                  value
                else
                  %blame% ctr_label,
          },

    blame
      | doc m%"
          Raises blame for a given label.

          Type: `forall a. Label -> a`
          (for technical reasons, this function isn't actually statically typed)

          Blame is the mechanism to signal contract violation in Nickel. It
          ends the program execution and prints a detailed report thanks to the
          information tracked inside the label.

          # Examples

          ```nickel
          let IsZero = fun label value =>
            if value == 0 then
              value
            else
              std.contract.blame label
          in
          0 | IsZero
          ```
        "%
      = fun label => %blame% label,

    blame_with_message
      | doc m%"
          Raises blame with respect to a given label with a custom error message.

          Type: `forall a. String -> Label -> a`
          (for technical reasons, this function isn't actually statically typed)

          Same as `blame`, but takes an additional error message that will be
          displayed as part of the blame error. `blame_with_message message label`
          is equivalent to `blame (label.with_message message label)`

          # Examples

          ```nickel
          let IsZero = fun label value =>
            if value == 0 then
              value
            else
              std.contract.blame_with_message "Not zero" label
          in

          0 | IsZero
          ```
        "%
      = fun message label => %blame% (%label/with_message% message label),

    custom
      : (
        Dyn -> Dyn -> [|
          'Ok Dyn,
          'Error {
            message | String | optional,
            notes | Array String | optional
          }
        |]
      ) -> Dyn
      | doc m%"
          Build a generic custom contract. Whenever possible, more restricted
          constructors such as `std.contract.from_predicate` or
          `std.contract.from_validator` should be used. However, the most
          general `std.contract.custom` might be required to write delayed
          contracts or contracts taking other contracts as parameters.

          # Custom contracts

          The argument of `std.contract.custom` is a function that takes a label
          and the value to be checked, and returns `'Ok value` to return
          the original value with potential delayed checks inside, or `'Error
          {..}` to signal an immediate contract violation.

          The label should only be used for delayed checks. The preferred method
          to indicate a contract violation is to return `'Error {..}`.

          For more details on custom contracts and delayed contracts, please
          refer to the corresponding section of the user manual.

          # Typing

          Because Nickel doesn't currently have proper types for labels and
          contracts, they are replaced with `Dyn` in the type signature. You can
          think of the actual type of the delayed part as `(Label -> Dyn ->
          [| ... |])`, and the return type of `custom` as `Contract`.

          # Examples

          ```nickel
          let Nullable = fun Contract =>
            std.contract.custom (fun label value =>
              if value == null then
                'Ok value
              else
                std.contract.check Contract label value
            )
          in

          null | Nullable Number
          # => null
          ```
        "%
      = fun contract => %contract/custom% contract,

    from_predicate
      | (Dyn -> Bool) -> Dyn
      | doc m%"
          Build a contract from a boolean predicate.

          # Contract application

          Before Nickel 1.8, `from_predicate` used to wrap the predicate as a
          naked function of a label and value. You could thus theoretically
          apply it as a normal function, as in
          `(std.contract.from_predicate my_pred) label value`. While possible,
          this wasn't recommended nor really officially supported: all contracts
          should be applied using a contract annotation or `std.contract.apply`
          and its variants, as explained in the documentation of
          `std.contract.apply`.

          From Nickel 1.8 and onwards, `from_predicate` generates a bespoke
          contract value that can only be used in an annotation, be passed to
          `std.contract.apply` or other operations on contracts, but can't be
          used as normal function anymore.

          # Typing

          Because Nickel doesn't currently have proper types for contracts, the
          return type of `from_predicate` is simply `Dyn` in the type signature.
          You should think of `from_predicate` as some returning a value of some
          hypothetical type `Contract`.

          # Examples

          ```nickel
          let IsZero = std.contract.from_predicate (fun x => x == 0) in
          0 | IsZero
          ```
        "%
      = fun pred =>
        %contract/custom% (fun _label value =>
          if pred value then
            'Ok value
          else
            'Error {}
        ),

    from_validator
      | (
        Dyn -> [|
          'Ok,
          'Error {
            message
              | String
              | optional,
            notes
              | Array String
              | optional,
          }
        |]
      ) -> Dyn
      | doc m%%"
          Build a contract from a validator.

          # Validator

          A validator is a function returning either `'Ok` or `'Error {message,
          notes}` with an optional error message and notes to display. Both
          validators and predicates are immediate contracts: they must decide
          right away if the value passes or fails (as opposed to delayed
          contracts). But as opposed to predicates, validators have the
          additional ability to customize the error reporting.

          # Typing

          Because Nickel doesn't currently have proper types for contracts, the
          return type of `from_validator` is simply `Dyn` in the type signature.
          You should think of `from_validator` as returning a value of some
          hypothetical type `Contract`.

          # Examples

          ```nickel
          let IsZero = std.contract.from_validator (match {
            0 => 'Ok,
            n if std.is_number n =>
              'Error {
                message = "expected 0, got %{std.to_string n}",
                notes = ["The value is a number, but it isn't 0"],
              },
            v =>
              let vtype = v |> std.typeof |> std.to_string in
              'Error { message = "expected a number, got a %{vtype}" }
          })
          in

          0 | IsZero
          ```
        "%%
      # A validator turns out to be a valid custom contract as well (which just
      # never returns a value with delayed checks)
      = fun validator =>
        %contract/custom% (fun label value =>
          let result = validator value in

          if result == 'Ok then
            'Ok value
          else
            result
        ),

    Sequence
      | Array Dyn -> Dyn
      | doc m%"
          Apply multiple contracts from left to right.

          Type: `Array Contract -> Contract`
          (for technical reasons, this function isn't actually statically typed)

          # Examples

          ```nickel ignore
          x | std.contract.Sequence [ Foo, Bar ]
          ```

          is equivalent to

          ```nickel ignore
          (x | Foo) | Bar
          ```

          This is useful in positions where one cannot apply multiple contracts,
          such as an argument to another contract:

          ```nickel ignore
          x | Array (std.contract.Sequence [ Foo, Bar ])
          ```

          Or stored in a variable:

          ```nickel ignore
          let C = std.contract.Sequence [ Foo, Bar ] in
          x | C
          ```
        "%
      = fun contracts =>
        %contract/custom% (fun label value =>
          std.array.try_fold_left
            (fun acc Contract => std.contract.check Contract label acc)
            value
            contracts
        ),

    label
      | doc m%"
          The label submodule provides functions that manipulate the label
          associated with a contract application.

          A label is a special opaque value automatically passed by the Nickel
          interpreter to contracts when performing a contract check.

          A label stores a stack of custom error diagnostics, that can be
          manipulated by the functions in this module. Labels thus offer a way to
          customize the error message that will be shown if a contract is broken.

          Note that the label should only be used for delayed checks. Immediate
          error reporting is better done in custom contracts by returning an
          error value.

          The setters (`with_XXX` functions) always operate on the current error
          diagnostic, which is the last diagnotic on the stack. If the stack is
          empty, a fresh diagnostic is silently created when using a setter.

          The previous diagnostics on the stack are considered archived, and
          can't be modified anymore. All diagnostics will be shown during error
          reporting, with the most recent being used as the main one.

          `std.contract.apply` and `std.contract.check` are the
          operations that push a new fresh diagnostic on the stack, saving the
          previously current error diagnostic. `apply` and friends are typically
          used to apply subcontracts inside a parent contract. Stacking the
          current diagnostic potentially customized by the parent contract saves
          the information inside, and provides a fresh diagnostic for the child
          contract to use.
        "%
      = {
        with_message
          | doc m%"
              Attaches a custom error message to the current error diagnostic of a
              label.

              Type: `String -> Label -> Label`
              (for technical reasons, this function isn't actually statically typed)

              If a custom error message was previously set, there are two
              possibilities:
                - the label has gone through a `std.contract.apply` call in-between.
                  In this case, the previous diagnostic has been stacked, and
                  using `with_message` won't erase anything but rather modify
                  the fresh current diagnostic.
                - no `std.contract.apply` has taken place since the last message
                  was set. In this case, the current diagnostic's error message
                  is replaced.

              # Examples

              This example implements a contract which ensures that the value is
              a record with a field `foo` that is an even number.

              ```nickel
              let FooIsEven = std.contract.custom (fun label =>
                match {
                  record @ { foo, .. } =>
                    'Ok (
                      std.record.map (fun key value =>
                        if key == "foo"
                        && !(std.is_number value && value % 2 == 0) then
                          label
                          |> std.contract.label.with_message "field foo must be an even number"
                          |> std.contract.blame
                        else
                          value
                      )
                      record
                    ),
                  _ => 'Error {},
                }
              )
              in

              { foo = 4, hello = "world" } | FooIsEven
              # => { foo = 4, hello = "world" }
              ```
            "%
          = fun message label => %label/with_message% message label,

        with_notes
          | doc m%"
              Attaches custom error notes to the current error diagnostic of a
              label.

              Type: `Array String -> Label -> Label`
              (for technical reasons, this function isn't actually statically typed)

              If custom error notes were previously set, there are two
              possibilities:
                - the label has gone through a `std.contract.apply` call in-between.
                  In this case, the previous diagnostic has been stacked, and
                  using `with_notes` won't erase anything but rather modify the
                  fresh current diagnostic.
                - no `std.contract.apply` has taken place since the last message was
                  set. In this case, the current diagnostic's error notes will
                  be replaced.

              # Examples

              This example is illustrative. In practice, returning `'Error {..}`
              should always be preferred over manipulating the label directly,
              at least whenever possible.

              ```nickel
              let AlwaysFailWithNotes = std.contract.custom (fun label _ =>
                label
                |> std.contract.label.with_notes [
                  "This is note 1",
                  "This is note 2",
                ]
                |> std.contract.blame
              )
              in

              null | AlwaysFailWithNotes

              # => error
              ```
            "%
          # the %label/with_notes% operator expects an array of strings which is
          # fully evaluated, thus we force the notes first
          = fun notes label => %label/with_notes% (%force% notes) label,

        append_note
          | doc m%"
              Appends a note to the error notes of the current diagnostic of a label.

              Type: `String -> Label -> Label`
              (for technical reasons, this function isn't actually statically typed)

              # Examples

              This example is illustrative. In practice, returning `'Error {..}`
              should always be preferred over manipulating the label directly
              whenever possible.

              ```nickel
              let AlwaysFailWithNotes = std.contract.custom (fun label _ =>
                label
                |> std.contract.label.append_note "This is note 1"
                |> std.contract.label.append_note "This is note 2"
                |> std.contract.blame
              )
              in

              null | AlwaysFailWithNotes
              # => error
              ```
            "%
          = fun note label => %label/append_note% note label,
      },

    apply
      | doc m%"
          Applies a contract to a label and a value. Either aborts the execution
          with a broken contract error if the contract fails, or proceeds with
          the evaluation of the value.

          A contract annotation such as `value | Contract` is eventually
          translated (automatically) to a call to `std.contract.apply Contract label
          value` where `label` is provided by the Nickel interpreter.

          Note that depending on the situation, you might need the variant
          `std.contract.check` instead. Please refer to the documentation of
          `std.contract.check` for more information.

          # Type

          For technical reasons, `apply` isn't statically typed nor protected by
          a contract. The hypothetical type of `apply` is `Contract -> Label ->
          Dyn -> Dyn` (note `Contract` and `Label` currently don't exist).

          # Arguments

          `apply` takes three arguments. The arguments are in order:

          - a contract which is currently either a custom contract (produced by
            the stdlib constructors `std.contract.custom`,
            `std.contract.from_validator` or `std.contract.from_predicate`), a
            record, a static type or a naked function (see legacy contracts
            below).
          - a label (see `std.contract.label`)
          - the value to be checked

          # Return value

          `apply` returns the original value upon success, with potential
          delayed checks inserted inside. Some contracts even return a modified
          value, such as record contract which can set default values.

          # Legacy contracts

          For backward-compatibility reasons, the argument can also be a naked
          function `Label -> Dyn -> Dyn`, but this is discouraged and will be
          deprecated in the future. For such a contract, please wrap it using
          `std.contract.custom` instead (the return type must be adapted).

          # Examples

          This example is a custom re-implementation of the builtin contract
          `[| 'Foo Number |]`. This makes for a short illustration of `apply`,
          but you should of course just use the builtin contract in real-world
          applications.

          ```nickel
          let FooOfNumber =
            std.contract.custom
              (fun label =>
                match {
                  'Foo arg => 'Ok ('Foo (std.contract.apply Number label arg)),
                  _ => 'Error {},
                }
              )
          in
          'Foo 3 | FooOfNumber
          ```

          # Diagnostics stack

          Using `apply` will stack the current custom error reporting data, and
          create a fresh working diagnostic. `apply` thus acts as a split point
          between a contract and its subcontracts, providing the subcontracts
          with a fresh diagnostic to use, while remembering the previous
          diagnostics set by parent contracts.

          ## Illustration

          ```nickel
          let ChildContract = std.contract.from_validator (fun _ =>
            'Error {
              message = "child's message",
              notes = ["child's note"]
            }
          )
          in

          let ParentContract = std.contract.from_validator (fun _ =>
            'Error {
              message = "parent's message",
              notes = ["parent's note"]
            }
          )
          in

          null | ParentContract
          # => error: contract broken by a value
          ```

          This example will print two diagnostics: the main one, using the
          message and note of the child contract, and a secondary diagnostic,
          using the parent contract message and note.

          This behavior is also observed when manipulating the label directly
          and using `std.contract.blame` instead of returning an error value.
          ```
        "%
      = fun contract label value =>
        %contract/apply% contract (%label/push_diag% label) value,

    check
      : Dyn -> Dyn -> Dyn -> [|
        'Ok Dyn,
        'Error {
          message | String | optional,
          notes | Array String | optional
        }
      |]
      | doc m%"
          Variant of `std.contract.apply` which returns `'Error {..}` upon
          immediate failure of the contract instead of aborting the execution
          with a blame error, or wraps the return value as `'Ok value`
          otherwise. Delayed errors may still cause uncatchable blame errors.

          `check` is intended to be used when calling another contract from a
          custom contract, since the return type of `check` is precisely the
          same as the return type of a custom contract, and because it preserves
          immediate errors.

          Another use-case is to check if the immediate part of a contract is
          failing or not without aborting the execution with an uncatchable
          error.

          `check` has the same behavior as `std.contract.apply` with respect to
          the diagnostic stack. See `std.contract.apply` for more details.

          Looking at the action of `check` on a custom contract might help: if
          `Contract` is a custom contract defined as `std.contract.custom
          contract_impl` where `contract_impl` is a function `fun label value =>
          ...`, then `std.contract.check Contract label value` is just the
          normal function application `contract_impl label value` (this isn't
          entirely true because of the diagnostic stack and how checked values
          are tracked for error reporting, but it's mostly true). Of course,
          `check` works on other types of contracts, such as static types or
          record contracts.

          # Examples

          ```nickel
          let Nullable = fun Contract =>
            std.contract.custom
              (fun label value =>
                if value == null then
                  'Ok value
                else
                  std.contract.check Contract label value
              )
          in

          { foo = 1 } | Nullable {foo | Number}
          ```
        "%
      = fun contract label value =>
        %contract/check% contract (%label/push_diag% label) value,

    any_of
      | Array Dyn -> Dyn
      | doc m%"
          Builds a contract that checks if the value satisfies at least one of
          the given contracts. More precisely, `any_of` applies each contract in
          order and returns the result of the first one that doesn't fail
          immediately (that is, the first one which returns the value `'Ok _`).

          **Important** `any_of` is only an approximation of what you could
          expect from an `or` operation on contracts. Because `any_of` must
          preserve the lazyness of contracts, it means that it has to
          pick the right branch by relying only on the immediate part of each
          contract, and not on the delayed part.

          Fortunately, `any_of` is an overstrict approximation, in that it might
          reject more values than what you expect, but it'll never let invalid
          values slip through.

          Typically, `std.contract.any_of [{ foo | String }, { foo | Number }]`
          will behave exactly as `{ foo | Number }`, because the immediate part
          of the built-in record contracts can't discriminate between the two
          possibilities. In particular, perhaps surprisingly, the value `{foo =
          1+1}` is rejected. Please refer to the boolean combinators section of
          the contract chapter of the manual for a more detailed
          explanation of the limitations of `any_of` and other boolean
          combinators, as well as how to work around them.

          # Examples

          ```nickel
          let Date = std.contract.any_of [
            String,
            {
              day | Number,
              month | Number,
              year | Number
            }
          ]
          in

          { day = 1, month = 1, year = 1970 } | Date
          ```
        "%
      = fun contracts =>
        %contract/custom% (fun label value =>
          std.array.try_fold_left
            (fun _acc Contract =>
              let label =
                %label/with_message%
                  "any_of: a delayed check of the picked branch failed"
                  label
              in
              std.contract.check Contract label value
              # We want to short-circuit on contract success. Since try_fold_left
              # short-circuits on failure, we need to flip the two.
              |> match {
                'Ok value => 'Error value,
                'Error msg => 'Ok msg
              }
            )
            ('Ok null)
            contracts
          |> match {
            'Ok _ =>
              'Error {
                message = "any_of: value didn't match any of the contracts",
              },
            'Error value => 'Ok value,
          }
        ),

    all_of
      : Array Dyn -> Dyn
      | doc m%"
          Builds a contract that checks if the value satisfies all of the given
          contracts. `all_of` is just an alias for `std.contract.Sequence`, to
          maintain consistency with `std.contract.any_of` and other JSON
          Schema-style combinators.

          As opposed to other boolean combinators such as `not` or `any_of`,
          `all_of` work as you expect most of the time, including on contracts
          with a delayed part. The only contracts for which `all_of` is
          overstrict are function contracts. In general, you shouldn't use
          function contracts with contract combinators.
          ```
        "%
      = std.contract.Sequence,

    not
      | Dyn -> Dyn
      | doc m%"
          Builds a contract that checks if the value doesn't satisfy the given
          contract. More specifically, `not Contract` will accept `value` if the
          application of `Contract` to `value` fails immediately, returning
          `'Error {..}`.

          **Important**: as for other boolean combinators, `not` is only an
          overstrict approximation of what you could expect. For example,
          `std.contract.not (Array Number)` will reject `["a"]`, because the
          check that elements are numbers is delayed. See the boolean
          combinators section of the contract chapter of the manual for a more
          detailed explanation of the limitations of `not` and other boolean
          combinators, as well as how to work around them.

          # Examples

          ```nickel
          let NotNumber = std.contract.not Number in
          "a" | NotNumber
          # => "a"
          ```
        "%
      = fun Contract =>
        %contract/custom% (fun label value =>
          value
          |> std.contract.check Contract label
          |> match {
            'Ok _ => 'Error { message = "not: value matched the immediate part of the contract" },
            error => 'Ok value,
          }
        ),

    unstable
      | doc m%"
          The unstable module gathers contracts that are used right now in the
          standard library to check additional pre-condition, but aren't yet
          stabilized.

          Unstable contracts are useful as a temporary solution for argument
          validation, but they shouldn't be used outside of the standard
          library. They aren't part of the backward compatibility policy, and
          are subject to change at any point in time.
        "%
      = {
        DependentFun
          | doc m%"
              **Warning**: this is an unstable item. It might be renamed,
              modified or deleted in any subsequent minor Nickel version.

              A dependent function contract, where the argument is also passed
              to the contract of the returned value.

              That is, `some_function | DependentFun Foo Bar` performs the
              same checks as

              ```nickel ignore
              fun arg =>
                let arg_checked = (arg | Foo) in
                (some_function arg_checked | (Bar arg_checked))
              ```
            "%
          = fun Domain Codomain =>
            %contract/custom% (fun label function =>
              'Ok (fun arg =>
                let arg_with_ctr = std.contract.apply Domain label arg in

                function arg_with_ctr
                |> std.contract.apply (Codomain arg_with_ctr) label
              )
            ),

        RangeStep
          | doc m%"
              **Warning**: this is an unstable item. It might be renamed,
              modified or deleted in any subsequent minor Nickel version.

              A contract for a range step, which must be a positive number.

              This contract blames only if the value is a number but is not
              positive: the fact that the value is a number is assumed to be
              checked by an associated static type annotation.
            "%
          =
            %contract/custom% (fun _label value =>
              if %typeof% value == 'Number && value < 0 then
                'Error {
                  message = "invalid range step",
                  notes = ["Expected a positive number, got %{%to_string% value}"],
                }
              else
                'Ok value
            ),

        RangeFun
          | doc m%"
              **Warning**: this is an unstable item. It might be renamed,
              modified or deleted in any subsequent minor Nickel version.

              A function contract which checks that the two first argument are a
              valid range, that is two numbers `start` and `end` such that
              `start <= end`.

              This contract blames only if the arguments are of the right type
              but don't satisfy the tested condition. The type of arguments is
              assumed to be checked by an associated static type annotation. For
              example, if the first argument is a string, this contract silently
              passes.
            "%
          = fun Codomain =>
            let RangeSecond = fun start =>
              %contract/custom% (fun _label value =>
                if %typeof% start == 'Number
                && %typeof% value == 'Number
                && start > value then
                  let start_as_str = %to_string% start in
                  let end_as_str = %to_string% value in

                  'Error {
                    message = "invalid range",
                    notes = [
                      "Expected a range end greater than %{start_as_str} (range start), got %{end_as_str}"
                    ]
                  }
                else
                  'Ok value
              )
            in

            fun label value =>
              %contract/apply%
                (
                  DependentFun
                    Dyn
                    (fun start => RangeSecond start -> Codomain)
                )
                label
                value,

        IndexedArrayFun
          | [| 'Index, 'Split |] -> Dyn
          | doc m%"
              **Warning**: this is an unstable item. It might be renamed,
              modified or deleted in any subsequent minor Nickel version.

              A function contract which checks that the two first argument are,
              in order, a number and an array such that the number is a valid
              index for that array, that is a natural number such that
              `0 <= index <= length array`.

              This contract blames only if the arguments are of the right type
              but don't satisfy the tested condition. The type of arguments is
              assumed to be checked by an associated static type annotation. For
              example, if the first argument is a string, this contract silently
              passes.
            "%
          =
            # capture the `contract.label` module to avoid name collisions with
            # local contract argument `label`
            let label_module = label in
            let attach_message = label_module.with_message "invalid array indexing" in

            let
              ArrayIndexFirst =
                %contract/custom% (fun label value =>
                  if %typeof% value == 'Number then
                    let label =
                      label
                      |> attach_message
                      |> label_module.append_note "Expected array index to be a positive integer, got %{%to_string% value} "
                    in
                    std.contract.check std.number.Nat label value
                  else
                    'Ok value
                ),
              ArrayIndexSecond = fun type min_size =>
                %contract/custom% (fun _label value =>
                  if %typeof% min_size == 'Number && %typeof% value == 'Array then
                    let max_idx =
                      type
                      |> match {
                        'Index => %array/length% value - 1,
                        'Split => %array/length% value
                      }
                    in

                    if min_size > max_idx then
                      let index_as_str = %to_string% min_size in
                      let max_as_str = %to_string% max_idx in
                      let note =
                        if %array/length% value == 0 then
                          "Can't index into an empty array"
                        else
                          "Expected an array index between 0 and %{max_as_str} (included), got %{index_as_str}"
                      in

                      'Error {
                        message = "invalid array indexing",
                        notes = [note],
                      }
                    else
                      'Ok value
                  else
                    'Ok value
                ),
            in

            fun type =>
              DependentFun
                ArrayIndexFirst
                (fun index => ArrayIndexSecond type index -> Dyn),

        ArraySliceFun
          | doc m%"
              **Warning**: this is an unstable item. It might be renamed,
              modified or deleted in any subsequent minor Nickel version.

              A function contract which checks that the three first arguments
              are, in order, two numbers and an array such that the two numbers
              are a valid slice of that array, that is they are natural numbers
              such that: `0 <= n1 <= n2 <= length array`

              This contract blames only if the arguments are of the right type
              but don't satisfy the tested condition. The type of arguments is
              assumed to be checked by an associated static type annotation. For
              example, if the first argument is a string, this contract silently
              passes.
            "%
          =
            # capture the `contract.label` module to avoid name collisions with
            # local contract argument `label`
            let label_module = label in
            let attach_message = label_module.with_message "invalid array slice indexing" in

            let
              SliceIndexFirst =
                %contract/custom% (fun label value =>
                  if %typeof% value == 'Number then
                    let label =
                      label
                      |> attach_message
                      |> label_module.append_note "Expected the array slice start index to be a positive integer, got %{%to_string% value}"
                    in
                    std.contract.check std.number.Nat label value
                  else
                    'Ok value
                ),
              SliceIndexSecond = fun start =>
                %contract/custom% (fun label value =>
                  if %typeof% start == 'Number && %typeof% value == 'Number then
                    if value < start then
                      'Error {
                        message = "invalid array slice indexing",
                        notes = [
                          "Expected the array slice indices to satisfy `start <= end`, but got %{%to_string% start} (start) and %{%to_string% value} (end)"
                        ],
                      }
                    else
                      let label =
                        label
                        |> attach_message
                        |> label_module.append_note "Expected the array slice end index to be a positive integer, got %{%to_string% value}"
                      in
                      std.contract.check std.number.Nat label value
                  else
                    'Ok value
                ),
              ArraySliceArray = fun end_index =>
                %contract/custom% (fun _label value =>
                  if %typeof% end_index == 'Number
                  && %typeof% value == 'Array
                  && end_index > %array/length% value then
                    let index_as_str = %to_string% end_index in
                    let size_as_str = %to_string% (%array/length% value) in

                    'Error {
                      message = "invalid array slice indexing",
                      notes = [
                        "Expected the slice end index to be between 0 and %{size_as_str} (array's length), got %{index_as_str}"
                      ],
                    }
                  else
                    'Ok value
                ),
            in

            DependentFun
              SliceIndexFirst
              (fun start_index =>
                DependentFun
                  (SliceIndexSecond start_index)
                  (fun end_index =>
                    ArraySliceArray end_index -> Dyn
                  )
              ),

        HasField
          | doc m%%"
              **Warning**: this is an unstable item. It might be renamed,
              modified or deleted in any subsequent minor Nickel version.

              A function contract for 2-ary functions which checks that second
              argument is a record that contains the first argument as a field.
              This contract is parametrized by the return type of the function.

              This contract blames only if the arguments are of the right type
              but don't satisfy the tested condition (the first is a string and
              the second is a record). The type of arguments is assumed to be
              checked by an associated static type annotation. For example, if
              the first argument is a number, this contract silently passes.

              ## Example

              ```nickel
              let f | HasField Dyn = fun field record => record."%{field}" in
              (f "foo" { foo = 1, bar = 2 })
              + (f "baz" { foo = 1, bar = 2 })
              # => error: missing field
              ```

              In this example, the first call to `f` won't blame, but the second
              will, as `baz` isn't a field of the record argument.
            "%%
          = fun Codomain =>
            let HasFieldSecond = fun field =>
              %contract/custom% (fun _label value =>
                if %typeof% field == 'String
                && %typeof% value == 'Record
                && !(%record/has_field% field value) then
                  'Error {
                    message = "missing field",
                    notes = [
                      "The record given as an argument lacks the required field `%{field}`."
                    ]
                  }
                else
                  'Ok value
              )
            in

            DependentFun
              Dyn
              (fun field => HasFieldSecond field -> Codomain),
      }
  },

  enum = {
    Tag
      | doc m%"
        Enforces that the value is an enum tag.

        # Examples

        ```nickel multiline
        ('foo | std.enum.Tag)
        # => 'foo

        ('FooBar | std.enum.Tag)
        # => 'FooBar

        ("tag" | std.enum.Tag)
        # => error
        ```
      "%
      = std.contract.from_predicate is_enum_tag,

    Enum
      | doc m%"
        Enforces that the value is an enum (either a tag or a variant).

        # Examples

        ```nickel multiline
        ('Foo | std.enum.Enum)
        # => 'Foo

        ('Bar 5 | std.enum.Enum)
        # => 'Bar 5

        ("tag" | std.enum.Enum)
        # => error
        "%
      =
        %contract/custom% (fun _label value =>
          if std.is_enum value then
            'Ok value
          else
            'Error {}
        ),

    TagOrString
      | doc m%%"
        Accepts both enum tags and strings. Strings are automatically
        converted to an enum tag.

        `TagOrString` is typically used in conjunction with an enum type, to
        accept both actual enum tags and tags represented as strings (e.g.
        coming from a JSON serialization).

        **Warning**: contracts are applied in-order. The pattern described here
        requires that `TagOrString` is applied before the corresponding enum
        contract. Thus, `TagOrString` must appear before the enum  contract in
        the annotations, as in the example below.

        # Examples

        ```nickel
        let Schema = {
          protocol
            | std.enum.TagOrString
            | [| 'http, 'ftp |],
          port
            | Number,
          method
            | std.enum.TagOrString
            | [| 'GET, 'POST |]
        } in
        let serialized =
          m%"
            {"protocol": "http", "port": 80, "method": "GET"}
          "%
          |> std.deserialize 'Json
        in

        serialized | Schema
        ```
      "%%
      =
        %contract/custom% (fun _label value =>
          %typeof% value
          |> match {
            'String => 'Ok (%enum/from_string% value),
            'Enum if !(is_enum_variant value) => 'Ok value,
            _ =>
              'Error {
                message = "expected either a string or an enum tag"
              },
          }
        ),

    is_enum_tag
      : Dyn -> Bool
      | doc m%"
        Checks if a value is an enum tag. Enum variants (applied to an argument)
        aren't considered enum tags.

        # Examples

        ```nickel multiline
        std.enum.is_enum_tag 'foo
        # => true

        std.enum.is_enum_tag 'FooBar
        # => true

        std.enum.is_enum_tag "tag"
        # => false

        std.enum.is_enum_tag ('Foo "arg")
        # => false
        ```
      "%
      = fun value => std.is_enum value && !(%enum/is_variant% value),

    is_enum_variant
      : Dyn -> Bool
      | doc m%"
        Checks if a value is an enum variant. Bare enum tags (not applied to an
        argument) aren't considered enum variants.

        # Examples

        ```nickel multiline
        std.enum.is_enum_variant ('Foo "arg")
        # => true

        std.enum.is_enum_variant ('Http {version = "1.1"})
        # => true

        std.enum.is_enum_variant 'foo
        # => false

        std.enum.is_enum_variant [1, 2, 3]
        # => false
        ```
      "%
      = fun value => %enum/is_variant% value,

    to_tag_and_arg
      | Enum -> { tag | String, arg | optional }
      | doc m%"
        Converts an enum to record with a string tag and an optional argument. If
        the enum is an enum tag, the `arg` field is simply omitted.

        `std.enum.from_tag_and_arg` provides the inverse transformation,
        reconstructing an enum from a string tag and an argument.

        # Examples

        ```nickel multiline
        std.enum.to_tag_and_arg ('Foo "arg")
        # => { tag = "Foo", arg = "arg" }

        std.enum.to_tag_and_arg 'http
        # => { tag = "http" }
        ```
        "%
      = fun enum_value =>
        let tag_string = %to_string% (%enum/get_tag% enum_value) in
        if %enum/is_variant% enum_value then
          {
            tag = tag_string,
            arg = %enum/get_arg% enum_value,
          }
        else
          { tag = tag_string },

    from_tag_and_arg
      | { tag | String, arg | optional } -> Enum
      | doc m%"
        Creates an enum from a string tag and an optional argument. If the `arg`
        field is omitted, a bare enum tag is created.

        `std.enum.to_tag_and_value` provides the inverse transformation,
        extracting a string tag and an argument from an enum.

        # Examples

        ```nickel multiline
        std.enum.from_tag_and_arg { tag = "Foo", arg = "arg" }
        # => ('Foo "arg")

        std.enum.from_tag_and_arg { tag = "http" }
        # => 'http
        ```
        "%
      = fun enum_data =>
        if %record/has_field% "arg" enum_data then
          %enum/make_variant% enum_data.tag enum_data.arg
        else
          %enum/from_string% enum_data.tag,

    map
      | (Dyn -> Dyn) -> Enum -> Enum
      | doc m%"
          Maps a function over an enum variant's argument. If the enum doesn't
          have an argument, it is left unchanged.

          # Examples

          ```nickel multiline
          std.enum.map ((+) 1) ('Foo 42)
          # => 'Foo 43

          std.enum.map (fun x => 1) 'Bar
          # => 'Bar
          ```
        "%
      = fun f enum_value =>
        if %enum/is_variant% enum_value then
          let
            tag = (%to_string% (%enum/get_tag% enum_value)),
            mapped = f (%enum/get_arg% enum_value),
          in

          %enum/make_variant% tag mapped
        else
          enum_value,
  },

  function = {
    id
      : forall a. a -> a
      | doc m%"
        The identity function, that is, `id x` is `x` for any value `x`.

        # Examples

        ```nickel multiline
        std.function.id null
        # => null

        (std.function.id (fun x => x + 1)) 0
        # => 1
        ```
      "%
      = fun x => x,

    compose
      : forall a b c. (b -> c) -> (a -> b) -> (a -> c)
      | doc m%"
        Function composition, from right to left. That is, `compose f g x` is
        `f (g x)`.

        # Examples

        ```nickel
        let f = std.function.compose (fun x => x + 1) (fun x => x / 2)
        in
        f 10
        # => (10 / 2) + 1
        # => 6
        ```
      "%
      = fun g f x => x |> f |> g,

    flip
      : forall a b c. (a -> b -> c) -> b -> a -> c
      | doc m%%"
        Flips the argument order for a two-argument function.

        # Examples

        ```nickel
        std.function.flip (fun x y => "%{x} %{y}") "world!" "Hello,"
        # => "Hello, world!"
        ```
      "%%
      = fun f x y => f y x,

    const
      : forall a b. a -> b -> a
      | doc m%"
        Takes a value and returns the constant function which always returns
        this value. Same as `std.function.first`.

        # Examples

        ```nickel
        let f = std.function.const 5 in
        f 7
        # => 5
        ```
      "%
      = fun x y => x,

    first
      : forall a b. a -> b -> a
      | doc m%"
        Always returns the first argument, ignoring the second. Same as
        `std.function.const`.

        # Examples

        ```nickel
        std.function.first 5 7
        # => 5
        ```
      "%
      = fun x y => x,

    second
      : forall a b. a -> b -> b
      | doc m%"
        Always returns the second argument, ignoring the first.

        # Examples

        ```nickel
        std.function.second 5 7
        # => 7
        ```
      "%
      = fun x y => y,

    pipe
      : forall a. a -> Array (a -> a) -> a
      | doc m%%"
        Applies an array of functions to a value, in order.

        # Examples

        ```nickel multiline
        std.function.pipe 2 [ (+) 2, (+) 3 ]
        # => 7

        std.function.pipe 'World [ std.string.from, fun s => "Hello, %{s}!" ]
        # => "Hello, World!"
        ```
      "%%
      = fun x fs => std.array.fold_left (|>) x fs,
  },

  number = {
    Integer
      | doc m%"
        Enforces that a number is an integer.

        # Examples

        ```nickel multiline
        (1.5 | Integer)
        # => error

        (42 | Integer)
        # => 42
        ```
      "%
      =
        %contract/custom% (fun _label value =>
          if %typeof% value == 'Number then
            if value % 1 == 0 then
              'Ok value
            else
              'Error { message = "expected an integer" }
          else
            'Error { message = "expected a number" }
        ),

    Nat
      | doc m%"
        Enforces that a number is a natural number (including 0).

        # Examples

        ```nickel multiline
        (42 | std.number.Nat)
        # => 42

        (0 | std.number.Nat)
        # => 0

        (-4 | std.number.Nat)
        # => error
        ```
      "%
      =
        %contract/custom% (fun _label value =>
          if %typeof% value == 'Number then
            if value % 1 == 0 && value >= 0 then
              'Ok value
            else
              'Error { message = "expected a natural" }
          else
            'Error { message = "expected a number" }
        ),

    PosNat
      | doc m%"
        Enforces that a number is a strictly positive natural number.

        # Examples

        ```nickel multiline
        (42 | std.number.PosNat)
        # => 42

        (0 | std.number.PosNat)
        # => error

        (-4 | std.number.PosNat)
        # => error
        ```
      "%
      =
        %contract/custom% (fun _label value =>
          if %typeof% value == 'Number then
            if value % 1 == 0 && value > 0 then
              'Ok value
            else
              'Error { message = "expected a positive integer" }
          else
            'Error { message = "expected a number" }
        ),

    NonZero
      | doc m%"
        Enforces that a number is not equal to zero.

        # Examples

        ```nickel multiline
        (1 | std.number.NonZero)
        # => 1

        (0.0 | std.number.NonZero)
        # => error
        ```
      "%
      =
        %contract/custom% (fun _label value =>
          if %typeof% value == 'Number then
            if value != 0 then
              'Ok value
            else
              'Error { message = "value is zero" }
          else
            'Error { message = "expected a number" }
        ),

    is_integer
      : Number -> Bool
      | doc m%"
        Checks if the given number is an integer.

        # Examples

        ```nickel multiline
        std.number.is_integer 42
        # => true

        std.number.is_integer 1.5
        # => false
        ```
      "%
      = fun x => x % 1 == 0,

    compare
      : Number -> Number -> [| 'Lesser, 'Equal, 'Greater |]
      | doc m%"
        Compares two numbers.

        # Examples
        ```nickel
        std.number.compare 1 2
        # => 'Lesser
        ```
      "%
      = fun x y =>
        if x < y then
          'Lesser
        else if x > y then
          'Greater
        else
          'Equal,

    min
      : Number -> Number -> Number
      | doc m%"
        Returns the lower of two numbers.

        # Examples

        ```nickel
        std.number.min (-1337) 42
        # => -1337
        ```
      "%
      = fun x y => if x <= y then x else y,

    max
      : Number -> Number -> Number
      | doc m%"
        Returns the higher of two numbers.

        # Examples

        ```nickel
        std.number.max (-1337) 42
        # => 42
        ```
      "%
      = fun x y => if x >= y then x else y,

    floor
      : Number -> Number
      | doc m%"
        Rounds a number down to the next integer.

        # Examples

        ```nickel multiline
        std.number.floor 42.5
        # => 42

        std.number.floor (-42.5)
        # => -43
        ```
      "%
      = fun x =>
        if x >= 0 then
          x - (x % 1)
        else
          x - 1 - (x % 1),

    abs
      : Number -> Number
      | doc m%"
        Returns the absolute value of a number.

        # Examples

        ```nickel multiline
        std.number.abs (-5)
        # => 5

        std.number.abs 42
        # => 42
        ```
      "%
      = fun x => if x < 0 then -x else x,

    fract
      : Number -> Number
      | doc m%"
        Returns the fractional part of a number.

        # Examples

        ```nickel multiline
        std.number.fract 13.37
        # => 0.37

        std.number.fract 42
        # => 0
        ```
      "%
      = fun x => x % 1,

    truncate
      : Number -> Number
      | doc m%"
        Truncates a number, rounding it towards 0.

        # Examples

        ```nickel multiline
        std.number.truncate (-13.37)
        # => -13

        std.number.truncate 42.5
        # => 42
        ```
      "%
      = fun x => x - (x % 1),

    pow
      : Number -> Number -> Number
      | doc m%"
        `pow x y` returns `x` to the power of `y`.

        # Examples

        ```nickel
        std.number.pow 2 8
        # => 256
        ```

        # Precision

        Nickel numbers are arbitrary precision rationals. If the exponent `y` is
        an integer which fits into a 64-bit signed or unsigned integer (that is, if
        `y` is an integer between `−2^63` and `2^64-1`), the result is computed
        exactly.

        Otherwise, both operands `x` and `y` are converted to the nearest 64 bit
        float (excluding `NaN` and infinity) and the result is computed as a 64 bit
        float. This result is then converted back to a rational. In this case,
        **be aware that both the conversion from rationals to floats, and the
        power operation, might incur rounding errors**.
      "%
      = fun x n => %pow% x n,

    log
      : Number -> Number -> Number
      | doc m%"
        `log x base` returns the logarithm of x with respect to the given base.

        # Examples
        ```nickel ignore
        std.number.log 2 1024
        # => 10

        std.number.log 100 10
        # => 2

        std.number.log 10 std.number.e
        # => 2.302585
        ```
      "%
      = fun x b => %number/log% x b,

    cos
      : Number -> Number
      | doc m%"
        `cos x` returns the cosinus of x.

        # Examples

        ```nickel ignore
        std.number.cos (std.number.pi / 4)
        # => 0.70710678118
        ```
      "%
      = fun x => %number/cos% x,

    sin
      : Number -> Number
      | doc m%"
        `sin x` returns the sinus of x.

        # Examples

        ```nickel ignore
        std.number.sin (std.number.pi / 2)
        # => 1
        ```
      "%
      = fun x => %number/sin% x,

    tan
      : Number -> Number
      | doc m%"
        `tan x` returns the tangent of x.

        # Examples

        ```nickel ignore
        std.number.tan (std.number.pi / 4)
        # => 1
        ```
      "%
      = fun x => %number/tan% x,

    arccos
      : Number -> Number
      | doc m%"
        `arccos x` returns the arccosinus of x.

        # Examples

        ```nickel ignore
        std.number.arccos 0
        # => 1.570796326794897

        std.number.arccos 1
        # => 0
        ```
      "%
      = fun x => %number/arccos% x,

    arcsin
      : Number -> Number
      | doc m%"
        `arcsin x` returns the arccsinus of x.

        # Examples

        ```nickel ignore
        std.number.arcsin 0
        # => 0

        std.number.arcsin 1
        # => 1.570796326794897
        ```
      "%
      = fun x => %number/arcsin% x,

    arctan
      : Number -> Number
      | doc m%"
        `arctan x` returns the arctangent of x.

        # Examples

        ```nickel ignore
        std.number.arctan 1
        # => 0.7853981633974482
        ```
      "%
      = fun x => %number/arctan% x,

    arctan2
      : Number -> Number -> Number
      | doc m%"
        `arctan2 y x returns the four quadrant arctangent of y over x.

        # Examples

        ```nickel ignore
        std.number.arctan2 1 0
        # => 1.570796326794897

        std.number.arctan2 (-0.5) (-0.5)
        # => -2.356194490192345
        ```
      "%
      = fun y x => %number/arctan2% y x,

    sqrt
      : Number -> Number
      | doc m%"
        `sqrt x` return the square root of x.

        # Examples

        ```nickel
        std.number.sqrt 4
        # => 2
        ```
      "%
      = fun x => pow x (1 / 2),

    pi
      : Number
      | doc m%"
        The π mathematical constant.
      "%
      = 3.14159265358979323846,

    e
      : Number
      | doc m%"
        Euler's number, the e mathematical constant.
      "%
      = 2.7182818284590452354,
  },

  package
    | doc m%"
      Contracts supporting Nickel's forthcoming package management feature.

      These contracts are not yet stabilized: they aren't part of the backward
      compatibility policy and are subject to change at any point in time.
    "%
    =
      let
        semver_re = m%"^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$"%,
        # Just the major.minor.patch part, with minor and patch being optional.
        partial_semver_re = m%"^(0|[1-9]\d*)(?:\.(0|[1-9]\d*))?(?:\.(0|[1-9]\d*))?$"%,
        # An exact version constraint. This one is required to have minor and patch versions, and it's allowed to have a prerelease.
        semver_equals_req_re = m%"^=(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(\.(0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?$"%,
        # The regex for nickel identifiers.
        ident_re = "^_*[a-zA-Z][_a-zA-Z0-9-']*$",
      in
      let
        semver_req_re = "(%{partial_semver_re})|(%{semver_equals_req_re})",
        # TODO: add a std.record.FieldMatches contract and rewrite this in terms of that
        IdentKeys =
          std.contract.from_validator (fun dict =>
            let bad_name = std.string.is_match ident_re |> std.function.compose (fun x => !x) in
            let bad_names = std.record.fields dict |> std.array.filter bad_name |> std.array.map (fun s => m%""%{s}""%) in
            std.array.length bad_names
            |> match {
              0 => 'Ok,
              1 => 'Error { message = "invalid package name %{std.array.at 0 bad_names}", notes = ["package names must be valid identifiers"] },
              _ => 'Error { message = "invalid package names %{std.string.join "," bad_names}", notes = ["package names must be valid identifiers"] },
            }
          )
      in
      {
        structured = {
          Semver
            | doc m%"
              A contract for semantic version ("semver") identifiers, structured as a record.

              See also `std.package.Semver`, which can produce one of these by parsing a string.
            "%
            = {
              major
                | number.Nat
                | doc "The major version number.",
              minor
                | number.Nat
                | doc "The minor version number.",
              patch
                | number.Nat
                | doc "The patch version.",
              pre
                | String
                | doc m%"
                  The prerelease identifier.

                  When comparing  versions for compatibility, non-empty prerelease
                  strings only match one another if they are exactly equal. See
                  `std.package.SemverReq` for more details
                "%
                | optional,
              build
                | String
                | doc m%"
                  The build metadata.

                  This is completely ignored while comparing versions for compatibility.
                "%
                | optional,
            },
          ExactSemverReq
            | doc m%"
              A contract for exact semantic version ("semver") requirements, structured as a record.

              This differs from `std.package.structured.Semver` in that it lacks a build metadata field.
            "%
            = {
              major
                | number.Nat
                | doc "The major version number.",
              minor
                | number.Nat
                | doc "The minor version number.",
              patch
                | number.Nat
                | doc "The patch version.",
              pre
                | String
                | doc "The prerelease identifier."
                | optional,
            },
          SemverPrefix
            | doc m%"
              A contract for semantic version ("semver") prefixes, structured as a record.
            "%
            = {
              major
                | number.Nat
                | doc "The major version number.",
              minor
                | number.Nat
                | doc "The minor version number."
                | optional,
              patch
                | number.Nat
                | doc "The patch version."
                | optional,
            },
          SemverReq = [| 'Compatible SemverPrefix, 'Exact ExactSemverReq |]
        },
        Semver
          | doc m%"
            A contract for semantic version ("semver") identifiers.

            This is a normalizing contract, which accepts either a string to be parsed
            or a record. If a string is provided, it will be parsed into a record (in
            the `std.package.structured.Semver` format).

            # Examples

            ```nickel multiline
            "1.2.0-pre1" | std.package.Semver
            # => { major = 1, minor = 2, patch = 0, pre = "pre1", }

            { major = 1, minor = 2, patch = 0 } | std.package.Semver
            # => { major = 1, minor = 2, patch = 0, }

            { major = 1, minor = 2 } | std.package.Semver
            # => error: missing definition

            "1.foo" | std.package.Semver
            # => error: contract broken by a value
            ```
          "%
          =
            %contract/custom% (fun label value =>
              %typeof% value
              |> match {
                'Record => std.contract.check structured.Semver label value,
                'String =>
                  let matches = string.find semver_re value in
                  if matches.index == -1 then
                    'Error { message = "invalid semver" }
                  else
                    let gs = matches.groups in
                    let
                      pre_ = gs |> array.at 3,
                      build_ = gs |> array.at 4,
                    in
                    let val =
                      {
                        major = gs |> array.at 0 |> string.to_number,
                        minor = gs |> array.at 1 |> string.to_number,
                        patch = gs |> array.at 2 |> string.to_number,
                      }
                      & (if pre_ != "" then { pre = pre_ } else {})
                      & (if build_ != "" then { build = build_ } else {})
                    in
                    std.contract.check structured.Semver label val,
                _ => 'Error { message = "expected a string or a record" }
              }
            ),
        SemverPrefix
          | doc m%"
            A contract for semantic version ("semver") prefixes.

            This prefix must contain a major version number. It may contain a minor
            version and a patch version, but it must not contain a prerelease version
            or build metadata.

            This is a normalizing contract, which accepts either a string to be parsed
            or a record. If a string is provided, it will be parsed into a record (in
            the `std.package.structured.SemverPrefix` format).

            # Examples

            ```nickel multiline
            "1.2" | std.package.SemverPrefix
            # => { major = 1, minor = 2 }

            { major = 1, minor = 2 } | std.package.SemverPrefix
            # => { major = 1, minor = 2 }

            "1.foo" | std.package.SemverPrefix
            # => error: contract broken by a value
            ```
          "%
          =
            %contract/custom% (fun label value =>
              %typeof% value
              |> match {
                'Record => std.contract.check structured.SemverPrefix label value,
                'String =>
                  let matches = string.find partial_semver_re value in
                  if matches.index == -1 then
                    'Error { message = "invalid semver" }
                  else
                    let gs = matches.groups in
                    let
                      minor_ = gs |> array.at 1,
                      patch_ = gs |> array.at 2,
                    in
                    let val =
                      { major = gs |> array.at 0 |> string.to_number }
                      & (if minor_ == "" then {} else { minor = string.to_number minor_ })
                      & (if patch_ == "" then {} else { patch = string.to_number patch_ })
                    in
                    std.contract.check structured.SemverPrefix label val,
                _ => 'Error { message = "expected a string or a record" }
              }
            ),
        ExactSemverReq
          | doc m%"
            A contract for exact semantic version ("semver") requirements, structured as a record.

            An exact semver requirement looks like "=1.2.3" or "=1.2.3-rc1": it starts with an
            equals sign, must contain major, minor, and patch versions, and may contain a
            prerelease specifier.

            This is a normalizing contract, which accepts either a string to be parsed
            or a record. If a string is provided, it will be parsed into a record (in
            the `std.package.structured.ExactSemverReq` format).
          "%
          =
            %contract/custom% (fun label value =>
              %typeof% value
              |> match {
                'Record => std.contract.check structured.ExactSemverPrefix label value,
                'String =>
                  let matches = string.find semver_equals_req_re value in
                  if matches.index == -1 then
                    'Error { message = "invalid semver" }
                  else
                    let gs = matches.groups in
                    let pre_ = array.at 3 gs in
                    let val =
                      {
                        major = gs |> array.at 0 |> string.to_number,
                        minor = gs |> array.at 1 |> string.to_number,
                        patch = gs |> array.at 2 |> string.to_number,
                      }
                      & (if pre_ == "" then {} else { pre = pre_ })
                    in
                    std.contract.check structured.ExactSemverReq label val,
                _ => 'Error { message = "expected a string or a record" }
              }
            ),
        SemverReq
          | doc m%"
            A contract for semantic version ("semver") requirements.

            This is a normalizing contract, which accepts either a string to be parsed
            or a record. If a string is provided, it will be parsed into a record (in
            the `std.package.structured.SemverReq` format).

            Nickel supports two kinds of requirements: semver-compatible
            requirements and exact version requirements. Semver-compatible
            requirements take the form "major.minor.patch", where minor and patch
            are optional. Their semantics are:

            - "1.2.3" will match all versions having major version 1, minor version 2,
              and patch version at least 3.
            - "1.2" will match all versions having major version 1 and minor version
              at least 2.
            - "1" will match all versions having major version 1.
            - a semver-compatible requirement will never match a prerelease version.

            Exact version requirements take the form "=major.minor.patch-pre", where
            the prerelease tag is optional, but major, minor, and patch are all required.

            # Examples

            ```nickel multiline
            "1.2" | SemverReq
            # => 'Compatible { major = 1, minor = 2 }

            "=1.2" | SemverReq
            # => error: contract broken by a value

            "1.2.0" | SemverReq
            # => 'Compatible { major = 1, minor = 2, patch = 0 }

            "=1.2.0" | SemverReq
            # => 'Exact { major = 1, minor = 2, patch = 0, }

            "1.2.0-pre1" | SemverReq
            # => error: contract broken by a value

            "=1.2.0-pre1" | SemverReq
            # => 'Exact { major = 1, minor = 2, patch = 0, pre = "pre1", }
            ```
          "%
          =
            %contract/custom% (fun label value =>
              %typeof% value
              |> match {
                'Record => std.contract.check structured.SemverReq label value,
                'String =>
                  if string.is_match semver_equals_req_re value then
                    std.contract.check ExactSemverReq label value
                    |> match {
                      'Ok v => 'Ok ('Exact v),
                      'Error e => 'Error e,
                    }
                  else
                    std.contract.check SemverPrefix label value
                    |> match {
                      'Ok v => 'Ok ('Compatible v),
                      'Error e => 'Error e,
                    },
                _ => 'Error { message = "expected a string or a record" }
              }
            ),

        GitId
          | doc m%"
            A full git commit id. That is, a 160-bit SHA-1 hash as a lower-case hexadecimal string.

            # Examples

            ```nickel multiline
            "0c0a82aa4a05cd84ba089bdba2e6a1048058f41b" | GitId
            # =>  "0c0a82aa4a05cd84ba089bdba2e6a1048058f41b"
            ```
          "%
          =
            let hex_chars = std.string.is_match "^[a-f0-9]*$" in
            let value_validator =
              std.contract.from_validator (fun s =>
                if (std.string.length s) != 40 then
                  let len = std.string.length s |> std.to_string in
                  'Error { message = "expected 40 characters, got %{len}", notes = ["use the full commit id; short ids are not supported"] }
                else if !(hex_chars s) then
                  'Error { message = "contains an invalid character", notes = ["only 0-9 and a-f are allowed"] }
                else
                  'Ok
              )
            in std.contract.Sequence [ String, value_validator],

        GitDependency
          | doc m%"
            A contract identifying a dependency that can be fetched from a git repository.
          "%
          = {
            url
              | String
              | doc m%"
                The url of a git repository.

                This supports local file paths, https urls like `https://example.com/example-repo`,
                and ssh urls like `user@example.com:repo`.
              "%,
            ref
              | [| 'Head, 'Branch String, 'Tag String, 'Commit std.package.GitId |]
              | optional
              | doc m%"
                The git ref to fetch from the repository.

                If not provided, defaults to 'Head.
              "%,
            path
              | String
              | optional
              | doc m%"
                The path of the nickel package within the git repository. If omitted, the nickel package
                is at the root of the git repository.
              "%,
          },

        IndexDependency
          | doc m%"
            A contract identifying a dependency that can be fetched from a package index.
          "%
          = {
            package
              | String
              | doc m%"
                The dependency's identifier within the package index, in the format "github/<organization>/<repository>".
              "%,
            version
              | SemverReq
              | doc m%"
                The required version of the package.

                Nickel supports two kinds of requirements: semver-compatible
                requirements and exact version requirements. Semver-compatible
                requirements take the form "major.minor.patch", where minor and patch
                are optional. Their semantics are:

                - "1.2.3" will match all versions having major version 1, minor version 2,
                  and patch version at least 3.
                - "1.2" will match all versions having major version 1 and minor version
                  at least 2.
                - "1" will match all versions having major version 1.
                - a semver-compatible requirement will never match a prerelease version.

                Exact version requirements take the form "=major.minor.patch-pre", where
                the prerelease tag is optional, but major, minor, and patch are all required.
              "%,
          },

        Manifest
          | doc m%"
            A contract for a Nickel package manifest.

            # Example

            ```nickel
            {
              name = "my-package",
              version = "1.0.0",
              minimal_nickel_version = "1.10",
              authors = ["Me <me@example.com>"],
              description = "My great package",

              dependencies = {
                my_local_dep = 'Path "../somewhere",
                git_dep = 'Git { url = "https://example.com/repo", ref = 'Tag "v1.0" },
                index_dep = 'Index { package = "github/nickel-lang/example", version = "1.0" },
              },
            } | std.package.Manifest
            ```
          "%
          = {
            name
              | String
              | doc m%"
                The name of this package.
              "%,

            version
              | String
              | Semver
              | doc m%"
                The version of this package.

                Any semantic version is accepted, but the build metadata field has no effect when matching versions.
              "%,

            minimal_nickel_version
              | String
              | SemverPrefix
              | doc m%"
                The minimal nickel version required for this package.
              "%,

            authors
              | Array String
              | doc m%"
                The authors of this package.
              "%,

            description
              | String
              | optional
              | doc m%"
                A description of this package.
              "%,

            keywords
              | Array String
              | optional
              | doc m%"
                A list of keywords to help people find this package.
              "%,

            license
              | String
              | optional
              | doc m%"
                The name of the license that this package is available under.

                This is a completely free-form string, but some tooling may impose
                restrictions. For example, if you want to publish your package in
                Nickel's global package registry, the license field needs to be a
                valid SPDX license expression that allows redistribution.
              "%,

            dependencies
              | {
                _ : [|
                  'Path String,
                  'Git std.package.GitDependency,
                  'Index std.package.IndexDependency,
                |]
              }
              | IdentKeys
              | doc m%"
                A dictionary of package dependencies, keyed by the name that this package uses to refer to them locally.
              "%
              | default
              = {},
          },
      },

  record = {
    map
      : forall a b. (String -> a -> b) -> { _ : a } -> { _ : b }
      | doc m%"
        Maps a function over every field of a record. The function is passed the
        name and value of each field.

        # Examples

        ```nickel multiline
        std.record.map (fun s x => s) { hi = 2 }
        # => { hi = "hi" }

        std.record.map (fun s x => x + 1) { hello = 1, world = 2 }
        # => { hello = 2, world = 3 }
        ```
      "%
      = fun f r => %record/map% r f,

    fields
      : forall a. { _ : a } -> Array String
      | doc m%"
        Returns an array containing the names of all the fields of a record.

        # Empty optional fields

        By default, `fields` ignores optional fields without definition. An
        empty optional field won't be listed in the result.

        Use `std.record.fields_with_opts` for a version which doesn't ignore
        empty optional fields.

        # Examples

        ```nickel multiline
        std.record.fields { one = 1, two = 2 }
        # => [ "one", "two" ]

        std.record.fields { one = 1, two = 2, three_opt | optional }
        # => [ "one", "two" ]
        ```
      "%
      = fun r => %record/fields% r,

    fields_with_opts
      : forall a. { _ : a } -> Array String
      | doc m%"
        Returns an array containing the names of all the fields of a record.

        Same as `std.record.fields`, but doesn't ignore empty optional fields.

        # Examples

        ```nickel multiline
        std.record.fields_with_opts { one = 1, two = 2 }
        # => [ "one", "two" ]

        std.record.fields_with_opts { one = 1, two = 2, three_opt | optional }
        # => [ "one", "three_opt", "two" ]
        ```
      "%
      = fun r => %record/fields_with_opts% r,

    values
      : forall a. { _ : a } -> Array a
      | doc m%"
        Returns an array containing the values of all the fields of a record.

        # Examples

        ```nickel
        std.record.values { one = 1, world = "world" }
        # => [ 1, "world" ]
        ```
      "%
      = fun r => %record/values% r,

    has_field
      : forall a. String -> { _ : a } -> Bool
      | doc m%"
        Given a string, checks if a record contains a field of that name.

        # Empty optional fields

        By default, `has_field` ignores optional fields without definition.

        Use `std.record.has_field_with_opts` for a version which doesn't ignore
        empty optional fields.

        # Examples

        ```nickel multiline
        std.record.has_field "hello" { one = 1, two = 2 }
        # => false

        std.record.has_field "one" { one = 1, two = 2 }
        # => true

        std.record.has_field "two_opt" { one, two_opt | optional }
        # => false

        ({ one = 1 } | {one, two_opt | optional })
        |> std.record.has_field "two_opt"
        # => false
        ```
      "%
      = fun field r => %record/has_field% field r,

    has_field_with_opts
      : forall a. String -> { _ : a } -> Bool
      | doc m%"
        Given a string, checks if a record contains a field of that name.

        Same as `std.record.has_field`, but doesn't ignore empty optional
        fields.

        # Examples

        ```nickel multiline
        std.record.has_field_with_opts "hello" { one = 1, two = 2 }
        # => false

        std.record.has_field_with_opts "one" { one = 1, two = 2 }
        # => true

        std.record.has_field_with_opts "two_opt" { one, two_opt | optional }
        # => true

        ({ one = 1 } | {one, two_opt | optional })
        |> std.record.has_field_with_opts "two_opt"
        # => true
      "%
      = fun field r => %record/has_field_with_opts% field r,

    get
      : forall a. String -> { _ : a } -> a
      | std.contract.unstable.HasField Dyn
      | doc m%%"
        Returns the field of a record with the given name.

        `std.record.get field record` is just `record."%{field}"`. The latter
        form is more idiomatic and should be generally preferred.

        However, `std.record.get` can come in handy when a proper function is
        expected, typically in a sequence of operations chained with the reverse
        application operator `|>`.

        Trying to extract a field which doesn't exist will result in a contract
        error.

        # Examples

        ```nickel multiline
        std.record.get "one" { one = 1, two = 2 }
        # => 1

        { one = 1, two = 2, string = "three"}
        |> std.record.to_array
        |> std.array.filter (fun { field, value } => std.is_number value)
        |> std.record.from_array
        |> std.record.get "two"
        # => 2
        ```
      "%%
      = fun field r => r."%{field}",

    get_or
      : forall a. String -> a -> { _ : a } -> a
      | doc m%%"
        Returns the field of a record with the given name, or the default value
        if either there is no such field or if this field doesn't have a
        definition.

        # Examples

        ```nickel multiline
        std.record.get_or "tree" 3 { one = 1, two = 2 }
        # => 3

        std.record.get_or "one" 11 { one = 1, two = 2 }
        # => 1

        std.record.get_or "value" "default" { tag = 'Hello, value }
        # => "default"
        ```
      "%%
      = fun field default_value record =>
        if %record/has_field% field record
        && %record/field_is_defined% field record then
          record."%{field}"
        else
          default_value,

    insert
      : forall a. String -> a -> { _ : a } -> { _ : a }
      | doc m%%"
          Inserts a new field into a record. `insert` doesn't mutate the original
          record but returns a new one instead.

          # Preconditions

          The field must not exist in the initial record, otherwise `insert`
          will fail. If the field might already exist, use `std.record.update`
          instead.

          # Empty optional fields

          By default, `insert` ignores optional fields without definition. The
          precondition above doesn't apply to empty optional fields, and
          `insert` will silently erase an existing empty optional field.

          Use `std.record.insert_with_opts` for a version which doesn't ignore
          empty optional fields.

          # Examples

          ```nickel multiline
          std.record.insert "foo" 5 { bar = "bar" }
          # => { foo = 5, bar = "bar" }

          {}
          |> std.record.insert "file.txt" "data/text"
          |> std.record.insert "length" (10*1000)
          # => {"file.txt" = "data/text", "length" = 10000}

          std.record.insert "already_there_opt" 0 {already_there_opt | optional}
          # => {already_there_opt = 0}

          std.record.insert "already_there" 0 {already_there = 1}
          # => error
          ```
        "%%
      = fun field content r => %record/insert% field (%record/freeze% r) content,

    insert_with_opts
      : forall a. String -> a -> { _ : a } -> { _ : a }
      | doc m%%"
          Inserts a new field into a record. `insert_with_opts` doesn't mutate the
          original record but returns a new one instead.

          Same as `std.record.insert`, but doesn't ignore empty optional fields.

          # Preconditions

          The field must not exist in the initial record, otherwise `insert`
          will fail. If the field might already exist, use `std.record.update`
          instead.

          # Examples

          ```nickel multiline
          std.record.insert_with_opts "foo" 5 { bar = "bar" }
          # => { foo = 5, bar = "bar" }

          {}
          |> std.record.insert_with_opts "file.txt" "data/text"
          |> std.record.insert_with_opts "length" (10*1000)
          # => {"file.txt" = "data/text", "length" = 10000}

          std.record.insert_with_opts "already_there_optional" 0 {already_there | optional}
          # => { already_there_optional = 0 }

          std.record.insert_with_opts "already_there" 0 {already_there = 1}
          # => error
          ```
        "%%
      = fun field content r => %record/insert_with_opts% field (%record/freeze% r) content,

    remove
      : forall a. String -> { _ : a } -> { _ : a }
      | doc m%"
          Removes a field from a record. `remove` doesn't mutate the original
          record but returns a new one instead.

          # Preconditions

          The field to remove must be present in the record, or `remove` will
          fail.

          # Empty optional fields

          By default, `remove` ignores optional fields. The precondition above
          doesn't apply to an empty optional field and `remove` will fail with a
          missing field error when trying to remove an empty optional field.

          Use `std.record.remove_with_opts` for a version which doesn't ignore
          empty optional fields.

          # Examples

          ```nickel multiline
          std.record.remove "foo" { foo = "foo", bar = "bar" }
          # => { bar = "bar" }

          std.record.remove "foo_opt" {foo_opt | optional}
          # => error

          std.record.remove "foo" { bar = "bar" }
          # => error
          ```
        "%
      = fun field r => %record/remove% field (%record/freeze% r),

    remove_with_opts
      : forall a. String -> { _ : a } -> { _ : a }
      | doc m%"
          Removes a field from a record. `remove` doesn't mutate the original
          record but returns a new one instead.

          Same as `std.record.remove`, but doesn't ignore empty optional fields.

          # Preconditions

          The field to remove must be present in the record, or `remove` will
          fail.

          # Examples

          ```nickel multiline
          std.record.remove_with_opts "foo" { foo = "foo", bar = "bar" }
          # => { bar = "bar" }

          std.record.remove_with_opts "foo_opt" {foo_opt | optional}
          # => {}

          std.record.remove_with_opts "foo" { bar = "bar" }
          # => error
          ```
        "%
      = fun field r => %record/remove_with_opts% field (%record/freeze% r),

    update
      : forall a. String -> a -> { _ : a } -> { _ : a }
      | doc m%"
          Updates a field of a record with a new value. `update` doesn't mutate the
          original record but returns a new one instead. If the field to update is absent
          from the given record, `update` adds it.

          # Empty optional fields

          `update` works the same way for empty optional fields and other fields.
          `update` guarantees that it can always replace a field with a new
          value, no matter the nature of the field.

          # Examples

          ```nickel multiline
          std.record.update "foo" 5 { foo = "foo", bar = "bar" }
          # => { foo = 5, bar = "bar" }

          std.record.update "foo" 5 { bar = "bar" }
          # => { foo = 5, bar = "bar" }

          std.record.update "foo_opt" 5 {foo_opt | optional}
          # => {foo_opt = 5}
          ```

          # Overriding

          As opposed to overriding a value with the merge operator `&`, `update`
          will only change the specified field and won't automatically update the other
          fields which depend on it:

          ```nickel multiline
          { foo = bar + 1, bar | default = 0 } & { bar = 1 }
          # => { foo = 2, bar = 1 }

          std.record.update "bar" 1 {foo = bar + 1, bar | default = 0 }
          # => { foo = 1, bar = 1 }
          ```
        "%
      = fun field content r =>
        let r = %record/freeze% r in
        let r =
          if %record/has_field% field r then
            %record/remove% field r
          else
            r
        in
        %record/insert% field r content,

    map_values
      : forall a b. (a -> b) -> { _ : a } -> { _ : b }
      | doc m%"
        Maps a function over the values of all the fields of a record.
        `map_values f` is the same as `std.record.map (fun _field => f)`.

        # Examples

        ```nickel multiline
        std.record.map_values (fun x => x + 1) { hi = 2 }
        # => { hi = 3 }

        std.record.map_values (fun x => x + 1) { hello = 1, world = 2 }
        # => { hello = 2, world = 3 }
        ```
      "%
      = fun f => map (fun _field => f),

    to_array
      : forall a. { _ : a } -> Array { field : String, value : a }
      | doc m%"
        Converts a record to an array of key-value pairs.

        # Examples

        ```nickel
        std.record.to_array { hello = "world", foo = "bar" }
        # => [ { field = "foo", value = "bar" }, { field = "hello", value = "world" } ]
        ```
      "%
      = fun record =>
        record
        |> fields
        |> std.array.map (fun field' => { field = field', value = record."%{field'}" }),

    from_array
      : forall a. Array { field : String, value : a } -> { _ : a }
      | doc m%"
        Converts an array of key-value pairs into a record. The field names in
        the input array must be distinct.

        # Examples

        ```nickel
        std.record.from_array [
          { field = "hello", value = "world" },
          { field = "foo", value = "bar" }
        ]
        # => { hello = "world", foo = "bar" }
        ```
      "%
      = fun bindings =>
        bindings
        |> std.array.fold_left (fun accum { field, value } => %record/insert% "%{field}" accum value) {},

    is_empty
      : forall a. { _ : a } -> Bool
      | doc m%"
        Checks whether a record is empty.

        # Examples

        ```nickel multiline
        std.record.is_empty {}
        # => true

        std.record.is_empty { foo = 1 }
        # => false
        ```
      "%
      = (==) {},

    merge_all
      : Array { _ : Dyn } -> { _ : Dyn }
      | doc m%"
        Merges an array of records.

        # Examples

        ```nickel
        std.record.merge_all [ { foo = 1 }, { bar = 2 } ]
        # => { foo = 1, bar = 2 }
        ```
      "%
      = fun rs => (std.array.fold_left (&) {} (rs | Array Dyn)) | { _ : Dyn },

    filter
      : forall a. (String -> a -> Bool) -> { _ : a } -> { _ : a }
      | doc m%"
        `filter f r` returns a record containing all fields from `r` for which
        `f` returns true. The function `f` is passed the name and value of each field
        to make a decision.

        # Examples

        ```nickel
        std.record.filter (fun _name x => x % 2 == 0) { even = 2, odd = 3 }
        # => { even = 2 }
        ```
      "%
      = fun f record =>
        record
        |> to_array
        |> std.array.filter (fun { field, value } => f field value)
        |> from_array,

    apply_on
      : forall a b. String -> (a -> a -> b) -> { _ : a } -> { _ : a } -> b
      | doc m%"
        `apply_on field f a b` is equivalent to `f a."${field}" b."${field}"`
        Useful for comparisons.

        # Examples

        ```nickel multiline
        std.record.apply_on "name" std.string.compare { name = "Alice", age = 27 } { name = "Bob", age = 23 }
        # => 'Lesser

        std.record.apply_on "age" std.number.compare { name = "Alice", age = 27 } { name = "Bob", age = 23 }
        # => 'Greater
        ```
        "%
      = fun field f a b => f a."%{field}" b."%{field}",

    length
      : forall a. { _ : a } -> Number
      | doc m%"
          Returns the number of fields in a record. This count doesn't include
          fields both marked as optional and without a definition.

          Because of the need to filter empty optional fields, the cost of
          `length` is linear in the size of the record.
        "%
      = fun record =>
        record
        |> fields
        |> std.array.length,

    freeze
      : forall a. { _ : a } -> { _ : a }
      | doc m%"
        Freezes a recursive record:

        - The values of recursive fields are frozen at the current values, and
          the dependencies are forgotten. **This means that subsequent overrides
          through merging will only affect the overriden fields, but will no
          longer update reverse dependencies**.
        - All lazy pending contracts accumulated through merging and annotations
          will be applied and flushed. **This means that the contracts of this
          record won't propagate through future merges anymore after freezing**.

        Note that the documentation and the priority of the fields aren't
        affected.

        `freeze` is used internally in the stdlib when applying dictionary
        operations, such as `std.record.insert` or `std.record.remove`. Indeed,
        interleaving dictionary operations and recursive records can lean to
        surprising and unintuitive results. The Nickel stdlib tries to avoid
        using records in both recursive and dictionary modes at the same time.

        Freezing is idempotent: freezing an already frozen record has no effect.

        # Examples

        ```nickel multiline
        let frozen = std.record.freeze { x = "old", y = x } in
        frozen & { x | force = "new" }
        # => { x = "new", y = "old" }

        let frozen = std.record.freeze { x | Number = 1 } in
        frozen & { x | force = "bypass number contract" }
        # => { x = "bypass number contract" }
        ```
      "%
      = fun record => %record/freeze% record,
  },

  string = {
    BoolLiteral
      | doc m%"
        Contract for a string representation of a boolean, namely `true` or
        `false`.

        # Examples

        ```nickel multiline
        ("true" | std.string.BoolLiteral)
        # => "true"

        ("hello" | std.string.BoolLiteral)
        # => error

        (true | std.string.BoolLiteral)
        # => error
        ```
      "%
      = fun l s =>
        if %typeof% s == 'String then
          if s == "true" then
            "true"
          else if s == "false" then
            "false"
          else
            %blame% (%label/with_message% "expected \"true\" or \"false\", got %{s}" l)
        else
          %blame% (%label/with_message% "not a string" l),

    NumberLiteral
      | doc m%"
        Contract for a string representation of a numerical value.

        # Examples

        ```nickel multiline
        ("+1.2" | std.string.NumberLiteral)
        # => "+1.2"

        ("5" | std.string.NumberLiteral)
        # => "5"

        (42 | std.string.NumberLiteral)
        # => error
        ```
      "%
      =
        let pattern = m%"^[+-]?(\d+(\.\d*)?(e[+-]?\d+)?|\.\d+(e[+-]?\d+)?)$"% in
        let is_num_literal = %string/is_match% pattern in

        %contract/custom% (fun _label value =>
          if %typeof% value == 'String then
            if is_num_literal value then
              'Ok value
            else
              'Error { message = "invalid number literal" }
          else
            'Error { message = "expected a string" }
        ),

    Character
      | doc m%"
        Contract for a character, i.e. a string of length 1.

        # Examples

        ```nickel multiline
        ("e" | std.string.Character)
        # => "e"

        ("#" | std.string.Character)
        # => "#"

        ("" | std.string.Character)
        # => error

        (1 | std.string.Character)
        # => error
        ```
      "%
      =
        %contract/custom% (fun _label value =>
          if %typeof% value == 'String then
            if length value == 1 then
              'Ok value
            else
              'Error { message = "length different than one" }
          else
            'Error { message = "expected a string" }
        ),

    Stringable
      | doc m%"
        Enforces that the value is convertible to a string via
        `std.to_string` or `std.string.from`. Accepted values are:

        - Numbers
        - Booleans
        - Strings
        - Enum tags
        - `null`

        For string representations of more complex values, see `std.serialize`.

        # Examples

        ```nickel multiline
        ('Foo | std.string.Stringable)
        # => 'Foo

        (false | std.string.Stringable)
        # => false

        ("bar" ++ "foo" | std.string.Stringable)
        # => "barfoo"

        ({foo = "baz"} | std.string.Stringable)
        # => error
        ```
      "%
      =
        %contract/custom% (fun _label value =>
          let type = std.typeof value in

          if value == null
          || type == 'Number
          || type == 'Bool
          || type == 'String
          # note that `type == 'Enum` isn't sufficient, as it includes enum
          # variants
          || std.enum.is_enum_tag value then
            'Ok value
          else
            'Error {}
        ),

    NonEmpty
      | doc m%"
        Contract for a non-empty string.

        # Examples

        ```nickel multiline
        ("" | std.string.NonEmpty)
        # => error

        ("hi!" | std.string.NonEmpty)
        # => "hi!"

        (42 | std.string.NonEmpty)
        # => error
        ```
      "%
      =
        %contract/custom% (fun _label value =>
          if %typeof% value == 'String then
            if %string/length% value > 0 then
              'Ok value
            else
              'Error { message = "empty string" }
          else
            'Error { message = "not a string" }
        ),

    join
      : String -> Array String -> String
      | doc m%"
        Joins an array of strings with a given separator.

        # Examples

        ```nickel multiline
        std.string.join ", " [ "Hello", "World!" ]
        # => "Hello, World!"

        std.string.join ";" ["I'm alone"]
        # => "I'm alone"

        std.string.join ", " []
        # => ""
        ```
      "%
      = fun sep fragments =>
        let length = %array/length% fragments in
        if length == 0 then
          ""
        else
          let
            first = %array/at% fragments 0,
            rest =
              %array/slice% 1 length fragments
              |> std.array.fold_left (fun acc s => acc ++ sep ++ s) "",
          in
          first ++ rest,

    split
      : String -> String -> Array String
      | doc m%"
        Splits a string based on a separator string.

        Note that this function never splits up Unicode extended grapheme
        clusters, even in cases where the sought value exists within one.

        # Examples

        ```nickel multiline
        std.string.split "," "1,2,3"
        # => [ "1", "2", "3" ]

        std.string.split "." "1,2,3"
        # => [ "1,2,3" ]
        ```
      "%
      = fun sep s => %string/split% s sep,

    trim
      : String -> String
      | doc m%"
        Trims whitespace from the start and end of a string.

        # Examples

        ```nickel multiline
        std.string.trim " hi  "
        # => "hi"

        std.string.trim "1   2   3   "
        # => "1   2   3"
        ```
      "%
      = fun s => %string/trim% s,

    characters
      : String -> Array String
      | doc m%"
        Separates a string into its individual Unicode extended grapheme clusters.

        # Examples

        ```nickel
        std.string.characters "Hello"
        # => [ "H", "e", "l", "l", "o" ]
        ```
      "%
      = fun s => %string/chars% s,

    uppercase
      : String -> String
      | doc m%"
        Returns the uppercase version of a string. Unicode extended grapheme
        clusters without an uppercase version are left untouched.

        # Examples

        ```nickel multiline
        std.string.uppercase "a"
        # => "A"

        std.string.uppercase "æ"
        # => "Æ"

        std.string.uppercase "hello.world"
        # => "HELLO.WORLD"
        ```
      "%
      = fun s => %string/uppercase% s,

    lowercase
      : String -> String
      | doc m%"
        Returns the lowercase version of a string. Unicode extended grapheme
        clusters without a lowercase version are left untouched.


        # Examples

        ```nickel multiline
        std.string.lowercase "A"
        # => "a"

        std.string.lowercase "Æ"
        # => "æ"

        std.string.lowercase "HELLO.WORLD"
        # => "hello.world"
        ```
      "%
      = fun s => %string/lowercase% s,

    contains
      : String -> String -> Bool
      | doc m%"
        Checks if the first string is part of the second string.

        Note that this function returns false if the sought string exists entirely inside
        or straddles Unicode extended grapheme clusters.

        # Examples

        ```nickel multiline
        std.string.contains "cde" "abcdef"
        # => true

        std.string.contains "" "abcdef"
        # => true

        std.string.contains "ghj" "abcdef"
        # => false
        ```
      "%
      = fun subs s => %string/contains% s subs,

    compare
      : String -> String -> [| 'Lesser, 'Equal, 'Greater |]
      | doc m%"
        Compares two strings lexicographically.

        # Examples

        ```nickel multiline
        std.string.compare "abc" "def"
        # => 'Lesser

        std.string.compare "a" "a"
        # => 'Equal

        std.string.compare "world" "hello"
        # => 'Greater
        ```
      "%
      = fun a b => %string/compare% a b,

    replace
      : String -> String -> String -> String
      | doc m%"
        `replace sub repl str` replaces every occurrence of `sub` in `str` with `repl`.

        Note that this function will not replace `sub` if it exists within a
        larger unicode extended grapheme cluster.

        # Examples

        ```nickel multiline
        std.string.replace "cd" "   " "abcdef"
        # => "ab   ef"

        std.string.replace "" "A" "abcdef"
        # => "AaAbAcAdAeAfA"
        ```
      "%
      = fun pattern replace s =>
        %string/replace% s pattern replace,

    replace_regex
      : String -> String -> String -> String
      | doc m%"
        `replace_regex regex repl string` replaces every match of `regex` in `string` with `repl`.

        **Note**: this function will only replace matches which start & end
        on the boundary of Unicode extended grapheme clusters. For example,
        `replace_regex "❤️" "_" "👨‍❤️‍💋‍👨"` will return `"👨‍❤️‍💋‍👨"`, since the
        heart codepoint occurs within the larger emoji grapheme cluster.

        # Examples

        ```nickel multiline
        std.string.replace_regex "l+." "j" "Hello!"
        # => "Hej!"

        std.string.replace_regex "\\d+" "\"a\" is not" "This 37 is a number."
        # "This \"a\" is not a number."
      ```
      "%
      = fun pattern replace s =>
        %string/replace_regex% s pattern replace,

    is_match
      : String -> String -> Bool
      | doc m%"
        `is_match regex string` checks if `string` matches `regex`.

        **Note**: this function only returns `true` when the regex match does
        not begin or end in the middle of a Unicode extended grapheme cluster.
        For example, searching for `"❤️"` within the string `"👨‍❤️‍💋‍👨"` will
        return `false`, as the heart codepoint is contained in the larger
        extended grapheme cluster.

        # Examples

        ```nickel multiline
        std.string.is_match "^\\d+$" "123"
        # => true

        std.string.is_match "\\d{4}" "123"
        # => false
        ```

        # Performance

        When checking multiple strings against a common regular expression, it
        is advantageous to store a partially applied version of this function. This
        partial evaluation will store a compiled version of the regular expression and
        prevent recompilation at each call site.

        For example, in the following program, the whole call to
        `std.string.is_match "[0-9]*\\.?[0-9]+ x"` is re-evaluated at each invocation of
        `is_number`. The regexp will be compiled 3 times in total:

        ```nickel
        let is_number = fun x =>
          std.string.is_match "[0-9]*\\.?[0-9]+" x
        in
        ["0", "42", "0.5"]
        |> std.array.all is_number
        # => true
        ```

        On the other hand, in the version below, the partial application of
        `std.string.is_match "[0-9]*\\.?[0-9]+"` is evaluated once, returning a
        function capturing the compiled regexp. The regexp will only be compiled
        once and for all:

        ```nickel
        let is_number' = std.string.is_match "[0-9]*\\.?[0-9]+" in
        ["0", "42", "0.5"]
        |> std.array.all is_number'
        # => true
        ```
      "%
      = fun regex => %string/is_match% regex,

    find
      : String -> String -> { matched : String, index : Number, groups : Array String }
      | doc m%"
        `find regex string` looks for matches of `regexp` in `string`. Returns
        the part of `string` that matched, the index of the first character that
        was part of the match in `string`, and an array of all capture groups if
        there were any.

        If there is no match, `find` returns
        `{matched = "", index = -1, groups = []}`.

        **Note**: this function ignores any match where either the match itself,
        or one of its capture groups, begin or end in the middle of a Unicode
        extended grapheme cluster.

        # Examples

        ```nickel multiline
        std.string.find "^(\\d).*(\\d).*(\\d).*$" "5 apples, 6 pears and 0 grapes"
        # => { matched = "5 apples, 6 pears and 0 grapes", index = 0, groups = [ "5", "6", "0" ] }

        std.string.find "3" "01234"
        # => { matched = "3", index = 3, groups = [ ] }
        ```

        # Performance

        Note that this function may perform better by sharing its partial
        application between multiple calls, because in this case the underlying
        regular expression will only be compiled once (see the documentation of
        `std.string.is_match` for more details).
      "%
      = fun regex => %string/find% regex,

    find_all
      : String -> String -> Array { matched : String, index : Number, groups : Array String }
      | doc m%"
        `find_all regex string` looks for all matches of `regexp` in `string`.
        For each match, it returns the part of `string` that matched, the index
        of the first character that was part of the match in `string`, and an
        array of all capture groups if there were any. Thus the return type is
        an array of the return type of `std.string.find`.

        If there is no match, `find` returns an empty array: `[]`.

        **Note**: this function ignores any match where either the match itself,
        or one of its capture groups, begin or end in the middle of a Unicode
        extended grapheme cluster.

        # Examples

        ```nickel multiline
        std.string.find_all "(\\d) (\\w+)" "5 apples, 6 pears and 0 grapes"
        # => [
        #   { groups = [ "5", "apples" ], index = 0, matched = "5 apples", },
        #   { groups = [ "6", "pears" ], index = 10, matched = "6 pears", },
        #   { groups = [ "0", "grapes" ], index = 22, matched = "0 grapes", }
        # ]

        std.string.find_all "2" "123 123 123"
        # => [
        #   { groups = [  ], index = 1, matched = "2", },
        #   { groups = [  ], index = 5, matched = "2", },
        #   { groups = [  ], index = 9, matched = "2", }
        # ]
        ```

        # Performance

        Note that this function may perform better by sharing its partial
        application between multiple calls, because in this case the underlying
        regular expression will only be compiled once (see the documentation of
        `std.string.is_match` for more details).
      "%
      = fun regex => %string/find_all% regex,

    length
      : String -> Number
      | doc m%"
        Returns the length of the string, as measured by the number of Unicode
        [extended grapheme clusters](https://unicode.org/glossary/#extended_grapheme_cluster).

        Generally speaking, this gives the number of "visible" glyphs in the string.

        **Warning**: because `length` works on Unicode grapheme clusters, some
        seemingly intuitive invariants might not hold. In particular, it isn't
        always true that `length (s1 ++ s2)` is equal to
        `length s1 + length s2`.

        # Examples

        ```nickel multiline
        std.string.length ""
        # => 0

        std.string.length "hi"
        # => 2

        std.string.length "四字熟語"
        # => 4

        std.string.length "👨🏾‍❤️‍💋‍👨🏻"
        # => 1
        ```
      "%
      = fun s => %string/length% s,

    substring
      : Number -> Number -> String -> String
      | doc m%"
        `substring start end str` takes the slice of `str` from `start` (included) to `end` (excluded).

        The index arguments are the indices of Unicode extended grapheme clusters
        rather than codepoints.

        # Preconditions

        In `substring start end str`, `start` and `end` must be positive integers
        such that `0 <= start <= end <= std.array.length value`.

        # Examples

        ```nickel multiline
        std.string.substring 3 5 "abcdef"
        # => "de"

        std.string.substring 3 10 "abcdef"
        # => error

        std.string.substring (-3) 4 "abcdef"
        # => error
        ```
      "%
      = fun start end s => %string/substr% s start end,

    from
      | Stringable -> String
      | doc m%"
        Converts a stringable value to its string representation. Same as
        `std.to_string`.

        # Examples

        ```nickel multiline
        std.string.from 42
        # => "42"

        std.string.from 'Foo
        # => "Foo"

        std.string.from null
        # => "null"

        std.string.from {value = 0}
        # => error
        ```
      "%
      = fun x => %to_string% x,

    from_number
      | Number -> String
      | doc m%"
        Converts a number to its string representation.

        # Examples

        ```nickel
        std.string.from_number 42
        # => "42"
        ```
      "%
      = from,

    from_enum
      | forall a. [|; a |] -> String
      | std.enum.Tag -> Dyn
      | doc m%"
        Converts an enum variant to its string representation.

        # Examples

        ```nickel
        std.string.from_enum 'MyEnum
        # => "MyEnum"
        ```
      "%
      = from,

    from_bool
      | Bool -> String
      | doc m%"
        Converts a boolean value to its string representation.

        # Examples

        ```nickel
        std.string.from_bool true
        # => "true"
        ```
      "%
      = from,

    to_number
      : String -> Number
      | NumberLiteral -> Dyn
      | doc m%"
        Converts a string that represents a number to that number.

        # Examples

        ```nickel
        std.string.to_number "123"
        # => 123
        ```
      "%
      = fun s => %number/from_string% s,

    to_bool
      : String -> Bool
      | BoolLiteral -> Dyn
      | doc m%"
        Converts a representation of a boolean (either `true` or `false`) to that boolean.

        # Examples

        ```nickel multiline
        std.string.to_bool "true"
        # => true

        std.string.to_bool "false"
        # => false
        ```
      "%
      # because of the contract on the argument, `s` can only be `"true"` or
      # `"false"`
      = fun s => s == "true",

    to_enum
      | String -> std.enum.Tag
      | doc m%"
        Converts a string to an enum tag.

        # Examples

        ```nickel multiline
        std.string.to_enum "Hello"
        # => 'Hello

        std.string.to_enum "hey,there!"
        # => '"hey,there!"
        ```
      "%
      = fun s => %enum/from_string% s,
  },

  test = {
    Assert
      | doc m%"
        A contract that checks if its argument is the boolean true.

        # Examples

        ```nickel multiline
        1 == 2 | std.test.Assert
        # => error: contract broken by a value

        1 == 1 | std.test.Assert
        # => true
        ```
        "%
      =
        %contract/custom% (fun _label value =>
          if value then
            'Ok value
          else
            'Error {}
        ),

    assert_all
      | Array Assert -> Bool
      | doc m%"
        Asserts that all the elements of an array are equal to true. Applies the
        `std.test.Assert` contract on each element of the given array.

        Evalutes to `true` if all the elements of the array are equal to true,
        or fails with a contract error otherwise.

        # Example

        ```nickel
        [ (1 == 2), (1 == 1), (1 == 3) ] |> std.test.assert_all
        # => error: contract broken by the caller
        ```
        "%
      # We mostly rely on the contracts to do the work here. We just need to
      # make sure each element of the array is properly forced.
      = std.array.all (fun x => x),
  },

  is_number
    : Dyn -> Bool
    | doc m%"
      Checks if a value is a number.

      # Examples

      ```nickel multiline
      std.is_number 1
      # => true

      std.is_number "Hello, World!"
      # => false
      ```
    "%
    = fun x => %typeof% x == 'Number,

  is_bool
    : Dyn -> Bool
    | doc m%"
      Checks if a value is a bool.

      # Examples

      ```nickel multiline
      std.is_bool false
      # => true

      std.is_bool 42
      # => false
      ```
    "%
    = fun x => %typeof% x == 'Bool,

  is_string
    : Dyn -> Bool
    | doc m%"
      Checks if a value is a string.

      # Examples

      ```nickel multiline
      std.is_string true
      # => false

      std.is_string "Hello, World!"
      # => true
      ```
    "%
    = fun x => %typeof% x == 'String,

  is_enum
    : Dyn -> Bool
    | doc m%"
      Checks if a value is an enum tag.

      # Examples

      ```nickel multiline
      std.is_enum true
      # => false

      std.is_enum 'false
      # => true
      ```
    "%
    = fun x => %typeof% x == 'Enum,

  is_function
    : Dyn -> Bool
    | doc m%"
      Checks if a value is a function.

      # Examples

      ```nickel multiline
      std.is_function (fun x => x)
      # => true

      std.is_function 42
      # => false
      ```
    "%
    = fun x => %typeof% x == 'Function,

  is_array
    : Dyn -> Bool
    | doc m%"
      Checks if a value is an array.

      # Examples

      ```nickel multiline
      std.is_array [ 1, 2 ]
      # => true

      std.is_array 42
      # => false
      ```
    "%
    = fun x => %typeof% x == 'Array,

  is_record
    : Dyn -> Bool
    | doc m%"
      Checks if a value is a record.

      # Examples

      ```nickel multiline
      std.is_record [ 1, 2 ]
      # => false

      std.is_record { hello = "Hello", world = "World" }
      # => true
      ```
    "%
    = fun x => %typeof% x == 'Record,

  typeof
    : Dyn -> [|
      'Number,
      'Bool,
      'String,
      'Enum,
      'Label,
      'Function,
      'Array,
      'Record,
      'ForeignId,
      'CustomContract,
      'Type,
      'Other
    |]
    | doc m%"
      Returns the type of a value.

      # Examples

      ```nickel multiline
      std.typeof [ 1, 2 ]
      # => 'Array

      std.typeof (fun x => x)
      # => 'Function
      ```
    "%
    = fun x => %typeof% x,

  seq
    : forall a. Dyn -> a -> a
    | doc m%"
      `seq x y` forces the evaluation of `x`, before returning `y`.

      The evaluation of `x` stops as soon as a so-called weak head normal form
      is reached, which is either:

       - an array
       - a record
       - a function (or a match statement, which is function in disguise)
       - a constant (a number, a boolean, a string literal or an enum tag)
       - a contract label

      Note that `seq` won't evaluate _the content_ of arrays and records
      further. Please see `std.deep_seq` to recursively force the
      evaluation of an expression.

      # Examples

      ```nickel multiline
      std.seq (42 / 0) 37
      # => error

      std.seq (42 / 2) 37
      # => 37

      std.seq { too_far = 42 / 0 } 37
      # => 37
      ```
    "%
    = fun x y => %seq% x y,

  deep_seq
    : forall a. Dyn -> a -> a
    | doc m%"
      `deep_seq x y` forces a deep evaluation `x`, before returning `y`. Deep
       evaluation means `deep_seq` recursively forces the evaluation of
       the content of records and arrays, as opposed to `std.seq`.

      # Examples

      ```nickel multiline
      std.deep_seq (42 / 0) 37
      # => error

      std.deep_seq (42 / 2) 37
      # => 37

      std.deep_seq [1+1, { not_too_far = [42 / 0] }] 37
      # => error
      ```
    "%
    = fun x y => %deep_seq% x y,

  hash
    : [| 'Md5, 'Sha1, 'Sha256, 'Sha512 |] -> String -> String
    | doc m%"
      Hashes the given string with the desired hashing algorithm.

      # Examples

      ```nickel
      std.hash 'Md5 "hunter2"
      # => "2ab96390c7dbe3439de74d0c9b0b1767"
      ```
    "%
    = fun type s => %hash% type s,

  serialize
    : [| 'Json, 'Toml, 'Yaml |] -> Dyn -> String
    | doc m%"
      Serializes a value into the desired representation.

      # Examples

      ```nickel
      serialize 'Json { hello = "Hello", world = "World" }
      # => "{\n  \"hello\": \"Hello\",\n  \"world\": \"World\"\n}"
      ```
    "%
    = fun format x => %serialize% format (%force% x),

  deserialize
    : [| 'Json, 'Toml, 'Yaml |] -> String -> Dyn
    | doc m%"
      Deserializes a string into a Nickel value from the given representation.

      # Examples

      ```nickel
      deserialize 'Json "{ \"hello\": \"Hello\", \"world\": \"World\" }"
      # => { hello = "Hello", world = "World" }
      ```
    "%
    = fun format x => %deserialize% format x,

  to_string
    | std.string.Stringable -> String
    | doc m%"
      Converts a stringable value to a string representation. Same as
      `std.string.from`.

      # Examples

      ```nickel multiline
      std.to_string 42
      # => "42"

      std.to_string 'Foo
      # => "Foo"

      std.to_string null
      # => "null"
      ```
    "%
    = fun x => %to_string% x,

  trace
    : forall a. String -> a -> a
    | doc m%"
      `std.trace msg x` prints `msg` to standard error, then proceeds with
      the evaluation of `x`.

      # Examples

      ```nickel
      std.trace "Hello, world!" true
      # std.trace: Hello, world!
      # => true
      ```
    "%
    = fun msg x => %trace% msg x,

  FailWith
    | doc m%"
      A contract that always fails with the given message.

      # Examples

      ```nickel
      1 | std.FailWith "message"
      # => error: message
      ```
    "%
    = fun msg =>
      %contract/custom% (fun _label _value => 'Error { message = msg }),

  fail_with
    | String -> (forall a. a)
    | doc m%"
      Abort the evaluation with the given message. The error will be reported as
      a contract violation, as `fail_with` uses `std.FailWith` under the hood.

      # Examples

      ```nickel
      std.fail_with "message"
      # => error: message
      ```
    "%
    = fun msg => null | FailWith msg,
}
