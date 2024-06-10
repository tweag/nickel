# Nickel standrd library. This file is included at compile-time in the nickel
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

          ```nickel
          ([] | std.array.NonEmpty) =>
            error
          ([ 1 ] | std.array.NonEmpty) =>
            [ 1 ]
          ```
        "%
      = fun label value =>
        if %typeof% value == 'Array then
          if %array/length% value != 0 then
            value
          else
            %blame% (%label/with_message% "empty array" label)
        else
          %blame% (%label/with_message% "not a array" label),

    first
      : forall a. Array a -> a
      | NonEmpty -> Dyn
      | doc m%"
          Returns the first element of an array.

          # Examples

          ```nickel
          std.array.first [ "this is the head", "this is not" ] =>
            "this is the head"
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
          std.array.last [ "this is the head", "this is not" ] =>
            "this is not"
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
          std.array.drop_first [ 1, 2, 3 ] =>
            [ 2, 3 ]
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
          std.array.drop_last [ 1, 2, 3 ] =>
            [ 1, 2 ]
          ```
        "%
      = fun array => %array/slice% 0 (%array/length% array - 1) array,

    length
      : forall a. Array a -> Number
      | doc m%"
          Returns the length of an array.

          # Examples

          ```nickel
          std.array.length [ "Hello,", " World!" ] =>
            2
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
          std.array.map (fun x => x + 1) [ 1, 2, 3 ] =>
            [ 2, 3, 4 ]
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
          std.array.at 3 [ "zero", "one", "two", "three", "four" ] =>
            "three"
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

          ```nickel
          std.array.at_or 3 "default" [ "zero", "one", "two", "three" ] =>
            "three"

          std.array.at_or 3 "default" [ "zero", "one" ] =>
            "default"
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
            std.array.concat [ 1, 2, 3 ] [ 4, 5, 6 ] =>
              [ 1, 2, 3, 4, 5, 6 ]
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
             => -2
            std.array.fold_left (-) 0 [1, 2, 3, 4]
             => -10
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
          fold_left (fun acc e => acc + e) 0 [ 1, 2, 3 ] =>
            (((0 + 1) + 2) 3) =>
            6
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
          std.array.fold_right (fun e acc => acc @ [e]) [] [ 1, 2, 3 ] =>
            ((([] @ [3]) @ [2]) @ [1]) =>
            [ 3, 2, 1 ]
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
          std.array.prepend 1 [ 2, 3 ] =>
            [ 1, 2, 3 ]
          ```
        "%
      = fun x l => [x] @ l,

    append
      : forall a. a -> Array a -> Array a
      | doc m%"
          Builds an array given the last element and the rest of the array.

          # Examples

          ```nickel
          std.array.append 3 [ 1, 2 ] =>
            [ 1, 2, 3 ]
          ```
        "%
      = fun x l => l @ [x],

    reverse
      : forall a. Array a -> Array a
      | doc m%"
          Reverses an array.

          # Examples

          ```nickel
          std.array.reverse [ 1, 2, 3 ] =>
            [ 3, 2, 1 ]
          ```
        "%
      = fun l => fold_left (fun acc e => [e] @ acc) [] l,

    filter
      : forall a. (a -> Bool) -> Array a -> Array a
      | doc m%"
          `filter f xs` returns an array containing all elements from `xs` that satisfy `f`.

          # Examples

          ```nickel
          std.array.filter (fun x => x <= 3) [ 4, 3, 2, 5, 1 ] =>
            [ 3, 2, 1 ]
          ```
        "%
      = fun pred l => fold_left (fun acc x => if pred x then acc @ [x] else acc) [] l,

    flatten
      : forall a. Array (Array a) -> Array a
      | doc m%"
          Concatenates all elements of an array of arrays.

          # Examples

          ```nickel
          std.array.flatten [[1, 2], [3, 4]] =>
            [1, 2, 3, 4]
          ```
        "%
      = fun l => fold_right (fun l acc => l @ acc) [] l,

    all
      : forall a. (a -> Bool) -> Array a -> Bool
      | doc m%"
          Returns `true` if all elements in the given array satisfy the predicate,
          `false` otherwise.

          # Examples

          ```nickel
          std.array.all (fun x => x < 3) [ 1, 2 ] =>
            true
          std.array.all (fun x => x < 3) [ 1, 2, 3 ] =>
            false
          ```
        "%
      = fun pred l => fold_right (fun x acc => if pred x then acc else false) true l,

    any
      : forall a. (a -> Bool) -> Array a -> Bool
      | doc m%"
          Returns `true` if at least one element in the given array satisfies
          the predicate, `false` otherwise.

          # Examples

          ```nickel
          std.array.any (fun x => x < 3) [ 1, 2, 3, 4 ] =>
            true
          std.array.any (fun x => x < 3) [ 5, 6, 7, 8 ] =>
            false
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
          std.array.elem 3 [ 1, 2, 3, 4, 5 ] =>
            true
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
          std.array.partition (fun x => x < 5) [ 2, 4, 5, 3, 7, 8, 6 ] =>
            { right = [ 2, 4, 3 ], wrong = [ 5, 7, 8, 6 ] }
          ```
        "%
      = fun pred l =>
        let aux = fun acc x =>
          if (pred x) then
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
          std.array.generate (fun x => x * x) 4 =>
            [ 0, 1, 4, 9 ]
          ```
        "%
      = fun f n => %array/generate% n f,

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
          => [ 1, 2, 4, 5 ]
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
            => [1, 1, 2, 2, 3, 3]
          ```
        "%
      = fun f xs => map f xs |> flatten,

    intersperse
      : forall a. a -> Array a -> Array a
      | doc m%"
          Intersperses a value between the elements of an array.

          # Examples

          ```nickel
          std.array.intersperse ", " [ "Hello", "wonderful", "world!" ]
            => [ "Hello", ", ", "wonderful", ", ", "world!" ]
          std.array.intersperse ", " [ "Hello" ]
            => [ "Hello" ]
          std.array.intersperse ", " []
            => []
          ```
        "%
      = fun v array =>
        let length = %array/length% array in
        if length <= 1 then
          array
        else
          let first = %array/at% array 0 in
          let rest = %array/slice% 1 length array in
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

          ```nickel
          std.array.slice 1 3 [ 0, 1, 2, 3, 4, 5]
            => [ 1, 2 ]
          std.array.slice 0 3 [ "Hello", "world", "!" ]
            => [ "Hello", "world", "!" ]
          std.array.slice 2 3 [ "Hello", "world", "!" ]
            => [ "!" ]
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

          ```nickel
          std.array.split_at 2 [ 0, 1, 2, 3, 4, 5]
            => { left = [ 0, 1 ], right = [ 2, 3, 4, 5 ] }
          std.array.split_at 0 [ "Hello", "world", "!" ]
            => { left = [  ], right = [ "Hello", "world", "!" ] }
          std.array.split_at 3 [ "Hello", "world", "!" ]
            => { left = [ "Hello", "world", "!" ], right = [  ] }
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

          ```nickel
          std.array.replicate 0 false
            => [ ]
          std.array.replicate 5 "x"
            => [ "x", "x", "x", "x", "x" ]
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
           => [ -1.5, -1, -0.5, 0, 0.5, 1, 1.5 ]
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
           => [ 0, 1, 2, 3, 4 ]
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

          ```nickel
          std.array.reduce_left (@) [ [1, 2], [3], [4,5] ]
            => (([1, 2] @ [3]) @ [4,5])
            => [ 1, 2, 4, 5 ]
          std.array.reduce_left (-) [ 1, 2, 3, 4]
            => ((1 - 2) - 3) - 4
            => -8
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

          ```nickel
          std.array.reduce_right (@) [ [1, 2], [3], [4,5] ]
            => [1, 2] @ ([3] @ [4,5])
            => [ 1, 2, 4, 5 ]
          std.array.reduce_right (-) [ 1, 2, 3, 4]
            => 1 - (2 - (3 - 4))
            => -2
          ```
        "%
      = fun f array =>
        let last_index = %array/length% array - 1 in
        let last = %array/at% array last_index in
        let rest = %array/slice% 0 last_index array in
        fold_right f last rest,

    zip_with
      : forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c
      | doc m%"
          `zip_with f xs ys` combines the arrays `xs` and `ys` using the
          operation `f`. The resulting array's length will be the smaller of the
          lengths of `xs` and `ys`.

          # Examples

          ```nickel
          std.array.zip_with (+) [1, 2, 3] [4, 5, 6]
            => [5, 7, 9]
          std.array.zip_with (*) [1, 2] [4, 5, 6]
            => [4, 10]
          std.array.zip_with (-) [1, 2, 3] [4, 5]
            => [-3, -3]
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
          std.array.map_with_index (fun i x => i + x + 1) [ 1, 2, 3 ] =>
            [ 2, 4, 6 ]
          ```
        "%
      = fun f xs =>
        xs
        |> std.array.length
        |> std.array.generate (fun i => f i (std.array.at i xs)),
  },

  contract = {
    Equal
      | doc m%"
        `Equal` is a contract enforcing equality to a given constant.

        `Equal` is lazy over arrays and records. When checking such values,
        `Equal` doesn't test for equality of elements right away (it just tests
        that the size is the same for arrays and that fields are the same for
        records), but returns a new value where equality subcontracts have been
        pushed inside each element.

        # Example

        ```nickel
        1 + 4 | std.contract.Equal 5
         => 5
        4 | std.contract.Equal 5
         => error: contract broken by a value
        ```
      "%
      =
        let fields_diff
          | doc m%"
            Compute the difference between the fields of two records.
            `fields_diff` isn't concerned with the actual values themselves, but
            just with field names.

            Return a record of type
            `{extra : Array String, missing: Array String}`, relative to the
            first argument `constant`.
          "%
          = fun constant value =>
            let diff =
              value
              |> std.record.fields
              |> std.array.fold_left
                (
                  fun acc field =>
                    if std.record.has_field field acc.rest then
                      {
                        extra = acc.extra,
                        rest = std.record.remove field acc.rest,
                      }
                    else
                      {
                        extra = std.array.append field acc.extra,
                        rest = acc.rest,
                      }
                )
                { extra = [], rest = constant }
            in
            { extra = diff.extra, missing = std.record.fields diff.rest }
          in
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
                let diff = fields_diff constant value in
                if %array/length% diff.extra != 0 then
                  blame_fields_differ "extra" diff.extra ctr_label
                else if %array/length% diff.missing != 0 then
                  blame_fields_differ "missing" diff.missing ctr_label
                else
                  %contract/record_lazy_apply%
                    ctr_label
                    value
                    (
                      fun field =>
                        contract_map."%{field}"
                    ),
            'Array =>
              fun ctr_label value =>
                let value = check_typeof_eq ctr_label value in
                let value_length = %array/length% value in
                if value_length == %array/length% constant then
                  %array/generate%
                    value_length
                    (
                      fun i =>
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
                value
                |> check_typeof_eq ctr_label
                |> from_predicate ((==) constant) ctr_label,
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
          IsZero = fun label value =>
            if value == 0 then
              value
            else
              std.contract.blame label
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

    from_predicate
      | doc m%"
          Generates a contract from a boolean predicate.

          Type: `(Dyn -> Bool) -> (Label -> Dyn -> Dyn)`
          (for technical reasons, this function isn't actually statically typed)

          # Examples

          ```nickel
          let IsZero = std.contract.from_predicate (fun x => x == 0) in
          0 | IsZero
          ```
        "%
      = fun pred label value => if pred value then value else %blame% label,

    Sequence
      | doc m%"
          Apply multiple contracts from left to right.

          Type: `Array Contract -> Contract`
          (for technical reasons, this function isn't actually statically typed)

          # Examples

          ```nickel
          x | std.contract.Sequence [ Foo, Bar ]
          ```

          is equivalent to

          ```nickel
          (x | Foo) | Bar
          ```

          This is useful in positions where one cannot apply multiple contracts,
          such as an argument to another contract:

          ```nickel
          x | Array (std.contract.Sequence [ Foo, Bar ])
          ```

          Or stored in a variable:

          ```nickel
          let C = std.contract.Sequence [ Foo, Bar ] in
          x | C
          ```
        "%
      = fun contracts label value =>
        std.array.fold_left
          (fun acc contract => std.contract.apply contract label acc)
          value
          contracts,

    label
      | doc m%"
          The label submodule provides functions that manipulate the label
          associated with a contract application.

          A label is a special opaque value automatically passed by the Nickel
          interpreter to contracts when performing a contract check.

          A label stores a stack of custom error diagnostics, that can be
          manipulated by the functions in this module. Labels thus offer a way to
          customize the error message that will be shown if a contract is broken.

          The setters (`with_XXX` functions) always operate on the current error
          diagnostic, which is the last diagnotic on the stack. If the stack is
          empty, a fresh diagnostic is silently created when using a setter.

          The previous diagnostics on the stack are considered archived, and
          can't be modified anymore. All diagnostics will be shown during error
          reporting, with the most recent being used as the main one.

          `std.contract.apply` is the operation that pushes a new fresh diagnostic on
          the stack, saving the previously current error diagnostic. The function
          `std.contract.apply` is typically used to apply subcontracts inside a parent
          contract. Stacking the current diagnostic potentially customized by
          the parent contract saves the information inside, and provides a fresh
          diagnostic for the child contract to use.
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

              ```nickel
              let ContractNum = std.contract.from_predicate (fun x => x > 0 && x < 50) in

              let Contract = fun label value =>
                if std.is_number value then
                  std.contract.apply
                    ContractNum
                    (std.contract.label.with_message
                      "num subcontract failed! (out of bound)"
                      label
                    )
                    value
                else
                  value
              in

              5 | Contract
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

              ```nickel
              let ContractNum = std.contract.from_predicate (fun x => x > 0 && x < 50) in

              let Contract = fun label value =>
                if std.is_number value then
                  std.contract.apply
                    ContractNum
                    (label
                     |> std.contract.label.with_message "num subcontract failed! (out of bound)"
                     |> std.contract.label.with_notes [
                          "The value was a number, but this number is outside the expected bounds",
                          "The value must be a number between 0 and 50, both excluded",
                        ]
                    )
                    value
                else
                  value
              in

              5 | Contract
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

              ```nickel
              let AlwaysFailWithNotes = fun label _value =>
                label
                |> std.contract.label.append_note "This is note 1"
                |> std.contract.label.append_note "This is note 2"
                |> std.contract.blame
              in
              null | AlwaysFailWithNotes
            "%
          = fun note label => %label/append_note% note label,
      },

    apply
      | doc m%"
          Applies a contract to a label and a value.

          Type: `Contract -> Label -> Dyn -> Dyn`
          (for technical reasons, this function isn't actually statically typed)

          Nickel supports user-defined contracts defined as functions, but also
          as records. Moreover, the interpreter performs additional bookkeeping
          for error reporting when applying a contract in an expression
          `value | Contract`. You should not use standard function application
          to apply a contract, but this function instead.

          # Examples

          ```nickel
          let Nullable = fun param_contract label value =>
            if value == null then null
            else std.contract.apply param_contract label value
          in
          let Contract = Nullable {foo | Number} in
          ({foo = 1} | Contract)
          ```

          # Diagnostics stack

          Using `apply` will stack the current custom error reporting data, and
          create a fresh working diagnostic. `apply` thus acts as a split point
          between a contract and its subcontracts, providing the subcontracts
          with a fresh diagnostic to use, while remembering the previous
          diagnostics set by parent contracts.

          ## Illustration

          ```nickel
          let ChildContract = fun label value =>
            label
            |> std.contract.label.with_message "child's message"
            |> std.contract.label.append_note "child's note"
            |> std.contract.blame
          in

          let ParentContract = fun label value =>
            let label =
              label
              |> std.contract.label.with_message "parent's message"
              |> std.contract.label.append_note "parent's note"
            in
            std.contract.apply ChildContract label value
          in

          null | ParentContract
          ```

          This example will print two diagnostics: the main one, using the
          message and note of the child contract, and a secondary diagnostic,
          using the parent contract message and note.
        "%
      = fun contract label value =>
        %contract/apply% contract (%label/push_diag% label) value,

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

              ```nickel
              fun arg =>
                let arg_checked = (arg | Foo) in
                (some_function arg_checked | (Bar arg_checked))
              ```
            "%
          = fun Domain Codomain label function =>
            fun arg =>
              let arg_with_ctr = std.contract.apply Domain label arg in
              function arg_with_ctr
              |> contract.apply (Codomain arg_with_ctr) label,

        PosNumber
          | doc m%"
              **Warning**: this is an unstable item. It might be renamed,
              modified or deleted in any subsequent minor Nickel version.

              A contract for a positive number.

              This contract blames only if the value is a number but is not
              positive: the fact that the value is a number is assumed to be
              checked by an associated static type annotation.
            "%
          = fun label value =>
            if %typeof% value == 'Number then
              if value >= 0 then
                value
              else
                %blame% (%label/append_note% "Expected a positive number, got %{%to_string% value}" label)
            else
              value,

        RangeStep
          | doc m%"
              **Warning**: this is an unstable item. It might be renamed,
              modified or deleted in any subsequent minor Nickel version.

              A contract for a range step, which must be a positive number.
              Mostly a wrapper around `std.contract.unstable.PosNumber` with a
              custom error message.

              This contract blames only if the value is a number but is not
              positive: the fact that the value is a number is assumed to be
              checked by an associated static type annotation.
            "%
          = fun label =>
            PosNumber (%label/with_message% "invalid range step" label),

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
          =
            # capture the `contract.label` module to avoid name collisions with
            # local contract argument `label`
            let label_module = label in
            let attach_message = label_module.with_message "invalid range" in

            fun Codomain =>
              let RangeSecond = fun start label value =>
                if %typeof% start == 'Number && %typeof% value == 'Number then
                  if start > value then
                    let start_as_str = %to_string% start in
                    let end_as_str = %to_string% value in
                    label
                    |> attach_message
                    |> label_module.append_note "Expected a range end greater than %{start_as_str} (range start), got %{end_as_str}"
                    |> blame
                  else
                    value
                else
                  value
              in

              DependentFun
                Dyn
                (fun start => RangeSecond start -> Codomain),

        IndexedArrayFun
          | [| 'Index, 'Split |] -> Dyn -> Dyn
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

            let ArrayIndexFirst = fun label value =>
              if %typeof% value == 'Number then
                let label =
                  label
                  |> attach_message
                  |> label_module.append_note "Expected array index to be a positive integer, got %{%to_string% value} "
                in
                std.contract.apply std.number.Nat label value
              else
                value
            in

            let ArrayIndexSecond = fun type min_size label value =>
              if %typeof% min_size == 'Number && %typeof% value == 'Array then
                let max_idx = type |> match { 'Index => %array/length% value - 1, 'Split => %array/length% value } in
                if min_size > max_idx then
                  let index_as_str = %to_string% min_size in
                  let max_as_str = %to_string% max_idx in
                  let note =
                    if %array/length% value == 0 then
                      "Can't index into an empty array"
                    else
                      "Expected an array index between 0 and %{max_as_str} (included), got %{index_as_str}"
                  in
                  label
                  |> attach_message
                  |> label_module.append_note note
                  |> blame
                else
                  value
              else
                value
            in
            let contract = fun type label function =>
              DependentFun
                ArrayIndexFirst
                (fun index => ArrayIndexSecond type index -> Dyn)
                label
                function
            in contract,

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

            let SliceIndexFirst = fun label value =>
              if %typeof% value == 'Number then
                let label =
                  label
                  |> attach_message
                  |> label_module.append_note "Expected the array slice start index to be a positive integer, got %{%to_string% value}"
                in
                std.contract.apply std.number.Nat label value
              else
                value
            in

            let SliceIndexSecond = fun start label value =>
              if %typeof% start == 'Number && %typeof% value == 'Number then
                if value < start then
                  label
                  |> attach_message
                  |> label_module.append_note "Expected the array slice indices to satisfy `start <= end`, but got %{%to_string% start} (start) and %{%to_string% value} (end)"
                  |> blame
                else
                  let label =
                    label
                    |> attach_message
                    |> label_module.append_note "Expected the array slice end index to be a positive integer, got %{%to_string% value}"
                  in
                  std.contract.apply std.number.Nat label value
              else
                value
            in

            let ArraySliceArray = fun end_index label value =>
              if %typeof% end_index == 'Number && %typeof% value == 'Array then
                if end_index > %array/length% value then
                  let index_as_str = %to_string% end_index in
                  let size_as_str = %to_string% (%array/length% value) in
                  label
                  |> attach_message
                  |> label_module.append_note "Expected the slice end index to be between 0 and %{size_as_str} (array's length), got %{index_as_str}"
                  |> blame
                else
                  value
              else
                value
            in
            DependentFun
              SliceIndexFirst
              (
                fun start_index =>
                  DependentFun
                    (SliceIndexSecond start_index)
                    (
                      fun end_index =>
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
              ```

              In this example, the first call to `f` won't blame, but the second
              will, as `baz` isn't a field of the record argument.
            "%%
          =
            # capture the `contract.label` module to avoid name collisions with
            # local contract argument `label`
            let label_module = label in
            let attach_message = label_module.with_message "missing field" in

            fun Codomain =>
              let HasFieldSecond = fun field label value =>
                if %typeof% field == 'String
                && %typeof% value == 'Record
                && !(%record/has_field% field value) then
                  label
                  |> attach_message
                  |> label_module.append_note
                    "The record given as an argument lacks the required field `%{field}`."
                  |> blame
                else
                  value
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

        ```nickel
        ('foo | std.enum.Tag) =>
          'foo
        ('FooBar | std.enum.Tag) =>
          'FooBar
        ("tag" | std.enum.Tag) =>
          error
        ```
      "%
      = std.contract.from_predicate is_enum_tag,

    Enum
      | doc m%"
        Enforces that the value is an enum (either a tag or a variant).

        # Examples

        ```nickel
        ('Foo | std.enum.Enum) =>
          'Foo
        ('Bar 5 | std.enum.Enum) =>
          'Bar 5
        ("tag" | std.enum.Enum) =>
          error
        "%
      = std.contract.from_predicate std.is_enum,

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
      = fun label value =>
        let label =
          std.contract.label.with_message
            "expected either a string or an enum tag"
            label
        in
        %typeof% value
        |> match {
          'String => %enum/from_string% value,
          'Enum =>
            if is_enum_variant value then
              std.contract.blame label
            else
              value,
          _ => std.contract.blame label,
        },

    is_enum_tag
      : Dyn -> Bool
      | doc m%"
        Checks if a value is an enum tag. Enum variants (applied to an argument)
        aren't considered enum tags.

        # Examples

        ```nickel
        std.enum.is_enum_tag 'foo
          => true
        std.enum.is_enum_tag 'FooBar
          => true
        std.enum.is_enum_tag "tag"
          => false
        std.enum.is_enum_tag ('Foo "arg")
          => false
        ```
      "%
      = fun value => std.is_enum value && !(%enum/is_variant% value),

    is_enum_variant
      : Dyn -> Bool
      | doc m%"
        Checks if a value is an enum variant. Bare enum tags (not applied to an
        argument) aren't considered enum variants.

        # Examples

        ```nickel
        std.enum.is_enum_variant ('Foo "arg")
          => true
        std.enum.is_enum_variant ('Http {version = "1.1"})
          => true
        std.enum.is_enum_variant 'foo
          => false
        std.enum.is_enum_variant [1, 2, 3]
          => false
        ```
      "%
      = fun value => %enum/is_variant% value,

    to_tag_and_arg
      | Enum -> { tag | String, arg | optional }
      | doc m%"
        Convert an enum to record with a string tag and an optional argument. If
        the enum is an enum tag, the `arg` field is simply omitted.

        `std.enum.from_tag_and_arg` provides the inverse transformation,
        reconstructing an enum from a string tag and an argument.

        # Examples

        ```nickel
        std.enum.to_tag_and_arg ('Foo "arg") =>
          { tag = "Foo", arg = "arg" }
        std.enum.to_tag_and_arg 'http
          => { tag = "http" }
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
        Create an enum from a string tag and an optional argument. If the `arg`
        field is omitted, a bare enum tag is created.

        `std.enum.to_tag_and_value` provides the inverse transformation,
        extracting a string tag and an argument from an enum.

        # Examples

        ```nickel
        std.enum.from_tag_and_arg { tag = "Foo", arg = "arg" }
          => ('Foo "arg")
        std.enum.from_tag_and_arg { tag = "http" }
          => 'http
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

          ```nickel
          std.enum.map ((+) 1) ('Foo 42)
            => 'Foo 43
          std.enum.map f 'Bar
            => 'Bar
          ```
        "%
      = fun f enum_value =>
        if %enum/is_variant% enum_value then
          let tag = (%to_string% (%enum/get_tag% enum_value)) in
          let mapped = f (%enum/get_arg% enum_value) in

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

        ```nickel
        std.function.id null
          => null
        (std.function.id (fun x => x + 1)) 0
          => 1
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
          => (10 / 2) + 1
          => 6
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
          => "Hello, world!"
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
          => 5
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
          => 5
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
          => 7
        ```
      "%
      = fun x y => y,

    pipe
      : forall a. a -> Array (a -> a) -> a
      | doc m%%"
        Applies an array of functions to a value, in order.

        # Examples

        ```nickel
        std.function.pipe 2 [ (+) 2, (+) 3 ]
          => 7
        std.function.pipe 'World [ std.string.from, fun s => "Hello, %{s}!" ]
          => "Hello, World!"
        ```
      "%%
      = fun x fs => std.array.fold_left (|>) x fs,
  },

  number = {
    Integer
      | doc m%"
        Enforces that a number is an integer.

        # Examples

        ```nickel
        (1.5 | Integer) =>
          error
        (42 | Integer) =>
          42
        ```
      "%
      = fun label value =>
        if %typeof% value == 'Number then
          if value % 1 == 0 then
            value
          else
            %blame% (%label/with_message% "not an integer" label)
        else
          %blame% (%label/with_message% "not a number" label),

    Nat
      | doc m%"
        Enforces that a number is a natural number (including 0).

        # Examples

        ```nickel
        (42 | std.number.Nat) =>
          42
        (0 | std.number.Nat) =>
          0
        (-4 | std.number.Nat) =>
          error
        ```
      "%
      = fun label value =>
        if %typeof% value == 'Number then
          if value % 1 == 0 && value >= 0 then
            value
          else
            %blame% (%label/with_message% "not a natural" label)
        else
          %blame% (%label/with_message% "not a number" label),

    PosNat
      | doc m%"
        Enforces that a number is a strictly positive natural number.

        # Examples

        ```nickel
        (42 | std.number.PosNat) =>
          42
        (0 | std.number.PosNat) =>
          error
        (-4 | std.number.PosNat) =>
          error
        ```
      "%
      = fun label value =>
        if %typeof% value == 'Number then
          if value % 1 == 0 && value > 0 then
            value
          else
            %blame% (%label/with_message% "not positive integer" label)
        else
          %blame% (%label/with_message% "not a number" label),

    NonZero
      | doc m%"
        Enforces that a number is not equal to zero.

        # Examples

        ```nickel
        (1 | std.number.NonZero) =>
          1
        (0.0 | std.number.NonZero) =>
          error
        ```
      "%
      = fun label value =>
        if %typeof% value == 'Number then
          if value != 0 then
            value
          else
            %blame% (%label/with_message% "non-zero" label)
        else
          %blame% (%label/with_message% "not a number" label),

    is_integer
      : Number -> Bool
      | doc m%"
        Checks if the given number is an integer.

        # Examples

        ```nickel
        std.number.is_integer 42 =>
          true
        std.number.is_integer 1.5 =>
          false
        ```
      "%
      = fun x => x % 1 == 0,

    min
      : Number -> Number -> Number
      | doc m%"
        Returns the lower of two numbers.

        # Examples

        ```nickel
        std.number.min (-1337) 42 =>
          -1337
        ```
      "%
      = fun x y => if x <= y then x else y,

    max
      : Number -> Number -> Number
      | doc m%"
        Returns the higher of two numbers.

        # Examples

        ```nickel
        std.number.max (-1337) 42 =>
          42
        ```
      "%
      = fun x y => if x >= y then x else y,

    floor
      : Number -> Number
      | doc m%"
        Rounds a number down to the next integer.

        # Examples

        ```nickel
        std.number.floor 42.5 =>
          42
        std.number.floor (-42.5) =>
          -43
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

        ```nickel
        std.number.abs (-5) =>
          5
        std.number.abs 42 =>
          42
        ```
      "%
      = fun x => if x < 0 then -x else x,

    fract
      : Number -> Number
      | doc m%"
        Returns the fractional part of a number.

        # Examples

        ```nickel
        std.number.fract 13.37 =>
          0.37
        std.number.fract 42 =>
          0
        ```
      "%
      = fun x => x % 1,

    truncate
      : Number -> Number
      | doc m%"
        Truncates a number, rounding it towards 0.

        # Examples

        ```nickel
        std.number.truncate (-13.37) =>
          -13
        std.number.truncate 42.5 =>
          42
        ```
      "%
      = fun x => x - (x % 1),

    pow
      : Number -> Number -> Number
      | doc m%"
        `pow x y` returns `x` to the power of `y`.

        # Examples

        ```nickel
        std.number.pow 2 8 =>
          256
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
  },

  record = {
    map
      : forall a b. (String -> a -> b) -> { _ : a } -> { _ : b }
      | doc m%"
        Maps a function over every field of a record. The function is passed the
        name and value of each field.

        # Examples

        ```nickel
        std.record.map (fun s x => s) { hi = 2 } =>
          { hi = "hi" }
        std.record.map (fun s x => x + 1) { hello = 1, world = 2 } =>
          { hello = 2, world = 3 }
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

        ```nickel
        std.record.fields { one = 1, two = 2 } =>
          [ "one", "two" ]
        std.record.fields { one = 1, two = 2, three_opt | optional } =>
          [ "one", "two" ]
        ```
      "%
      = fun r => %record/fields% r,

    fields_with_opts
      : forall a. { _ : a } -> Array String
      | doc m%"
        Returns an array containing the names of all the fields of a record.

        Same as `std.record.fields`, but doesn't ignore empty optional fields.

        # Examples

        ```nickel
        std.record.fields_with_opts { one = 1, two = 2 } =>
          [ "one", "two" ]
        std.record.fields_with_opts { one = 1, two = 2, three_opt | optional } =>
          [ "one", "two", "three_opt" ]
        ```
      "%
      = fun r => %record/fields_with_opts% r,

    values
      : forall a. { _ : a } -> Array a
      | doc m%"
        Returns an array containing the values of all the fields of a record.

        # Examples

        ```nickel
        std.record.values { one = 1, world = "world" } =>
          [ 1, "world" ]
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

        ```nickel
        std.record.has_field "hello" { one = 1, two = 2 }
          => false
        std.record.has_field "one" { one = 1, two = 2 }
          => true
        std.record.has_field "two_opt" { one, two_opt | optional }
          => false
        ({ one = 1 } | {one, two_opt | optional })
        |> std.record.has_field "two_opt"
          => false
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

        ```nickel
        std.record.has_field_with_opts "hello" { one = 1, two = 2 }
          => false
        std.record.has_field_with_opts "one" { one = 1, two = 2 }
          => true
        std.record.has_field_with_opts "two_opt" { one, two_opt | optional }
          => true
        ({ one = 1 } | {one, two_opt | optional })
        |> std.record.has_field_with_opts "two_opt"
          => true
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

        ```nickel
        std.record.get "one" { one = 1, two = 2 } =>
          1
        { one = 1, two = 2, string = "three"}
        |> std.record.to_array
        |> std.array.filter (fun { field, value } => std.is_number value)
        |> std.record.from_array
        |> std.record.get "two"
          => 2
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

        ```nickel
        std.record.get_or "tree" 3 { one = 1, two = 2 }
          => 3
        std.record.get_or "one" 11 { one = 1, two = 2 }
          => 1
        std.record.get_or "value" "default" { tag = 'Hello, value }
          => "default"
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

          ```nickel
          std.record.insert "foo" 5 { bar = "bar" }
            => { foo = 5, bar = "bar }
          {}
          |> std.record.insert "file.txt" "data/text"
          |> std.record.insert "length" (10*1000)
            => {"file.txt" = "data/text", "length" = 10000}
          std.record.insert "already_there_opt" 0 {already_there_opt | optional}
            => {already_there_optional = 0}
          std.record.insert "already_there" 0 {already_there = 1}
            => error
          ```
        "%%
      = fun field content r => %record/insert% field r content,

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

          ```nickel
          std.record.insert_with_opts "foo" 5 { bar = "bar" }
            => { foo = 5, bar = "bar }
          {}
          |> std.record.insert_with_opts "file.txt" "data/text"
          |> std.record.insert_with_opts "length" (10*1000)
            => {"file.txt" = "data/text", "length" = 10000}
          std.record.insert_with_opts "already_there_optional" 0 {already_there | optional}
            => error
          std.record.insert_with_opts "already_there" 0 {already_there = 1}
            => error
          ```
        "%%
      = fun field content r => %record/insert_with_opts% field r content,

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

          ```nickel
          std.record.remove "foo" { foo = "foo", bar = "bar" }
            => { bar = "bar" }
          std.record.remove "foo_opt" {foo_opt | optional}
            => error
          std.record.remove "foo" { bar = "bar" }
            => error
          ```
        "%
      = fun field r => %record/remove% field r,

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

          ```nickel
          std.record.remove_with_opts "foo" { foo = "foo", bar = "bar" }
            => { bar = "bar" }
          std.record.remove_with_opts "foo_opt" {foo_opt | optional}
            => {}
          std.record.remove_with_opts "foo" { bar = "bar" }
            => error
          ```
        "%
      = fun field r => %record/remove_with_opts% field r,

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

          ```nickel
          std.record.update "foo" 5 { foo = "foo", bar = "bar" } =>
            { foo = 5, bar = "bar" }
          std.record.update "foo" 5 { bar = "bar" } =>
            { foo = 5, bar = "bar" }
          std.record.update "foo_opt" 5 {foo_opt | optional} =>
            {foo_opt = 5}
          ```

          # Overriding

          As opposed to overriding a value with the merge operator `&`, `update`
          will only change the specified field and won't automatically update the other
          fields which depend on it:

          ```nickel
          { foo = bar + 1, bar | default = 0 } & { bar = 1 } =>
            { foo = 2, bar = 1 }
          std.record.update "bar" 1 {foo = bar + 1, bar | default = 0 } =>
            { foo = 1, bar = 1 }
          ```
        "%
      = fun field content r =>
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

        ```nickel
        std.record.map_values (fun x => x + 1) { hi = 2 }
          => { hi = 3 }
        std.record.map_values (fun x => x + 1) { hello = 1, world = 2 }
          => { hello = 2, world = 3 }
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
          => [
            { field = "hello", value = "world" },
            { field = "foo", value = "bar" },
          ]
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
          => { hello = "world", foo = "bar" }
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

        ```nickel
        std.record.is_empty {}
          => true
        std.record.is_empty { foo = 1 }
          => false
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
          => { foo = 1, bar = 2 }
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
          => { even = 2 }
        ```
      "%
      = fun f record =>
        record
        |> to_array
        |> std.array.filter (fun { field, value } => f field value)
        |> from_array,

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
  },

  string = {
    BoolLiteral
      | doc m%"
        Contract for a string representation of a boolean, namely `true` or
        `false`.

        # Examples

        ```nickel
        ("true" | std.string.BoolLiteral) =>
          "true"
        ("hello" | std.string.BoolLiteral) =>
          error
        (true | std.string.BoolLiteral) =>
          error
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

        ```nickel
        ("+1.2" | std.string.NumberLiteral) =>
          "+1.2"
        ("5" | std.string.NumberLiteral) =>
          "5"
        (42 | std.string.NumberLiteral) =>
          error
        ```
      "%
      =
        let pattern = m%"^[+-]?(\d+(\.\d*)?(e[+-]?\d+)?|\.\d+(e[+-]?\d+)?)$"% in
        let is_num_literal = %string/is_match% pattern in
        fun l s =>
          if %typeof% s == 'String then
            if is_num_literal s then
              s
            else
              %blame% (%label/with_message% "invalid number literal" l)
          else
            %blame% (%label/with_message% "not a string" l),

    Character
      | doc m%"
        Contract for a character, i.e. a string of length 1.

        # Examples

        ```nickel
        ("e" | std.string.Character)
          => "e"
        ("#" | std.string.Character)
          => "#"
        ("" | std.string.Character)
          => error
        (1 | std.string.Character)
          => error
        ```
      "%
      = fun l s =>
        if %typeof% s == 'String then
          if length s == 1 then
            s
          else
            %blame% (%label/with_message% "length different than one" l)
        else
          %blame% (%label/with_message% "not a string" l),

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

        ```nickel
        ('Foo | std.string.Stringable)
          => 'Foo
        (false | std.string.Stringable)
          => false
        ("bar" ++ "foo" | std.string.Stringable)
          => "barfoo"
        ({foo = "baz"} | std.string.Stringable)
          => error
        ```
      "%
      =
        std.contract.from_predicate
          (
            fun value =>
              let type = std.typeof value in
              value == null
              || type == 'Number
              || type == 'Bool
              || type == 'String
              # note that `type == 'Enum` isn't sufficient, as it includes enum
              # variants
              || std.enum.is_enum_tag value
          ),

    NonEmpty
      | doc m%"
        Contract for a non-empty string.

        # Examples

        ```nickel
        ("" | std.string.NonEmpty)
          => error
        ("hi!" | std.string.NonEmpty)
          => "hi!"
        (42 | std.string.NonEmpty)
          => error
        ```
      "%
      = fun l s =>
        if %typeof% s == 'String then
          if %string/length% s > 0 then
            s
          else
            %blame% (%label/with_message% "empty string" l)
        else
          %blame% (%label/with_message% "not a string" l),

    join
      : String -> Array String -> String
      | doc m%"
        Joins an array of strings with a given separator.

        # Examples

        ```nickel
        std.string.join ", " [ "Hello", "World!" ]
          => "Hello, World!"
        std.string.join ";" ["I'm alone"]
          => "I'm alone"
        std.string.join ", " []
          => ""
        ```
      "%
      = fun sep fragments =>
        let length = %array/length% fragments in
        if length == 0 then
          ""
        else
          let first = %array/at% fragments 0 in
          let rest =
            %array/slice% 1 length fragments
            |> std.array.fold_left (fun acc s => acc ++ sep ++ s) ""
          in
          first ++ rest,

    split
      : String -> String -> Array String
      | doc m%"
        Splits a string based on a separator string.

        Note that this function never splits up Unicode extended grapheme
        clusters, even in cases where the sought value exists within one.

        # Examples

        ```nickel
        std.string.split "," "1,2,3"
          => [ "1", "2", "3" ]
        std.string.split "." "1,2,3"
          => [ "1,2,3" ]
        ```
      "%
      = fun sep s => %string/split% s sep,

    trim
      : String -> String
      | doc m%"
        Trims whitespace from the start and end of a string.

        # Examples

        ```nickel
        std.string.trim " hi  "
          => "hi"
        std.string.trim "1   2   3   "
          => "1   2   3"
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
          => [ "H", "e", "l", "l", "o" ]
        ```
      "%
      = fun s => %string/chars% s,

    uppercase
      : String -> String
      | doc m%"
        Returns the uppercase version of a string. Unicode extended grapheme
        clusters without an uppercase version are left untouched.

        # Examples

        ```nickel
        std.string.uppercase "a"
          => "A"
        std.string.uppercase "æ"
          => "Æ"
        std.string.uppercase "hello.world"
          => "HELLO.WORLD"
        ```
      "%
      = fun s => %string/uppercase% s,

    lowercase
      : String -> String
      | doc m%"
        Returns the lowercase version of a string. Unicode extended grapheme
        clusters without a lowercase version are left untouched.


        # Examples

        ```nickel
        std.string.lowercase "A"
          => "a"
        std.string.lowercase "Æ"
          => "æ"
        std.string.lowercase "HELLO.WORLD"
          => "hello.world"
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

        ```nickel
        std.string.contains "cde" "abcdef"
          => true
        std.string.contains "" "abcdef"
          => true
        std.string.contains "ghj" "abcdef"
          => false
        ```
      "%
      = fun subs s => %string/contains% s subs,

    replace
      : String -> String -> String -> String
      | doc m%"
        `replace sub repl str` replaces every occurrence of `sub` in `str` with `repl`.

        Note that this function will not replace `sub` if it exists within a
        larger unicode extended grapheme cluster.

        # Examples

        ```nickel
        std.string.replace "cd" "   " "abcdef"
          => "ab   ef"
        std.string.replace "" "A" "abcdef"
          => "AaAbAcAdAeAfA"
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

        ```nickel
        std.string.replace_regex "l+." "j" "Hello!"
          => "Hej!"
        std.string.replace_regex "\\d+" "\"a\" is not" "This 37 is a number."
          "This \"a\" is not a number."
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

        ```nickel
        std.string.is_match "^\\d+$" "123"
          => true
        std.string.is_match "\\d{4}" "123"
          => false
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
          => true
        ```

        On the other hand, in the version below, the partial application of
        `std.string.is_match "[0-9]*\\.?[0-9]+"` is evaluated once, returning a
        function capturing the compiled regexp. The regexp will only be compiled
        once and for all:

        ```nickel
        let is_number' = std.string.is_match "[0-9]*\\.?[0-9]+" in
        ["0", "42", "0.5"]
        |> std.array.all is_number'
          => true
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

        ```nickel
        std.string.find "^(\\d).*(\\d).*(\\d).*$" "5 apples, 6 pears and 0 grapes"
          => { matched = "5 apples, 6 pears and 0 grapes", index = 0, groups = [ "5", "6", "0" ] }
        std.string.find "3" "01234"
          => { matched = "3", index = 3, groups = [ ] }
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

        ```nickel
        std.string.find_all "(\\d) (\\w+)" "5 apples, 6 pears and 0 grapes"
          => [
            { groups = [ "5", "apples" ], index = 0, matched = "5 apples", },
            { groups = [ "6", "pears" ], index = 10, matched = "6 pears", },
            { groups = [ "0", "grapes" ], index = 22, matched = "0 grapes", }
          ]
        std.string.find_all "2" "123 123 123"
          => [
            { groups = [  ], index = 1, matched = "2", },
            { groups = [  ], index = 5, matched = "2", },
            { groups = [  ], index = 9, matched = "2", }
          ]
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

        ```nickel
        std.string.length "" =>
          => 0
        std.string.length "hi" =>
          => 2
        std.string.length "四字熟語" =>
          => 4
        std.string.length "👨🏾‍❤️‍💋‍👨🏻" =>
          => 1
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

        ```nickel
        std.string.substring 3 5 "abcdef" =>
          "de"
        std.string.substring 3 10 "abcdef" =>
          error
        std.string.substring (-3) 4 "abcdef" =>
          error
        ```
      "%
      = fun start end s => %string/substr% s start end,

    from
      | Stringable -> String
      | doc m%"
        Converts a stringable value to its string representation. Same as
        `std.to_string`.

        # Examples

        ```nickel
        std.string.from 42
          => "42"
        std.string.from 'Foo
          => "Foo"
        std.string.from null
          => "null"
        std.string.from {value = 0}
          => error
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
          => "42"
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
          => "MyEnum"
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
          => "true"
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
          => 123
        ```
      "%
      = fun s => %number/from_string% s,

    to_bool
      : String -> Bool
      | BoolLiteral -> Dyn
      | doc m%"
        Converts a representation of a boolean (either `true` or `false`) to that boolean.

        # Examples

        ```nickel
        std.string.to_bool "true"
          => true
        std.string.to_bool "false"
          => false
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

        ```nickel
        std.string.to_enum "Hello"
          => 'Hello
        std.string.to_enum "hey,there!"
          => '"hey,there!"
        ```
      "%
      = fun s => %enum/from_string% s,
  },

  test = {
    Assert
      | doc m%"
        A contract that checks if its argument is the boolean true.

        # Examples

        ```nickel
        1 == 2 | std.test.Assert
         => error: contract broken by a value

        1 == 1 | std.test.Assert
         => true
        ```
        "%
      = fun label value => if value then true else %blame% label,

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
         => error: contract broken by a value
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

      ```nickel
      std.is_number 1
        => true
      std.is_number "Hello, World!"
        => false
      ```
    "%
    = fun x => %typeof% x == 'Number,

  is_bool
    : Dyn -> Bool
    | doc m%"
      Checks if a value is a bool.

      # Examples

      ```nickel
      std.is_bool false
        => true
      std.is_bool 42
        => false
      ```
    "%
    = fun x => %typeof% x == 'Bool,

  is_string
    : Dyn -> Bool
    | doc m%"
      Checks if a value is a string.

      # Examples

      ```nickel
      std.is_string true
        => false
      std.is_string "Hello, World!"
        => true
      ```
    "%
    = fun x => %typeof% x == 'String,

  is_enum
    : Dyn -> Bool
    | doc m%"
      Checks if a value is an enum tag.

      # Examples

      ```nickel
      std.is_enum true
        => false
      std.is_enum `false
        => true
      ```
    "%
    = fun x => %typeof% x == 'Enum,

  is_function
    : Dyn -> Bool
    | doc m%"
      Checks if a value is a function.

      # Examples

      ```nickel
      std.is_function (fun x => x)
        => true
      std.is_function 42
        => false
      ```
    "%
    = fun x => %typeof% x == 'Function,

  is_array
    : Dyn -> Bool
    | doc m%"
      Checks if a value is an array.

      # Examples

      ```nickel
      std.is_array [ 1, 2 ]
        => true
      std.is_array 42
        => false
      ```
    "%
    = fun x => %typeof% x == 'Array,

  is_record
    : Dyn -> Bool
    | doc m%"
      Checks if a value is a record.

      # Examples

      ```nickel
      std.is_record [ 1, 2 ]
        => false
      std.is_record { hello = "Hello", world = "World" }
        => true
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
      'Other
    |]
    | doc m%"
      Returns the type of a value.

      # Examples

      ```nickel
      std.typeof [ 1, 2 ]
        => 'Array
      std.typeof (fun x => x)
        => 'Function
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

      ```nickel
      std.seq (42 / 0) 37
        => error
      std.seq (42 / 2) 37
        => 37
      std.seq { too_far = 42 / 0 } 37
        => 37
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

      ```nickel
      std.deep_seq (42 / 0) 37
        => error
      std.deep_seq (42 / 2) 37
        => 37
      std.deep_seq [1+1, { not_too_far = [42 / 0] }] 37
        => error
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
        => "2ab96390c7dbe3439de74d0c9b0b1767"
      ```
    "%
    = fun type s => %hash% type s,

  serialize
    : [| 'Json, 'Toml, 'Yaml |] -> Dyn -> String
    | doc m%"
      Serializes a value into the desired representation.

      # Examples

      ```nickel
      serialize 'Json { hello = "Hello", world = "World" } =>
        "{
          \"hello\": \"Hello\",
          \"world\": \"World\"
        }"
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
        { hello = "Hello", world = "World" }
      ```
    "%
    = fun format x => %deserialize% format x,

  to_string
    | std.string.Stringable -> String
    | doc m%"
      Converts a stringable value to a string representation. Same as
      `std.string.from`.

      # Examples

      ```nickel
      std.to_string 42 =>
        "42"
      std.to_string 'Foo =>
        "Foo"
      std.to_string null =>
        "null"
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
        std.trace: Hello, world!
        => true
      ```
    "%
    = fun msg x => %trace% msg x,

  FailWith
    | doc m%"
      A contract that always fails with the given message.

      # Examples

      ```nickel
      1 | std.FailWith "message"
        => error: contract broken by a value: message
      ```
    "%
    = fun msg label value => std.contract.blame_with_message msg label,

  fail_with
    | String -> Dyn
    | doc m%"
      Abort the evaluation with the given message. The error will be reported as
      a contract violation, as `fail_with` uses `std.FailWith` under the hood.

      # Examples

      ```nickel
      std.fail_with "message"
        => error: contract broken by a value: message
      ```
    "%
    = fun msg => null | FailWith msg,
}
