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
# programmatically, but then error reporting and the LSP would be impacted (in
# particular, breaking the contract of a stdlib function would point to
# generated code that is nowhere accessible to the user) in a bad way.
#
# Another possiblity is to have special imports, like `<std/array.ncl>`. This is
# a possible avenue.
#
# In the meantime, we choose to have this one big monolithic `std` module. This
# choice doesn't negatively impact Nickel users (the file can be browsed looking
# for error locations, typechecking and the LSP are happy because no code is
# progammatically generated). It's just harder to maintain.
#
# Typing
#
# Stdlib modules shouldn't recursively refer to other modules directly, they
# should use the stdlib provided in the environment instead. This means using
# `std.array` from within the `record` module instead of just `array`. While the
# latter could work in principle, typechecking mutually recursive records is
# just harder, and the chances are this won't typecheck.
#
# On the other hand, the `std` module in the environment is properly typed, as
# the interface is elaborated from individual annotations.
{
  array = {
    NonEmpty
      | doc m%"
          Enforces that an array is not empty.

          # Examples

          ```nickel
          ([] | NonEmpty) =>
            error
          ([ 1 ] | NonEmpty) =>
            [ 1 ]
          ```
        "%
      = fun label value =>
        if %typeof% value == `Array then
          if %length% value != 0 then
            value
          else
            %blame% (%label_with_message% "empty array" label)
        else
          %blame% (%label_with_message% "not a array" label),

    first
      : forall a. Array a -> a
      | doc m%"
          Returns the first element of the given array.

          # Examples

          ```nickel
          first [ "this is the head", "this is not" ] =>
            "this is the head"
          ```
        "%
      = fun array => %elem_at% array 0,

    last
      : forall a. Array a -> a
      | doc m%"
          Returns the last element of the given array.

          # Examples

          ```nickel
          last [ "this is the head", "this is not" ] =>
            "this is not"
          ```
        "%
      = fun array => %elem_at% array (%length% array - 1),

    drop_first
      : forall a. Array a -> Array a
      | doc m%"
          Returns the given array without its first element.

          # Examples

          ```nickel
          drop_first [ 1, 2, 3 ] =>
            [ 2, 3 ]
          ```
        "%
      = fun array => %array_slice% 1 (%length% array) array,

    drop_last
      : forall a. Array a -> Array a
      | doc m%"
          Returns the given array without its last element.

          # Examples

          ```nickel
          drop_last [ 1, 2, 3 ] =>
            [ 1, 2 ]
          ```
        "%
      = fun array => %array_slice% 0 (%length% array - 1) array,

    length
      : forall a. Array a -> Number
      | doc m%"
          Returns the length of the given array.

          # Examples

          ```nickel
          length [ "Hello,", " World!" ] =>
            2
          ```
        "%
      = fun l => %length% l,

    map
      : forall a b. (a -> b) -> Array a -> Array b
      | doc m%"
          Applies a function to every element in the given array. That is,
          ```nickel
          map f [ x1, x2, ..., xn ] =>
            [ f x1, f x2, ..., f xn ]
          ```

          # Examples

          ```nickel
          map (fun x => x + 1) [ 1, 2, 3 ] =>
            [ 2, 3, 4 ]
          ```
        "%
      = fun f l => %map% l f,

    at
      : forall a. Number -> Array a -> a
      | doc m%"
          Retrieves the n-th element from an array, with indices starting at 0.

          # Examples

          ```nickel
          at 3 [ "zero" "one" "two" "three" "four" ] =>
            "three"
          ```
        "%
      = fun n l => %elem_at% l n,

    concat
      : forall a. Array a -> Array a -> Array a
      | doc m%"
          Appends the second array to the first one.

          # Examples

          ```nickel
            concat [ 1, 2, 3 ] [ 4, 5, 6 ] =>
              [ 1, 2, 3, 4, 5, 6 ]
          ```
        "%
      = fun l1 l2 => l1 @ l2,

    fold_left
      : forall a b. (a -> b -> a) -> a -> Array b -> a
      | doc m%"
          Folds a function over an array. Folds serve a similar purpose to loops or
          iterators in a functional language like Nickel. `fold_left` iterates over
          an array, by repeatedly applying a function over each element, threading
          an additional arbitrary state (the accumulator, of type `a` in the
          signature).

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
        let length = %length% array in
        if length == 0 then
          acc
        else
          let rec go = fun acc n =>
            if n == length then
              acc
            else
              let next_acc =
                %elem_at% array n
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
          over an array, by repeatedly applying a function to each element and
          threading an additional arbitrary state (the accumulator of type `a` in
          the signature).

          `fold_right f init [x1, x2, ..., xn]` results in `f x1 (f x2 (... (f xn init) ...))`.

          # Left vs right

          Folds come in two variants, left and right. How to decide which one to
          use? Please refer to the documentation of `fold_left`.

          # Examples

          ```nickel
          fold_right (fun e acc => acc @ [e]) [] [ 1, 2, 3 ] =>
            ((([] @ [3]) @ [2]) @ [1]) =>
            [ 3, 2, 1 ]
          ```
        "%
      = fun f fst l =>
        let length = %length% l in
        let rec go = fun n =>
          if n == length then
            fst
          else
            go (n + 1)
            |> f (%elem_at% l n)
        in go 0,

    prepend
      : forall a. a -> Array a -> Array a
      | doc m%"
          Builds an array given the first element and the rest of the array.

          # Examples

          ```nickel
          prepend 1 [ 2, 3 ] =>
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
          append 3 [ 1, 2 ] =>
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
          reverse [ 1, 2, 3 ] =>
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
          filter (fun x => x <= 3) [ 4, 3, 2, 5, 1 ] =>
            [ 3, 2, 1 ]
          ```
        "%
      = fun pred l => fold_left (fun acc x => if pred x then acc @ [x] else acc) [] l,

    flatten
      : forall a. Array (Array a) -> Array a
      | doc m%"
          Concatenates all elements of a given array of arrays.

          # Examples

          ```nickel
          flatten [[1, 2], [3, 4]] =>
            [1, 2, 3, 4]
          ```
        "%
      = fun l => fold_right (fun l acc => l @ acc) [] l,

    all
      : forall a. (a -> Bool) -> Array a -> Bool
      | doc m%"
          Returns true if all elements in the given array satisfy the predicate, false otherwise.

          # Examples

          ```nickel
          all (fun x => x < 3) [ 1, 2 ] =>
            true
          all (fun x => x < 3) [ 1, 2 3 ] =>
            false
          ```
        "%
      = fun pred l => fold_right (fun x acc => if pred x then acc else false) true l,

    any
      : forall a. (a -> Bool) -> Array a -> Bool
      | doc m%"
          Returns true if at least one element in the given array satisfies the predicate, false otherwise.

          # Examples

          ```nickel
          any (fun x => x < 3) [ 1, 2, 3, 4 ] =>
            true
          any (fun x => x < 3) [ 5, 6, 7, 8 ] =>
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
          Returns true if the given value appears in the array, false otherwise.

          # Examples

          ```nickel
          elem 3 [ 1, 2, 3, 4, 5 ] =>
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
          partition (fun x => x < 5) [ 2, 4, 5, 3, 7, 8, 6 ] =>
            { right = [ 3, 4, 2 ], wrong = [ 6, 8, 7, 5 ] }
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
      | doc m%"
          `generate f n` returns an array of length `n` by applying `f` to the integers from 0 to n-1:
          ```nickel
          generate f n = [ f 0, f 1, ..., f (n - 1) ]
          ```

          # Examples

          ```nickel
          generate (fun x => x * x) 4 =>
            [ 0, 1, 4, 9 ]
          ```
        "%
      = fun f n => %generate% n f,

    sort
      : forall a. (a -> a -> [| `Lesser, `Equal, `Greater |]) -> Array a -> Array a
      | doc m%"
          Sorts the given array based on the provided comparison operator.

          # Examples

          ```nickel
          sort (fun x y => if x < y then `Lesser else if (x == y) then `Equal else `Greater) [ 4, 5, 1, 2 ] =>
            [ 1, 2, 4, 5 ]
          ```
        "%
      #TODO: maybe inline partition to avoid contract checks?
      = fun cmp array =>
        let length = %length% array in
        let first = %elem_at% array 0 in
        let rest = %array_slice% 1 length array in
        let parts = partition (fun x => (cmp x first == `Lesser)) rest in
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
          flat_map (fun x => [x, x]) [1, 2, 3]
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
          intersperse ", " [ "Hello", "wonderful", "world!" ]
            => [ "Hello", ", ", "wonderful", ", ", "world!" ]
          intersperse ", " [ "Hello" ]
            => [ "Hello" ]
          intersperse ", " []
            => []
          ```
        "%
      = fun v array =>
        let length = %length% array in
        if length <= 1 then
          array
        else
          let first = %elem_at% array 0 in
          let rest = %array_slice% 1 length array in
          [first] @ (flat_map (fun a => [ v, a ]) rest),

    slice
      : forall a. Number -> Number -> Array a -> Array a
      | doc m%"
          `slice start end array` returns the slice of `array` between `start` (included) and
          `end` (excluded).

          # Preconditions

          In `slice start end value`, `start` and `end` must be positive
          integers such that `0 <= start <= end <= std.array.length value`.

          # Examples

          ```nickel
          slice 1 3 [ 0, 1, 2, 3, 4, 5]
            => [ 1, 2 ]
          slice 0 3 [ "Hello", "world", "!" ]
            => [ "Hello", "world", "!" ]
          slice 2 3 [ "Hello", "world", "!" ]
            => [ "!" ]
           ```
        "%
      = fun start end value => %array_slice% start end value,

    split_at
      : forall a. Number -> Array a -> { left : Array a, right : Array a }
      | doc m%"
          Splits an array in two at a given index, and puts all the elements
          to the left of the element at the given index (excluded) in the
          `left` field, and the rest of the array in the `right` field.

          # Preconditions

          In `split_at inded value`, `index` must be a positive integer such
          that `0 <= index <= std.array.length value`.

          # Examples

          ```nickel
          split_at 2 [ 0, 1, 2, 3, 4, 5]
            => { left = [ 2, 3, 4, 5 ], right = [ 0, 1 ] }
          split_at 0 [ "Hello", "world", "!" ]
            => { left = [  ], right = [ "Hello", "world", "!" ] }
          split_at 3 [ "Hello", "world", "!" ]
            => { left = [ "Hello", "world", "!" ], right = [  ] }
          ```
        "%
      = fun index value =>
        {
          left = %array_slice% 0 index value,
          right = %array_slice% index (%length% value) value
        },

    replicate
      : forall a. Number -> a -> Array a
      | doc m%"
          `replicate n x` creates an array containing `x` exactly `n` times.

          # Preconditions

          `n` must be an integer greater or equal to `0`.

          # Examples

          ```nickel
          replicate 0 false
            => [ ]
          replicate 5 "x"
            => [ "x", "x", "x", "x", "x" ]
          ```
        "%
      = fun n x => %generate% n (fun _i => x),

    range_step
      : Number -> Number -> Number -> Array Number
      | doc m%"
          `range_step start end step` generates the array of numbers
          `[start, start + step, start + 2*step, ..]` up to the first element
          (excluded) larger than or equal to `end`

          # Preconditions

          In `range_step start end step`, `start` and `end` must satisfy `start
          <= end`. `step` must be strictly greater than `0`.

          # Examples

          ```nickel
          range_step (-1.5) 2 0.5
           => [ -1.5, -1, -0.5, 0, 0.5, 1, 1.5 ]
          ```
        "%
      = fun start end step =>
        %generate%
          ((end - start) / step |> std.number.floor)
          (fun i => start + i * step),

    range
      : Number -> Number -> Array Number
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
          range 0 5
           => [ 0, 1, 2, 3, 4 ]
          ```
        "%
      = fun start end => range_step start end 1,

    reduce_left
      : forall a. (a -> a -> a) -> Array a -> a
      | doc m%"
          Reduces the elements to a single one, by repeatedly applying a reducing
          operation. If the array is empty, returns an empty array.

          `reduce_left` associates to the left, that is
          `reduce_left op [x1, x2, ..., xn]` results in `op (... (op (op x1 x2) x3) ...) xn`.

          `reduce_left` is the same as `fold_left`, but uses the first element as
          the initial accumulator.

          # Left vs right

          The rationale to decide between `fold_left` and `fold_right` applies to
          `reduce_left` and `reduce_right` as well. See the documentation of
          `fold_left`.

          # Examples

          ```nickel
          reduce_left (@) [ [1, 2], [3], [4,5] ]
            => (([1, 2] @ [3]) @ [4,5])
            => [ 1, 2, 4, 5 ]
          reduce_left (-) [ 1, 2, 3, 4]
            => ((1 - 2) - 3) - 4
            => -8
          ```
        "%
      = fun f array =>
        let first = %elem_at% array 0 in
        let rest = %array_slice% 1 (%length% array) array in
        fold_left f first rest,

    reduce_right
      : forall a. (a -> a -> a) -> Array a -> a
      | doc m%"
          Reduces the elements to a single one, by repeatedly applying a reducing
          operation. If the array is empty, returns an empty array.

          `reduce_right` associates to the right, that is
          `reduce_right op [x1, x2, ..., xn]` results in
          `op x1 (op x2 (... (op xn-1 xn) ...))`.

          `reduce_right` is the same as `fold_right`, but uses the last element as
          the initial element.

          # Left vs right

          The rationale to decide between `fold_left` and `fold_right` applies to
          `reduce_left` and `reduce_right` as well. See the documentation of
          `fold_left`.

          # Examples

          ```nickel
          reduce_right (@) [ [1, 2], [3], [4,5] ]
            => [1, 2] @ ([3] @ [4,5])
            => [ 1, 2, 4, 5 ]
          reduce_right (-) [ 1, 2, 3, 4]
            => 1 - (2 - (3 - 4))
            => -2
          ```
        "%
      = fun f array =>
        let last_index = %length% array - 1 in
        let last = %elem_at% array last_index in
        let rest = %array_slice% 0 last_index array in
        fold_right f last rest,
  },

  contract = {
    Equal
      | doc m%"
        `Equal` is a contract enforcing equality to a given constant.

        `Equal` is lazy over arrays and records. When checking such values,
        `Equal` doesn't test for equality of elements right away (it just tests
        that the size is the same for arrays and that fields are the same for
        records), but returns a new value where an equality contract has been
        pushed down each element.

        # Example

        ```nickel
        1 + 4 | Equal 5
         => 5
        4 | Equal 5
         => ERROR (contract broken by a value)
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
          let plural = if %length% fields == 1 then "" else "s" in
          ctr_label
          |> label.with_message "%{qualifier} field%{plural} `%{std.string.join ", " fields}`"
          |> label.append_note "`std.contract.Equal some_record` requires that the checked value is equal to the record `some_record`, but the sets of their fields differ."
          |> blame
        in
        fun constant =>
          let constant_type = %typeof% constant in
          let check_typeof_eq = fun ctr_label value =>
            let value_type = %typeof% value in
            if value_type == constant_type then
              value
            else
              ctr_label
              |> label.with_message "expected `%{%to_str% constant_type}`, got `%{%to_str% value_type}`"
              |> label.append_note "`std.contract.Equal some_value` requires that the checked value is equal to `some_value`, but they don't have the same type."
              |> blame
          in
          constant_type
          |> match {
            `Record =>
              # we map the constant from {field1 = val1, .., fieldn = valn} to
              # {field1 = Equal val1, .., fieldn = Equal valn}, building a
              # dictionary of equality contracts
              let contract_map = %record_map% constant (fun _key => Equal) in
              fun ctr_label value =>
                let value = check_typeof_eq ctr_label value in
                let diff = fields_diff constant value in
                if %length% diff.extra != 0 then
                  blame_fields_differ "extra" diff.extra ctr_label
                else if %length% diff.missing != 0 then
                  blame_fields_differ "missing" diff.missing ctr_label
                else
                  %record_lazy_assume%
                    ctr_label
                    value
                    (
                      fun field =>
                        contract_map."%{field}"
                    ),
            `Array =>
              fun ctr_label value =>
                let value = check_typeof_eq ctr_label value in
                let value_length = %length% value in
                if value_length == %length% constant then
                  %generate%
                    value_length
                    (
                      fun i =>
                        %elem_at% value i
                        |> std.contract.apply (Equal (%elem_at% constant i)) ctr_label
                    )
                else
                  ctr_label
                  |> label.with_message "array length mismatch (expected `%{%to_str% (%length% constant)}`, got `%{%to_str% value_length})`"
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
              std.contract.blame_with_message_message "Not zero" label
          in

          0 | IsZero
          ```
        "%
      = fun message label => %blame% (%label_with_message% message label),

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

    label
      | doc m%"
          The label submodule gathers functions that manipulate the label of
          a contract.

          A label is a special opaque value automatically passed by the Nickel
          interpreter to contracts when performing a contract check.

          A label stores a stack of custom error diagnostics, that can be
          manipulated by the functions in this module. Labels thus offer a way to
          customize the error message that will be shown if a contract is broken.

          The setters (`with_XXX` functions) always operate on the current error
          diagnostic, which is the last diagnotic on the stack. If the stack is
          empty, a fresh diagnostic is silently created when using a setter.
          The previous diagnostics are thus archived, and can't be modified
          anymore. All diagnostics will be shown during error reporting, with
          the most recent being used as the main one.

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
                  In this case, the previous diagnostic has been stacked,
                  and using `with_message` won't erase anything but rather modify
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
          = fun message label => %label_with_message% message label,

        with_notes
          | doc m%"
              Attaches custom error notes to the current error diagnostic of a
              label.

              Type: `Array String -> Label -> Label`
              (for technical reasons, this function isn't actually statically typed)

              If custom error notes were previously set, there are two
              possibilities:
                - the label has gone through a `std.contract.apply` call in-between.
                  In this case, the previous diagnostic has been stacked,
                  and using `with_notes` won't erase anything but rather modify
                  the fresh current diagnostic.
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
          # the %label_with_notes% operator expects an array of strings which is
          # fully evaluated, thus we force the notes first
          = fun notes label => %label_with_notes% (%force% notes) label,

        append_note
          | doc m%"
              Appends a note to the error notes of the current diagnostic of a label.

              Type: `String -> Label -> Label`
              (for technical reasons, this function isn't actually statically typed)
            "%
          = fun note label => %label_append_note% note label,
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
        %assume% contract (%label_push_diag% label) value,
  },

  enum = {
    Tag
      | doc m%"
        Enforces that the value is an enum tag.

        # Examples

        ```nickel
        (`foo | Tag) =>
          `foo
        (`FooBar | Tag) =>
          `FooBar
        ("tag" | Tag) =>
          error
        ```
      "%
      = std.contract.from_predicate std.is_enum,

    TagOrString
      | doc m%%"
        Accepts both enum tags and strings. Strings are automatically
        converted to an enum tag.

        `TagOrString` is typically used in conjunction with an enum type, to
        accept tags represented as strings (e.g. coming from a JSON
        serialization) as well.

        # Examples

        ``` nickel
        let Schema = {
          protocol
            | enum.TagOrString
            | [| `http, `ftp |],
          port
            | Number,
          method
            | enum.TagOrString
            | [| `GET, `POST |]
        } in
        let serialized =
          m%"
            {"protocol": "http", "port": 80, "method": "GET"}
          "%
          |> std.deserialize `Json
        in

        serialized | Schema
        ```
      "%%
      = fun label value =>
        %typeof% value
        |> match {
          `String => %enum_from_str% value,
          `Enum => value,
          _ =>
            std.contract.blame_with_message
              "expected either a string or an enum tag"
              label,
        },
  },

  function = {
    id
      : forall a. a -> a
      | doc m%"
        The identity function, that is

        ```nickel
        id x = x
        ```

        for any value `x`.
      "%
      = fun x => x,

    const
      : forall a b. a -> b -> a
      | doc m%"
        Always returns the first argument, ignoring the second.

        # Examples

        ```nickel
        const 5 42 == 5
        ```
      "%
      = fun x y => x,

    compose
      : forall a b c. (b -> c) -> (a -> b) -> (a -> c)
      | doc m%"
        Function composition, from right to left. That is,

        ```nickel
        compose f g x == f (g x)
        ```
      "%
      = fun g f x => x |> f |> g,

    flip
      : forall a b c. (a -> b -> c) -> b -> a -> c
      | doc m%%"
        Flips the argument order for a two-argument function.

        # Examples

        ```nickel
        flip (fun x y => "%{x} %{y}") "world!" "Hello,"
          => "Hello, world!"
        ```
      "%%
      = fun f x y => f y x,

    first
      : forall a b. a -> b -> a
      | doc m%"
        Always returns the first argument, ignoring the second.

        # Examples

        ```nickel
        first 5 7 => 5
        ```
      "%
      = const,

    second
      : forall a b. a -> b -> b
      | doc m%"
        Always returns the second argument, ignoring the first.

        # Examples

        ```nickel
        second 5 7 => 7
        ```
      "%
      = flip const,

    pipe
      : forall a. a -> Array (a -> a) -> a
      | doc m%%"
        Applies an array of functions to a value, in order.

        # Examples

        ```nickel
        pipe 2 [ (+) 2, (+) 3 ]
          => 7
        pipe `World [ std.string.from, fun s => "Hello, %{s}!" ]
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
        if %typeof% value == `Number then
          if value % 1 == 0 then
            value
          else
            %blame% (%label_with_message% "not an integer" label)
        else
          %blame% (%label_with_message% "not a number" label),

    Nat
      | doc m%"
        Enforces that a number is a natural number (including 0).

        # Examples

        ```nickel
        (42 | Nat) =>
          42
        (0 | Nat) =>
          0
        (-4 | Nat) =>
          error
        ```
      "%
      = fun label value =>
        if %typeof% value == `Number then
          if value % 1 == 0 && value >= 0 then
            value
          else
            %blame% (%label_with_message% "not a natural" label)
        else
          %blame% (%label_with_message% "not a number" label),

    PosNat
      | doc m%"
        Enforces that a number is a strictly positive natural number.

        # Examples

        ```nickel
        (42 | PosNat) =>
          42
        (0 | PosNat) =>
          error
        (-4 | PosNat) =>
          error
        ```
      "%
      = fun label value =>
        if %typeof% value == `Number then
          if value % 1 == 0 && value > 0 then
            value
          else
            %blame% (%label_with_message% "not positive integer" label)
        else
          %blame% (%label_with_message% "not a number" label),

    NonZero
      | doc m%"
        Enforces that a number is not equal to zero.

        # Examples

        ```nickel
        (1 | NonZero) =>
          1
        (0.0 | NonZero) =>
          error
        ```
      "%
      = fun label value =>
        if %typeof% value == `Number then
          if value != 0 then
            value
          else
            %blame% (%label_with_message% "non-zero" label)
        else
          %blame% (%label_with_message% "not a number" label),

    is_integer
      : Number -> Bool
      | doc m%"
        Checks if the given number is an integer.

        # Examples

        ```nickel
        is_int 42 =>
          true
        is_int 1.5 =>
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
        min (-1337) 42 =>
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
        max (-1337) 42 =>
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
        floor 42.5 =>
          42
        floor (-42.5) =>
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
        abs (-5) =>
          5
        abs 42 =>
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
        fract 13.37 =>
          0.37
        fract 42 =>
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
        truncate (-13.37) =>
          -13
        truncate 42.5 =>
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
        pow 2 8 =>
          256
        ```

        # Precision

        Nickel numbers are arbitrary precision rationals. If the exponent `y` is
        an integer which fits into a 64-bit signed or unsigned integer (that is, if
        `y` is an integer between `−2^63` and `2^64-1`), the result is computed
        exactly.

        Otherwise, both operands `x` and `y` are converted to the nearest 64 bit
        float (excluding `NaN` and infinity), and we compute the result as a 64
        bit float. This result is then converted back to a rational. In this
        case, **be aware that both the conversion from rationals to floats, and
        the power operation, might incur rounding errors**.
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
        map (fun s x => s) { hi = 2 } =>
          { hi = "hi" }
        map (fun s x => x + 1) { hello = 1, world = 2 } =>
          { hello = 2, world = 3 }
        ```
      "%
      = fun f r => %record_map% r f,

    fields
      : forall a. { _ : a } -> Array String
      | doc m%"
        Returns an array containing the names of all the fields of a record.

        # Examples

        ```nickel
        fields { one = 1, two = 2 } =>
          [ "one", "two" ]
        ```
      "%
      = fun r => %fields% r,

    values
      : forall a. { _ : a } -> Array a
      | doc m%"
        Returns an array containing the values of all the fields of a record.

        # Examples

        ```nickel
        values { one = 1, world = "world" }
          [ 1, "world" ]
        ```
      "%
      = fun r => %values% r,

    has_field
      : forall a. String -> { _ : a } -> Bool
      | doc m%"
        Given a string, checks if a record contains a field of that name.

        # Examples

        ```nickel
        has_field "hello" { one = 1, two = 2 } =>
          false
        has_field "one" { one = 1, two = 2 } =>
          true
        ```
      "%
      = fun field r => %has_field% field r,

    insert
      : forall a. String -> a -> { _ : a } -> { _ : a }
      | doc m%%"
          Inserts a new field into a record. `insert` doesn't mutate the original
          record but returns a new one instead.

          # Examples

          ```nickel
          insert "foo" 5 { bar = "bar" } =>
            { foo = 5, bar = "bar }
          {}
          |> insert "file.%{ext}" "data/text"
          |> insert "length" 10*1000 =>
            {"file.txt" = "data/text", "length" = 10000}
          ```
        "%%
      = fun field content r => %record_insert% field r content,

    remove
      : forall a. String -> { _ : a } -> { _ : a }
      | doc m%"
          Removes a field from a record. `remove` doesn't mutate the original
          record but returns a new one instead.

          # Examples

          ```nickel
          remove "foo" { foo = "foo", bar = "bar" } =>
            { bar = "bar }
          ```
        "%
      = fun field r => %record_remove% field r,

    update
      : forall a. String -> a -> { _ : a } -> { _ : a }
      | doc m%"
          Updates a field of a record with a new value. `update` doesn't mutate the
          original record but returns a new one instead. If the field to update is absent
          from the given record, `update` adds it.

          # Examples

          ```nickel
          update "foo" 5 { foo = "foo", bar = "bar" } =>
            { foo = 5, bar = "bar" }
          update "foo" 5 { bar = "bar" } =>
            { foo = 5, bar = "bar" }
          ```

          # Overriding

          As opposed to overriding a value with the merge operator `&`, `update`
          will only change the specified field and won't automatically update the other
          fields which depend on it:

          ```nickel
          { foo = bar + 1, bar | default = 0 } & { bar = 1 } =>
            { foo = 2, bar = 1 }
          update "bar" 1 {foo = bar + 1, bar | default = 0 } =>
            { foo = 1, bar = 1 }
          ```
        "%
      = fun field content r =>
        let r =
          if %has_field% field r then
            %record_remove% field r
          else
            r
        in
        %record_insert% field r content,

    map_values
      : forall a b. (a -> b) -> { _ : a } -> { _ : b }
      | doc m%"
        Maps a function over the values of all the fields of a record.

        # Examples

        ```nickel
        map_values (fun x => x + 1) { hi = 2 } =>
          { hi = 3 }
        map (fun x => x + 1) { hello = 1, world = 2 } =>
          { hello = 2, world = 3 }
        ```
      "%
      = fun f => map (fun _field value => f value),

    to_array
      : forall a. { _ : a } -> Array { field : String, value : a }
      | doc m%"
        Converts a record to an array of key-value pairs.

        # Examples

        ```nickel
        to_array { hello = "world", foo = "bar" } =>
          [ { field = "hello", value = "world" },
            { field = "foo", value = "bar" }
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
        Converts an array of key-value pairs into a record.

        # Examples

        ```nickel
        from_array [ { field = "hello", value = "world" },
                     { field = "foo", value = "bar" }
                   ]
          => { hello = "world", foo = "bar" }
        ```
      "%
      = fun bindings =>
        bindings
        |> std.array.map (fun binding => { "%{binding.field}" = binding.value })
        |> merge_all,

    is_empty
      : forall a. { _ : a } -> Bool
      | doc m%"
        Checks whether a record is empty.

        # Examples

        ```nickel
        is_empty {} => true
        is_empty { foo = 1 } => false
        ```
      "%
      = (==) {},

    merge_all
      | forall a. Array { _ : a } -> { _ : a }
      | doc m%"
        Merges an array of records.

        # Examples

        ```nickel
        merge_array [ { foo = 1 }, { bar = 2 } ]
          => { foo = 1, bar = 2 }
        ```
      "%
      = std.array.fold_left (&) {},

    filter
      : forall a. (String -> a -> Bool) -> { _ : a } -> { _ : a }
      | doc m%"
        `filter f r` returns a record containing all fields from `r` for which
        `f` returns true. The function `f` is passed the name and value of each field
        to make a decision.

        # Examples

        ```nickel
        filter (fun name x => x % 2 == 0) { even = 2, odd = 3 }
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
        Enforces that the value is a string that represents a boolean
        literal, namely
        - "True" or "true", which will be normalized to "true"; or
        - "False" or "false", which will be normalized to "false".

        # Examples

        ```nickel
        ("True" | BoolLiteral) =>
          "true"
        ("hello" | BoolLiteral) =>
          error
        (true | BoolLiteral) =>
          error
        ```
      "%
      = fun l s =>
        if %typeof% s == `String then
          if s == "true" || s == "True" then
            "true"
          else if s == "false" || s == "False" then
            "false"
          else
            %blame% (%label_with_message% "expected \"true\" or \"false\", got %{s}" l)
        else
          %blame% (%label_with_message% "not a string" l),

    NumberLiteral
      | doc m%"
        Enforces that the value is a string that represents a numerical value.

        # Examples

        ```nickel
        ("+1.2" | NumberLiteral) =>
          "+1.2"
        ("5" | NumberLiteral) =>
          "5"
        (42 | NumberLiteral) =>
          error
        ```
      "%
      =
        let pattern = m%"^[+-]?(\d+(\.\d*)?(e[+-]?\d+)?|\.\d+(e[+-]?\d+)?)$"% in
        let is_num_literal = %str_is_match% pattern in
        fun l s =>
          if %typeof% s == `String then
            if is_num_literal s then
              s
            else
              %blame% (%label_with_message% "invalid number literal" l)
          else
            %blame% (%label_with_message% "not a string" l),

    Character
      | doc m%"
        Enforces that the value is a character, i.e. a string of length 1.

        # Examples

        ```nickel
        ("e" | Character) =>
          "e"
        ("#" | Character) =>
          "#"
        ("" | Character) =>
          error
        (1 | Character) =>
          error
        ```
      "%
      = fun l s =>
        if %typeof% s == `String then
          if length s == 1 then
            s
          else
            %blame% (%label_with_message% "length different than one" l)
        else
          %blame% (%label_with_message% "not a string" l),

    Stringable
      | doc m%"
        Enforces that the value is convertible to a string via
        `std.to_string` or `std.string.from`. Accepted values are:

        - numbers
        - booleans
        - strings
        - enum tags
        - null

        # Examples

        ```nickel
        (`Foo | Stringable) =>
          `Foo
        (false | Stringable) =>
          false
        ("bar" ++ "foo" | Stringable) =>
          "barfoo"
        ({foo = "baz"} | Stringable) =>
          error
        ```
      "%
      =
        std.contract.from_predicate
          (
            fun value =>
              let type = std.typeof value in
              value == null
              || type == `Number
              || type == `Bool
              || type == `String
              || type == `Enum
          ),

    NonEmpty
      | doc m%"
        Enforces that the value is a non-empty string.

        # Examples

        ```nickel
        ("" | NonEmpty) =>
          error
        ("hi!" | NonEmpty) =>
          "hi!"
        (42 | NonEmpty) =>
          error
        ```
      "%
      = fun l s =>
        if %typeof% s == `String then
          if %str_length% s > 0 then
            s
          else
            %blame% (%label_with_message% "empty string" l)
        else
          %blame% (%label_with_message% "not a string" l),

    join
      : String -> Array String -> String
      | doc m%"
        Joins an array of strings with a given separator.

        # Examples

        ```nickel
        join ", " [ "Hello", "World!" ] =>
          "Hello, World!"
        ```
      "%
      = fun sep fragments =>
        let length = %length% fragments in
        if length == 0 then
          ""
        else
          let first = %elem_at% fragments 0 in
          let rest =
            %array_slice% 1 length fragments
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
        split "," "1,2,3" =>
          [ "1", "2", "3" ]
        split "." "1,2,3" =>
          [ "1,2,3" ]
        ```
      "%
      = fun sep s => %str_split% s sep,

    trim
      : String -> String
      | doc m%"
        Trims whitespace from the start and end of a string.

        # Examples

        ```nickel
        trim " hi  " =>
          "hi"
        trim "1   2   3   " =>
          "1   2   3"
        ```
      "%
      = fun s => %str_trim% s,

    characters
      : String -> Array String
      | doc m%"
        Separates a string into its individual Unicode extended grapheme clusters.

        # Examples

        ```nickel
        chars "Hello" =>
          [ "H", "e", "l", "l", "o" ]
        ```
      "%
      = fun s => %str_chars% s,

    uppercase
      : String -> String
      | doc m%"
        Returns the uppercase version of the given character (including non-ascii characters) if it exists, the same
        character if not.

        # Examples

        ```nickel
        uppercase "a" =>
          "A"
        uppercase "æ" =>
          "Æ"
        uppercase "." =>
          "."
        ```
      "%
      = fun s => %str_uppercase% s,

    lowercase
      : String -> String
      | doc m%"
        Returns the lowercase version of the given character (including non-ascii characters) if it exists, the same
        character if not.

        # Examples

        ```nickel
        lowercase "A" =>
          "a"
        lowercase "Æ" =>
          "æ"
        lowercase "." =>
          "."
        ```
      "%
      = fun s => %str_lowercase% s,

    contains
      : String -> String -> Bool
      | doc m%"
        Checks if the first string is part of the second string.

        Note that this function returns false if the sought string exists entirely inside
        or straddles Unicode extended grapheme clusters.

        # Examples

        ```nickel
        contains "cde" "abcdef" =>
          true
        contains "" "abcdef" =>
          true
        contains "ghj" "abcdef" =>
          false
        ```
      "%
      = fun subs s => %str_contains% s subs,

    replace
      : String -> String -> String -> String
      | doc m%"
        `replace sub repl str` replaces every occurrence of `sub` in `str` with `repl`.

        Note that this function will not replace `sub` if it exists within a
        larger unicode extended grapheme cluster.

        # Examples

        ```nickel
        replace "cd" "   " "abcdef" =>
          "ab   ef"
        replace "" "A" "abcdef" =>
          "AaAbAcAdAeAfA"
        ```
      "%
      = fun pattern replace s =>
        %str_replace% s pattern replace,

    replace_regex
      : String -> String -> String -> String
      | doc m%"
        `replace_regex regex repl string` replaces every match of `regex` in `string` with `repl`.

        **WARNING**: this function will currently break up Unicode extended
        grapheme clusters if the regex matches a codepoint that exists within
        one. This behaviour is expected to change in a future version.

        # Examples

        ```nickel
        replace_regex "l+." "j" "Hello!" =>
          "Hej!"
        replace_regex "\\d+" "\"a\" is not" "This 37 is a number." =>
          "This \"a\" is not a number."
      ```
      "%
      = fun pattern replace s =>
        %str_replace_regex% s pattern replace,

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
        is_match "^\\d+$" "123" =>
          true
        is_match "\\d{4}" "123" =>
          false
        ```

        # Performance

        When checking multiple strings against a common regular expression, it
        is advantageous to store a partially applied version of this function. This
        partial evaluation will store a compiled version of the regular expression and
        prevent recompilation at each call site.

        For example, in the following program, the whole call to
        `std.string.is_match "[0-9]*\\.?[0-9]+ x"` is re-evaluated at each invocation of
        `is_number`. The regexp will be recompiled 3 times in total:

        ```nickel
        let is_number = fun x => std.string.is_match "[0-9]*\\.?[0-9]+" x in
        ["0", "42", "0.5"] |> std.array.all is_number =>
          true
        ```

        On the other hand, in the version below, the partial application of
        `std.string.is_match "[0-9]*\\.?[0-9]+"` is evaluated once, returning a
        function capturing the compiled regexp. The regexp will only be compiled
        once and for all:

        ```nickel
        let is_number' = std.string.is_match "[0-9]*\\.?[0-9]+" in
        ["0", "42", "0.5"] |> std.array.all is_number' =>
          true
        ```
      "%
      = fun regex => %str_is_match% regex,

    find
      : String -> String -> { matched : String, index : Number, groups : Array String }
      | doc m%"
        `find regex string` looks for matches of `regexp` in `string`. Returns
        the part of `string` that matched, the index of the first character that
        was part of the match in `string`, and an array of all capture groups if
        there were any.

        **WARNING**: this function will currently return incorrect indices if
        `str` contains Unicode extended grapheme clusters. The indices returned
        will be the location of the matching codepoints, rather than the
        matching grapheme clusters.

        # Examples

        ```nickel
        find "^(\\d).*(\\d).*(\\d).*$" "5 apples, 6 pears and 0 grapes" =>
          { matched = "5 apples, 6 pears and 0 grapes", index = 0, groups = [ "5", "6", "0" ] }
        find "3" "01234" =>
          { matched = "3", index = 3, groups = [ ] }
        ```

        # Performance

        Note that this function may perform better by sharing its partial application between multiple calls,
        because in this case the underlying regular expression will only be compiled once (see the documentation
        of `std.string.is_match` for more details).
      "%
      = fun regex => %str_find% regex,

    length
      : String -> Number
      | doc m%"
        Returns the length of the string, as measured by the number of Unicode
        [extended grapheme clusters](https://unicode.org/glossary/#extended_grapheme_cluster).

        Generally speaking, this gives the number of "visible" glyphs in the string.

        # Examples

        ```nickel
        length "" =>
          0
        length "hi" =>
          2
        length "四字熟語" =>
          4
        length "👨🏾‍❤️‍💋‍👨🏻" =>
          1
        ```
      "%
      = fun s => %str_length% s,

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
        substring 3 5 "abcdef" =>
          "de"
        substring 3 10 "abcdef" =>
          error
        substring (-3) 4 "abcdef" =>
          error
        ```
      "%
      = fun start end s => %str_substr% s start end,

    from
      | Stringable -> String
      | doc m%"
        Converts a stringable value to its string representation. Same as
        `std.to_string`.

        # Examples

        ```nickel
        from 42 =>
          "42"
        from `Foo =>
          "Foo"
        from null =>
          "null"
        ```
      "%
      = fun x => %to_str% x,

    from_number
      | Number -> String
      | doc m%"
        Converts a number to its string representation.

        # Examples

        ```nickel
        from_number 42 =>
          "42"
        ```
      "%
      = from,

    from_enum
      | std.enum.Tag -> String
      | doc m%"
        Converts an enum variant to its string representation.

        # Examples

        ```nickel
        from_enum `MyEnum =>
          "MyEnum"
        ```
      "%
      = from,

    from_bool
      | Bool -> String
      | doc m%"
        Converts a boolean value to its string representation.

        # Examples

        ```nickel
        from_bool true =>
          "true"
        ```
      "%
      = from,

    to_number
      | NumberLiteral -> Number
      | doc m%"
        Converts a string that represents a number to that number.

        # Examples

        ```nickel
        to_number "123" =>
          123
        ```
      "%
      = fun s => %num_from_str% s,

    to_bool
      | BoolLiteral -> Bool
      | doc m%"
        Converts a string that represents a bool to that bool.

        # Examples

        ```nickel
        to_bool "true" =>
          true
        to_bool "True" =>
          true
        to_bool "false" =>
          false
        ```
      "%
      = fun s => s == "true",

    to_enum
      | String -> std.enum.Tag
      | doc m%"
        Converts any string that represents an enum variant to that enum variant.

        # Examples

        ```nickel
        to_enum "Hello" =>
          `Hello
        ```
      "%
      = fun s => %enum_from_str% s,
  },

  is_number
    : Dyn -> Bool
    | doc m%"
      Checks if a value is a number.

      # Examples

      ```nickel
      is_number 1 =>
        true
      is_number "Hello, World!" =>
        false
      ```
    "%
    = fun x => %typeof% x == `Number,

  is_bool
    : Dyn -> Bool
    | doc m%"
      Checks if a value is a bool.

      # Examples

      ```nickel
      is_bool false =>
        true
      is_bool 42 =>
        false
      ```
    "%
    = fun x => %typeof% x == `Bool,

  is_string
    : Dyn -> Bool
    | doc m%"
      Checks if a value is a string.

      # Examples

      ```nickel
      is_string true =>
        false
      is_string "Hello, World!" =>
        true
      ```
    "%
    = fun x => %typeof% x == `String,

  is_enum
    : Dyn -> Bool
    | doc m%"
      Checks if a value is an enum tag.

      # Examples

      ```nickel
      is_enum true =>
        false
      is_enum `false =>
        true
      ```
    "%
    = fun x => %typeof% x == `Enum,

  is_function
    : Dyn -> Bool
    | doc m%"
      Checks if a value is a function.

      # Examples

      ```nickel
      is_function (fun x => x) =>
        true
      is_function 42 =>
        false
      ```
    "%
    = fun x => %typeof% x == `Function,

  is_array
    : Dyn -> Bool
    | doc m%"
      Checks if a value is an array.

      # Examples

      ```nickel
      is_array [ 1, 2 ] =>
        true
      is_array 42 =>
        false
      ```
    "%
    = fun x => %typeof% x == `Array,

  is_record
    : Dyn -> Bool
    | doc m%"
    Checks if the given value is a record.
      Checks if a value is a record.

      # Examples

      ```nickel
      is_record [ 1, 2 ] =>
        false
      is_record { hello = "Hello", world = "World" } =>
        true
      ```
    "%
    = fun x => %typeof% x == `Record,

  typeof
    : Dyn -> [|
      `Number,
      `Bool,
      `String,
      `Enum,
      `Label,
      `Function,
      `Array,
      `Record,
      `Other
    |]
    | doc m%"
      Returns the type of a value.

      # Examples

      ```nickel
      typeof [ 1, 2 ] =>
        `Array
      typeof (fun x => x) =>
        `Function
      ```
    "%
    = fun x => %typeof% x,

  seq
    : forall a. Dyn -> a -> a
    | doc m%"
      `seq x y` forces the evaluation of `x`, before returning `y`.

      # Examples

      ```nickel
      seq (42 / 0) 37 =>
        error
      seq (42 / 2) 37 =>
        37
      seq { tooFar = 42 / 0 } 37 =>
        37
      ```
    "%
    = fun x y => %seq% x y,

  deep_seq
    : forall a. Dyn -> a -> a
    | doc m%"
      `deep_seq x y` forces a deep evaluation `x`, before returning `y`.

      # Examples

      ```nickel
      deep_seq (42 / 0) 37 =>
        error
      deep_seq (42 / 2) 37 =>
        37
      deep_seq { tooFar = 42 / 0 } 37 =>
        error
      ```
    "%
    = fun x y => %deep_seq% x y,

  hash
    : [| `Md5, `Sha1, `Sha256, `Sha512 |] -> String -> String
    | doc m%"
      Hashes the given string with the desired hashing algorithm.

      # Examples

      ```nickel
      hash `Md5 "hunter2" =>
        "2ab96390c7dbe3439de74d0c9b0b1767"
      ```
    "%
    = fun type s => %hash% type s,

  serialize
    : [| `Json, `Toml, `Yaml |] -> Dyn -> String
    | doc m%"
      Serializes a value into the desired representation.

      # Examples

      ```nickel
      serialize `Json { hello = "Hello", world = "World" } =>
        "{
          \"hello\": \"Hello\",
          \"world\": \"World\"
        }"
      ```
    "%
    = fun format x => %serialize% format (%force% x),

  deserialize
    : [| `Json, `Toml, `Yaml |] -> String -> Dyn
    | doc m%"
      Deserializes a string into a Nickel value from the given representation.

      # Examples

      ```nickel
      deserialize `Json "{ \"hello\": \"Hello\", \"world\": \"World\" }"
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
      from 42 =>
        "42"
      from `Foo =>
        "Foo"
      from null =>
        "null"
      ```
    "%
    = fun x => %to_str% x,

  trace
    : forall a. String -> a -> a
    | doc m%"
      `std.trace msg x` prints `msg` to standard error, then proceeds with
      the evaluation of `x`.

      # Examples

      ```nickel
      std.trace "Hello, world!" true =>
        std.trace: Hello, world!
        true
      ```
    "%
    = fun msg x => %trace% msg x,

  FailWith
    | doc m%"
      A contract that always fails with the given message.

      # Examples

      ```nickel
      1 | FailWith "message" =>
        error: contract broken by a value: message
      ```
    "%
    = fun msg label value => std.contract.blame_with_message msg label,

  fail_with
    | String -> Dyn
    | doc m%"
      Abort evaluation with the given message.

      # Examples

      ```nickel
      fail_with "message" =>
        error: contract broken by a value: message
      ```
    "%
    = fun msg => null | FailWith msg,
}
