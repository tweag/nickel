let test_cases : _ = {
  "destructuring let binding preserves types" =
    let some_record: { a : Num, b : Str, c : Num -> Num } = { a = 1, b = "test", c = fun n => n } in
    let { a, b, c } = some_record in
    { a_num = a, a_str = b, the_id_fn = c } : { a_num : Num, a_str : Str, the_id_fn : Num -> Num },

  "destructuring let binding infers types" =
    let some_record = { a = 1, b = "test", c = fun n => n } in
    let { a, b, c } = some_record in
    { a_num = a, a_str = b, the_id_fn = c } : { a_num : Num, a_str : Str, the_id_fn : Num -> Num },

  "destructuring function args preserves types" =
    let dstrct : { a : Num, b : Str } -> { num : Num, str : Str } =
      fun { a, b } => { num = a, str = b }
    in
    let r :  { a : Num, b : Str } = { a = 1, b = "" } in
    dstrct r : { num : Num, str : Str },

  "destructuring function args infers types" =
    let dstrct = fun { a, b } => { num = a, str = b } in
    let r = { a = 1, b = "" } in
    dstrct r : { num : Num, str: Str },

  "nested destructuring preserves types" =
    let { a = { b, c }} = { a = { b : Num = 1, c : Str = "" }} in
    { num = b, str = c } : { num : Num, str : Str },

  "nested destructuring infers types" =
    (let { a = { b, c }} = { a = { b = 1, c = "" }} in
    { num = b, str = c }) : { num : Num, str : Str },

  "destructuring rest pattern removes matched rows" =
    let some_record : { a : Num, b : Str, c : Bool } = { a = 1, b = "", c = true } in
    let { b, ..ac } = some_record in
    ac : { a: Num, c: Bool },

  "destructuring rest pattern infers correct type" =
    let some_record = { a = 1, b = "", c = fun x => x + 1 } in
    let { b, ..ac } = some_record in
    ac : { a : Num, c : Num -> Num },

  "destructuring rest pattern preserves tail type" =
    let f : forall z. { x: Num, y: Num; z } -> { y: Num; z } = fun { x, ..rest } => rest in
    (f { x = 1, y = 2, z = 3 }): { y : Num, z: Num },

  "destructuring rest pattern infers tail type" =
    let f = fun { x, ..rest } => rest in
    (f { x = "a", y = "b", z = 105}) : { y : Str, z : Num },

  # Note: we need to annotate `a` on the right-hand side of the binding
  #       because we don't currently have a subtyping rule like:
  #         `{ f_1: T, ..., f_N: T } <: {_ : T}`
  #       If you're reading this and we _do_ have such a subtyping rule
  #       (e.g. after RFC003 has been implemented) then it should be
  #       safe to remove that type annotation from this test case.
  "destructuring with explicit types" =
    let { a : { _ : Num } } = { a: { _ : Num } = { b = 1 } } in
    a : { _ : Num },

  "destructuring with contracts" =
    let { a | { _ : Num } } = { a = 1 } in
    a : Num,
} in true
