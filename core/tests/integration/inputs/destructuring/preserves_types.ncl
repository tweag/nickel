# test.type = 'pass'
let test_cases : _ = {
  "destructuring let binding preserves types" =
    let some_record: { a : Number, b : String, c : Number -> Number } = { a = 1, b = "test", c = fun n => n } in
    let { a, b, c } = some_record in
    { a_num = a, a_str = b, the_id_fn = c } : { a_num : Number, a_str : String, the_id_fn : Number -> Number },

  "destructuring let binding infers types" =
    let some_record = { a = 1, b = "test", c = fun n => n } in
    let { a, b, c } = some_record in
    { a_num = a, a_str = b, the_id_fn = c } : { a_num : Number, a_str : String, the_id_fn : Number -> Number },

  "destructuring function args preserves types" =
    let dstrct : { a : Number, b : String } -> { num : Number, str : String } =
      fun { a, b } => { num = a, str = b }
    in
    let r :  { a : Number, b : String } = { a = 1, b = "" } in
    dstrct r : { num : Number, str : String },

  "destructuring function args infers types" =
    let dstrct = fun { a, b } => { num = a, str = b } in
    let r = { a = 1, b = "" } in
    dstrct r : { num : Number, str: String },

  "nested destructuring preserves types" =
    let { a = { b, c }} = { a = { b : Number = 1, c : String = "" }} in
    { num = b, str = c } : { num : Number, str : String },

  "nested destructuring infers types" =
    (let { a = { b, c }} = { a = { b = 1, c = "" }} in
    { num = b, str = c }) : { num : Number, str : String },

  "destructuring rest pattern removes matched rows" =
    let some_record : { a : Number, b : String, c : Bool } = { a = 1, b = "", c = true } in
    let { b, ..ac } = some_record in
    ac : { a: Number, c: Bool },

  "destructuring rest pattern infers correct type" =
    let some_record = { a = 1, b = "", c = fun x => x + 1 } in
    let { b, ..ac } = some_record in
    ac : { a : Number, c : Number -> Number },

  "destructuring rest pattern preserves tail type" =
    let f : forall z. { x: Number, y: Number; z } -> { y: Number; z } = fun { x, ..rest } => rest in
    (f { x = 1, y = 2, z = 3 }): { y : Number, z: Number },

  "destructuring rest pattern infers tail type" =
    let f = fun { x, ..rest } => rest in
    (f { x = "a", y = "b", z = 105}) : { y : String, z : Number },

  # Note: we need to annotate `a` on the right-hand side of the binding
  #       because we don't currently have a subtyping rule like:
  #         `{ f_1: T, ..., f_N: T } <: {_ : T}`
  #       If you're reading this and we _do_ have such a subtyping rule
  #       (e.g. after RFC003 has been implemented) then it should be
  #       safe to remove that type annotation from this test case.
  "destructuring with explicit types" =
    let { a : { _ : Number } } = { a: { _ : Number } = { b = 1 } } in
    a : { _ : Number },

  "destructuring with contracts" =
    let { a | { _ : Number } } = { a = 1 } in
    a : Number,
} in true
