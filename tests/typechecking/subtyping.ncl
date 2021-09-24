let typecheck = [
  // records
 (let r = {foo = 1, bar = 2} in
  let id = fun x => x in
  id r) : {_ : Num},
  (({foo = 1, bar = "a"} : {foo : Num, bar : Str}) : {foo : Num | Dyn}) : {_: Dyn},

  // record ops
  (%valuesOf% {foo = 1, bar = 2}) : List,
  (%fieldsOf% {foo = 1, bar = "a"}) : List Str,
  (let f = fun field val => field in
   let r = {foo = true, bar = false} in
   %recordMap% r f) : {_ : Str},

  // lists
  [1, "a", {foo = null}] : List,
  (["a", "b", "c"] : List Str) : List,
  [{foo = 1}, {bar = 2}] : List {_ : Num},

  // functions
  (let m : Dyn -> Dyn -> Dyn = fun x y => x & y in
   m 1 2) : Dyn,
  (let higherOrder : ({foo : Num} -> {_ : Num}) -> Dyn = fun f => f {foo = 1} in
   let id : {_ : Num} -> {bar : Num} = fun x => {bar = 1} in
   higherOrder id) : Dyn,
   ((fun f => f 0) : (Num -> Num) -> Num) : (Dyn -> Num) -> Dyn,
] in

true
