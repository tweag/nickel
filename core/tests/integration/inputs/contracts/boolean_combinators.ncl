# test.type = 'pass'
let Assert = std.test.Assert in

[
  { foo = 1 } | std.contract.any_of [ String, Bool, { .. }],
  { foo = 1 }
    | std.contract.any_of
      [
        { bar | String },
        { baz | Bool },
        { foo | Number }
      ],
  true | std.contract.not (std.contract.any_of [ Number, String ]),
  let Is4Or5 =
    std.contract.any_of
      [
        std.contract.from_predicate ((==) 4),
        std.contract.from_predicate ((==) 5),
      ]
  in
  [
    3 | std.contract.not Is4Or5,
    4 | Is4Or5,
    5 | Is4Or5,
  ],
]
# This forces each element to make sure contract checks are evaluated, but we
# don't care about the precise value of `x`, and always return `true`
|> std.array.map (fun x => x == x)
|> std.test.assert_all
