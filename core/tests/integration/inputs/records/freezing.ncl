# test.type = 'pass'
[
  (%record/freeze% { x = 1, y = x }) & { x | force = 2 } == { x = 2, y = 1 },
  (%record/freeze% { x | default = 1, y = x }) & { x = 2 } == { x = 2, y = 1 },
  (%record/freeze% { x | default = 1, y = x })
  & (%record/freeze% { x = 2, z = x })
  & { x | force = 3 } == { x = 3, y = 1, z = 2 },

  # freezing, as record mapping, flushes pending contracts and make them not
  # propagate anymore
  (%record/freeze% {x | String = "a"}) & {x | force = 1} == {x = 1},

 ]
|> std.test.assert_all
