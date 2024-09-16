# test.type = 'error'
# eval = 'full'
#
# [test.metadata]
# error = 'EvalError::BlameError'

# Regression test for https://github.com/tweag/nickel/issues/1700
let outer = {
  spec
    | {
      hostAliases | Number,
      containers | Number,
    },
}
in

let inner = {
  spec
    | {
      hostAliases,
      containers
        | Array {
          ..
        },
    },
}
in

std.deep_seq
  (
    {
      spec = {
        hostAliases = "this should fail",
        containers = [
          {
            name = "test",
            image = "nginx",
            ports = [
              {
                containerPort = 80,
                name = "http"
              }
            ],
          }
        ]
      }
    } | inner | outer
  )
  true
