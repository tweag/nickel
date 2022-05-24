
{
  run = import "deploy.ncl",
  serialize.run = fun args => (
        builtin.serialize `Json (run args)
    )
}
