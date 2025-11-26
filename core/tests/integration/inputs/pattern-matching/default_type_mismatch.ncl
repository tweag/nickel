# test.type = 'pass'
[] |> match { { foo ? 1 } => foo, _ => true }
