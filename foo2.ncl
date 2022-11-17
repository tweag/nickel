{
    foo = "Overridden",
} & ({
  config.output = {},
} & {
  foo | default = "blub",

  config.output.value = foo,
})

# after the first merge:
# { foo = %foo, config = %config }
# [ %foo <- NormalThunk("blub" | default, []),
# [ %config <- RevertibleThunk(%config1 & %config2, RevertibleThunks[])
#   [ %config1 = RevertibleThunk({output.value2 = {foo}}, [])
#   [ %config2 = RevertibleThunk(output.value = foo), []]
#   )
# ]
#
# What we want is that %config is Revertible and overridable, so that subsequent
# merge propagate through the operantds of `&` inside config.
#
# %config <- RevertibleThunk(exp, env)
# (exp,env) ~ %config1 & %config2
# overriding env propagates through config1 and config2
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# [let x = 1 in x] @ [let x = 2 in x] ~> [%1, %2] {%1 <- (x, {x <- 1}), %2 <- (x, {x <- 2})
# [1 + 1, 2 + 2] ~> let %1 = 1 + 1 in let %2 = 2 + 2 in [%1, %2] / share_normal_form
# env: Identifier -> Closures
#
# merge field1 field2 => %n <- Thunk((%field1,env1) & (%field2, env2))
# 
# field ~ (field1,env1) & (field2,env2)
#
# { foo = {bar = 1}, bar = {foo = 2}, 
#
# Revertible iff at least of %fieldn is revertible
#
# field1 ~ self => ...
# field ~ exp
# field1 & field ~ fun self => 
#
# {config = {output = {value = foo, blarg = %2}}, foo | default = "blub"} & {foo = "overridden"}}
