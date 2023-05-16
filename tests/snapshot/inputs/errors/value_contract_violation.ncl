# capture = 'stderr'
# command = []
{ foo | std.FailWith "no reason" = 1 }.foo

