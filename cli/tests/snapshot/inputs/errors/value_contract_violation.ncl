# capture = 'stderr'
# command = ['eval']
{ foo | std.FailWith "no reason" = 1 }.foo

