# capture = 'stderr'
# command = ['eval']
let f = fun { one, two, one } => { one, two }
in f { one = 1, two = "2" }
