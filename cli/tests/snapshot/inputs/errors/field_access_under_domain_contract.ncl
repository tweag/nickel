# capture = 'stderr'
# command = ['eval']
let f | { name | String } -> String = fun x => x.name in f {}
