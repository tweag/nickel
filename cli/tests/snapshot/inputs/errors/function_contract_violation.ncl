# capture = 'stderr'
# command = ['eval']
let r = { f | Number -> Number = fun x => 'not-a-number } in r.f 7
