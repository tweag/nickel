let Assert = fun l x => x || %blame% l in

// left_annot_precedence
(let dummy = null in
  let LocalAssert = Assert in
  true | #LocalAssert) &&

(if false then
    null
  else
    let dummy = null in
    let LocalAssert = Assert in
    true | #LocalAssert) &&

(let f = fun x =>
    let dummy = null in
    let LocalAssert = Assert in
    x | #LocalAssert in
  f true) &&

// others_precedence
((fun x => x | #Assert -> Dyn) true) &&
(switch {Ok => true, Err => false} `Ok | #Assert) &&

true
