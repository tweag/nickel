let Assert = fun l x => x || %blame% l in

// accesses
(lists.elemAt 1 [1,2,3] == 2 | #Assert) &&
(lists.elemAt 1 (lists.map (fun x => x + 1) [1,2,3]) == 3 | #Assert) &&

// length
(lists.length [] == 0 | #Assert) &&
(lists.length [1,2,3] == 3 | #Assert) &&
(lists.length ([] @ [1,2] @ [3,4] @ []) == 4 | #Assert) &&

// Test case added after https://github.com/tweag/nickel/issues/154
(let x = 1 in let l = [x] @ [2] in %head% l == 1 | #Assert) &&

(let Y = fun f => (fun x => f (x x)) (fun x => f (x x)) in
  let foldr_ =
    fun self => fun f => fun acc => fun l =>
      if %length% l == 0 then acc
      else
        let h = %head% l in
        let t = %tail% l in
        let next_acc = self f acc t in
        f next_acc h in
  let foldr = Y foldr_ in
  let and : Bool -> Bool -> Bool =
      fun x => fun y =>
          if x then
              if y then true else false
          else false
  in
  let all = fun pred => fun l => foldr and true (%map% l pred) in
  let isZ = fun x => x == 0 in
  all isZ [0, 0, 0, 1]
  == false
  | #Assert) &&


 true
