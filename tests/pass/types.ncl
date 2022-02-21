let Assert = fun l x => x || %blame% l in

(let plus : Num -> Num -> Num = fun x => fun y => x + y in
 plus (54 : Num) (6 : Num) == 60 | Assert)
