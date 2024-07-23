# test.type = 'pass'

let number = std.number in
let almostEqual = fun a b => std.number.abs (a - b) < 0.001 in

[
    almostEqual (number.log 1024 2) 10,
    almostEqual (number.log 100 10) 2,
    almostEqual (number.log 10 std.number.e) 2.302585,

    almostEqual (number.cos 0) 1,
    almostEqual (number.cos number.pi) (-1),
    almostEqual (number.cos (number.pi / 2)) 0,

    almostEqual (number.sin 0) 0,
    almostEqual (number.sin (number.pi / 2)) 1,
    almostEqual (number.sin (number.pi / 4)) ((number.sqrt 2) / 2),

    almostEqual (number.tan 0) 0,
    almostEqual (number.tan number.pi) 0,
    almostEqual (number.tan (number.pi / 4)) 1,

    almostEqual (number.acos 0) (number.pi / 2),
    almostEqual (number.acos 1) 0,

    almostEqual (number.asin 0) 0,
    almostEqual (number.asin 1) (number.pi / 2),

    almostEqual (number.atan 0) 0,
    almostEqual (number.atan 1) (number.pi / 4),

    almostEqual (number.atan2 0 1) 0,
    almostEqual (number.atan2 1 0) (number.pi / 2),
    almostEqual (number.atan2 0.5 0.5) (number.pi / 4),
    almostEqual (number.atan2 0.5 (-0.5)) (3 * number.pi / 4),

    almostEqual (number.sqrt 4) 2,
    almostEqual (number.sqrt 2) 1.41421356237,
]
|> std.test.assert_all
