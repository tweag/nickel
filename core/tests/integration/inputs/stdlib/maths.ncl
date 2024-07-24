# test.type = 'pass'

let number = std.number in
let almost_equal = fun a b => std.number.abs (a - b) < 0.001 in

[
    almost_equal (number.log 1024 2) 10,
    almost_equal (number.log 100 10) 2,
    almost_equal (number.log 10 std.number.e) 2.302585,

    almost_equal (number.cos 0) 1,
    almost_equal (number.cos number.pi) (-1),
    almost_equal (number.cos (number.pi / 2)) 0,

    almost_equal (number.sin 0) 0,
    almost_equal (number.sin (number.pi / 2)) 1,
    almost_equal (number.sin (number.pi / 4)) ((number.sqrt 2) / 2),

    almost_equal (number.tan 0) 0,
    almost_equal (number.tan number.pi) 0,
    almost_equal (number.tan (number.pi / 4)) 1,

    almost_equal (number.acos 0) (number.pi / 2),
    almost_equal (number.acos 1) 0,

    almost_equal (number.asin 0) 0,
    almost_equal (number.asin 1) (number.pi / 2),

    almost_equal (number.atan 0) 0,
    almost_equal (number.atan 1) (number.pi / 4),

    almost_equal (number.atan2 0 1) 0,
    almost_equal (number.atan2 1 0) (number.pi / 2),
    almost_equal (number.atan2 0.5 0.5) (number.pi / 4),
    almost_equal (number.atan2 0.5 (-0.5)) (3 * number.pi / 4),

    almost_equal (number.sqrt 4) 2,
    almost_equal (number.sqrt 2) 1.41421356237,
]
|> std.test.assert_all
