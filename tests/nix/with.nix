let
  r1 = { a = "a1"; b = "b1"; c = "c1"; };
  r2 = { a = "a2"; b = "b2"; };
in
[
  # simple with
  (with r1; [ a b c ] == [ r1.a r1.b r1.c ])

  # with does not shadow staticaly defined vars
  (
    let
      a = 11;
    in
    with r1; a == 11
  )

  # with shadow previous with
  (with r1; with r2; [ b c ] == [ r2.b r1.c ])

  # complex shadowing
  (
    let
      a = 11;
    in
    with r1; with r2; [ a b c ] == [ 11 r2.b r1.c ]
  )
]
