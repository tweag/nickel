let
  r1 = { a = "a1"; b = "b1"; c = "c1"; };
  r2 = { a = "a2"; b = "b2"; };
  r3 = { a = { a = "raa"; }; };
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
  (
    let
      a = 11;
    in
    with r1; let
      b = 22;
    in
    [ a b c ] == [ 11 22 r1.c ]
  )

  # with and nested records
  (
    with r3; a == r3.a && (with a; a == r3.a.a)
  )

  # with and rec records
  # TODO: this test is disabled because, now, when we are in a rec record, we don't populate the environment with it's fields. See how it's done for `let in` blocks in `src/nix.rs`.
  #  (
  #    with r1; rec {
  #      a = 11;
  #      v =  a + 11;
  #      s = b + c;
  #    } == { a = 11; v = 22; s = "b1c1";}
  #  )

  # with inside a function body
  # TODO: this test is disabled because, now, when we are in a function body, we don't populate the environment with it's fields. See how it's done for `let in` blocks in `src/nix.rs`.
  #  (
  #    let
  #      f = a: with r1; [a b];
  #    in f 11 == [11 r1.b]
  #  )
  # Don't forget to manage the patern matched arguments.
  #  (
  #    let
  #      f = {a, b}: with r1; [a b c];
  #    in f r2 == [r2.a r2.b r1.c]
  #  )
]
