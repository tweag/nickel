let Assert = fun l x => x || %blame% l in

// basic arithmetic
(1+1 == 2 | #Assert) &&
(1-2+3-4 == -2 | #Assert) &&
(2-3-4 == -5 | #Assert) &&
(-1-2 == -3 | #Assert) &&
(2*2 + 2*3 - 2*4 == 2 | #Assert) &&
(1/2 + 1/4 - 1/8 == 0.625 | #Assert) &&
((10 + 1/4) % 3 == 1.25 | #Assert) &&
(10 + 1/4 % 3 == 10.25 | #Assert) &&
(34 + (if true then 2 else 222)
  == 36
  | #Assert) &&

// comparisons
(1 < 1 == false | #Assert) &&
(1 <= 1 == true | #Assert) &&
(1 > 1 == false | #Assert) &&
(1 >= 1 == true | #Assert) &&
(1 + 1/2 > 1 + 1/4 == true | #Assert) &&
(1 + 1/2 < 1 + 1/4 == false | #Assert) &&
(1 + 1/2 + 1/8 > 1 + 1/4 + 1/4 == true | #Assert) &&
(1 + 1/2 + 1/8 < 1 + 1/4 + 1/4 == false | #Assert) &&
(-1 - 2 < 3 - 10 == false | #Assert) &&
(-1 - 2 > 3 - 10 == true | #Assert) &&
(-1*2 > 1*2 == false | #Assert) &&
(-1*2 < 1*2 == true | #Assert) &&
(1/4 + 1/4 - 1/4 + 1/4 <= 1/2 == true | #Assert) &&
(1/4 + 1/4 - 1/4 + 1/4 < 1/2 == false | #Assert) &&
(1/4 + 1/4 - 1/4 + 1/4 >= 1/2 == true | #Assert) &&
(1/4 + 1/4 - 1/4 + 1/4 < 1/2 == false | #Assert) &&

// This test checks that the terms of a switch are closured
(let x = 3 in (switch { foo => 1, _ => x, } (3 + 2)) == 3) &&

true
