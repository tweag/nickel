let {Assert, ..} =  import "testlib.ncl" in

let block1 = {
  foo | default = 1,
  bar = 1,
  baz | force = 1,
  x | priority 10 = 1,
  y | priority -5 = 1,
  z | priority 0 = 1,
  d | default = 1,
} in

let block2 = {
  foo | priority -10 = 2,
  bar | priority 10 = 2,
  baz = 2,
  x | priority 11 = 2,
  y  = 2,
  z | priority 10 = 2,

} in 

let block3 = {
  foo | priority -10.1 = 3,
  bar | default = 3,
  baz | priority 1000 = 3,
  x | priority 12 = 3,
  y | priority -1 = 3,
  z | priority 50 = 3,
} in

block1 & block2 & block3
== {
  foo = 2,
  bar = 2,
  baz = 1,
  x = 3,
  y = 2,
  z = 3,
  d = 1,
} | Assert
