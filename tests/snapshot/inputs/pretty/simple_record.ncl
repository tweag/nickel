{ 
  a | Number | default = 1, 
  b : String | force = "some long string that goes past the 80 character line limit for pretty printing", 
  c : { x : Number, y: Number } = { x = 999.8979, y = 500 },
  d | Array string.NonEmpty = ["a", "list", "of", "non", "empty", "strings"],
}
