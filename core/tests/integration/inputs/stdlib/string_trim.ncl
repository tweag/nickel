# test.type = 'pass'
let {check, ..} = import "../lib/assert.ncl" in
let {string, ..} = std in

[
  # string.trim
  string.trim "  ひげ  " == "ひげ",
  string.trim "   " == "",
  string.trim "" == "",
  # zero width spaces aren't whitespace
  string.trim "​ ​ ​ a ​ ​ ​" == "​ ​ ​ a ​ ​ ​",
  string.trim "   \n   new\nline\n" == "new\nline",
  string.trim "　   	  " == "",
  string.trim m%"
  
        a multiline string




  "% == "a multiline string",
] |> check
