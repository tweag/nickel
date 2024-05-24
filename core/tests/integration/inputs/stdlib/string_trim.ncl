# test.type = 'pass'

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
] |> std.test.assert_all
