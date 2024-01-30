### /offsets.ncl
{
  foo = "𐑄𐐮𐑅 𐐰𐑊𐑁𐐰𐐺𐐯𐐻 𐐮𐑆 𐐿𐐭𐑊", bar = 1
}.bar
### # The "bar" in the first line is at lsp "character" 45--48,
### # because lsp measures characters in UTF-16 codepoints 🤦
###
### [[request]]
### type = "GotoDefinition"
### textDocument.uri = "file:///offsets.ncl"
### position = { line = 2, character = 3 }
###
### [[request]]
### type = "References"
### textDocument.uri = "file:///offsets.ncl"
### position = { line = 1, character = 46 }
### context = { includeDeclaration = true }
