# test.type = 'pass'
(import "imported/circular_imports.ncl") == { a = 1, b = 1 }