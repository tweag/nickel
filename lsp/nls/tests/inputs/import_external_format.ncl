### /external_import.json
{
    "someData": "someValue"
}
### /external_import.yaml
someData: someValue
### /external_import.ncl
let foo = import "external_import.json" in
let bar = import "external_import.yaml" in
foo & bar
### diagnostic = ["file:///external_import.ncl"]
