# test.type = 'pass'
#
# # Regression test for #2059, where importing toml would fail on nested tables
(import "imported/nested_table.toml").foo.bar.qux == 42
