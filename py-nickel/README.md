# pynickel

Python bindings to use Nickel.

## Install

```shell
pip install py-nickel
```

## Use

```python
import nickel

result = nickel.run("let x = 1 in { y = x + 2 }")
print(result)
# {
#   "y": 3
# }
```
