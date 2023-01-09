# pyckel

Python bindings to use Nickel.

## Install

```shell
pip install pyckel
```

## Use

```python
import pyckel

result = pyckel.run("let x = 1 in { y = x + 2 }")
print(result)
# {
#   "y": 3
# }
```
