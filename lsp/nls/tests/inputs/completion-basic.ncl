### /completion-basic.ncl
let config = {
  version = "1.2",
  verified = { really = true },
  foo = "bar",
}
in
{
    a = config.version,
    b = config.verified.really,
}
### Completion /completion-basic.ncl:7:12
### Completion /completion-basic.ncl:7:15 .
### Completion /completion-basic.ncl:8:15 .
### Completion /completion-basic.ncl:8:23
### Completion /completion-basic.ncl:8:24 .
### Completion /completion-basic.ncl:8:27
