# capture = 'stderr'
# command = ['eval']
let test = import "../../imports/package.toml" in
test | {
  package.name | String,
}
