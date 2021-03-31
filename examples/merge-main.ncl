// Merge several blocks into one final configuration. In a real world case, one
// would also want contracts to validate the shape of the data.
let server = import "merge-part1.ncl" in
let security = import "merge-part2.ncl" in
// Disabling firewall in the final result
server & security & {firewall.enabled = false}
