let C = std.contract.custom (fun label value => std.contract.check Number label "hi") in 1 | C
