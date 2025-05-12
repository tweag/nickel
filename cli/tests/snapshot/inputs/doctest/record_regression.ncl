# capture = 'all'
# command = ['test']

# Regression test for #2248
let types = {
  ServiceKind = [| 'Dns |],
  mk_Server | ServiceKind -> { kind }
    = fun kind_  => { kind = kind_ },
}
in
{
  MyDnsServer = types.mk_Server 'Dns
}
