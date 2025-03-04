let Linktype = {
  source | { file },
  target
    | Dyn
    | { file | default = source.file }
}
in
{
  source = { file = "" },
  target = {},
} | Linktype
