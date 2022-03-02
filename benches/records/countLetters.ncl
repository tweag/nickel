{
  run = fun s =>
    let updateDict = fun dict char =>
      if record.has_field char dict then
        dict -$ char & {"%{char}" = dict."%{char}" + 1}
      else
        dict & {"%{char}" = 1} in
    list.foldl updateDict {} (string.chars s)
}
