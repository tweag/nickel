{
  run = fun s =>
    let update_dict = fun dict char =>
      if record.has_field char dict then
        dict -$ char & {"%{char}" = dict."%{char}" + 1}
      else
        dict & {"%{char}" = 1} in
    array.foldl update_dict {} (string.chars s)
}
