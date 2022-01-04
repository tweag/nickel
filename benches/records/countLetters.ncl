{
  run = fun s =>
    let updateDict = fun dict char =>
      if records.has_field char dict then
        dict -$ char & {"#{char}" = dict."#{char}" + 1}
      else
        dict & {"#{char}" = 1} in
    lists.foldl updateDict {} (strings.chars s)
}
