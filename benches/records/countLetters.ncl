{
  run = fun s =>
    let updateDict = fun dict char =>
      if records.hasField dict char then
        dict -$ char & {"#{char}" = dict."#{char}" + 1}
      else
        dict & {"#{char}" = 1} in
    list.foldl updateDict {} (strings.chars s)
}
