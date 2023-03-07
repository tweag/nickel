{
  run = fun s =>
    let update_dict = fun dict char =>
      if record.has_field char dict then
        record.update char (dict."%{char}" + 1) dict
      else
        record.insert "%{char}" 1 dict in
    array.fold_left update_dict {} (string.chars s)
}
