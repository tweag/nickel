{
  run = fun s =>
    let update_dict = fun dict char =>
      if std.record.has_field char dict then
        std.record.update char (dict."%{char}" + 1) dict
      else
        std.record.insert "%{char}" 1 dict
    in
    std.array.fold_left update_dict {} (std.string.chars s)
}
