{
  dyn = fun l t => t;

  num = fun l t => if %isNum% t then t else %blame% l;

  bool = fun l t => if %isBool% t then t else %blame% l;

  string = fun l t => if %isStr% t then t else %blame% l;

  list = fun elt l t => if %isList% t then %map% t (elt (%goList% l)) else %blame% l;

  func = fun s t l e =>
      if %isFun% e then
          (fun x => t (%goCodom% l) (e (s (%chngPol% (%goDom% l)) x)))
      else
          %blame% l;

  forall_var = fun sy pol l t =>
      let lPol = %polarity% l in
      if pol == lPol then
          %unwrap% sy t (%blame% l)
      else
          %wrap% sy t;

  fail = fun l t => %blame% (%tag% "Fail" l);

  row_extend = fun contr case l t =>
      if (case t) then
          t
      else
          contr (%tag% "NotRowExt" l) t;

  record = fun cont l t =>
      if %isRecord% t then
          cont {} l t
      else
          %blame% (%tag% "not a record" l);

  dyn_record = fun contr l t =>
      if %isRecord% t then
          %recordMap% t (fun _field => contr l)
      else
          %blame% (%tag% "not a record" l);

  record_extend = fun field contr cont acc l t =>
      if %hasField% field t then
          let acc = acc$[field = contr (%goField% field l) (t.$field)] in
          let t = t -$ field in
          cont acc l t
      else
          %blame% (%tag% "missing field `#{field}`" l);

  forall_tail = fun sy pol acc l t =>
      let magic_fld = "_%wrapped" in
      if pol == (%polarity% l) then
          if %hasField% magic_fld t then
              let rest = (t -$ magic_fld) in
              if rest == {} then
                  let fail = %blame% (%tag% "polymorphic tail mismatch" l) in
                  let inner = %unwrap% sy (t.$magic_fld) fail in
                  acc & inner
              else
                  %blame% (%tag% "extra field `#{%head% (%fieldsOf% rest)}`" l)
          else
              %blame% (%tag% "missing polymorphic part" l)
      else
          acc$[magic_fld = %wrap% sy t];

  dyn_tail = fun acc l t => acc & t;

  empty_tail = fun acc l t =>
      if t == {} then acc
      else %blame% (%tag% "extra field `#{%head% (%fieldsOf% t)}`" l);

  contracts = {
    blame = fun l => %blame% l;
    tag = fun msg l => %tag% msg l;
  };
}
