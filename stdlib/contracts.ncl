{
    dyn = fun l t => t;

    num = fun l t => if isNum t then t else blame l;

    bool = fun l t => if isBool t then t else blame l;

    string = fun l t => if isStr t then t else blame l;

    list = fun l t => if isList t then t else blame l;

    func = fun s t l e =>
        if isFun e then
            (fun x => t (goCodom l) (e (s (chngPol (goDom l)) x)))
        else
            blame l;

    forall_var = fun sy pol l t =>
        let lPol = polarity l in
        if pol == lPol then
            unwrap sy t (blame l)
        else
            wrap sy t;

    fail = fun l t => blame (tag "Fail" l);

    row_extend = fun contr case l t =>
        if (case t) then
            t
        else
            contr (tag "NotRowExt" l) t;

    record = fun cont l t =>
        if isRecord t then
            cont {} l t
        else
            blame (tag "not a record" l);

    rec_extend = fun field contr cont acc l t =>
        if hasField field t then
            let acc = acc$[field = contr (goField field l) (t.$field)] in
            let rest = t -$ field in
            cont acc l rest
        else
            blame (tag "missing field" l);

    forall_tail = fun sy pol acc l rest =>
        let magic_fld = "_%wrapped" in
        if pol == (polarity l) then
            if rest == {} then
                if hasField magic_fld t then
                    let fail = blame (tag "polymorphic tail mismatch" l) in
                    let inner = unwrap sy (t.$magic_fld) fail in
                    merge t inner
                else
                    blame (tag "missing polymorphic part" l)
            else
                blame (tag "extra field" l)
        else
            t$[magic_fld = wrap sy rest];

    dyn_tail = fun acc l rest => merge acc rest;

    empty_tail = fun acc l rest =>
        if rest == {} then t
        else blame (tag "extra field" l)
}
