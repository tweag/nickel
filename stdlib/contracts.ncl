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
}
