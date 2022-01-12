# Notes on sum encoding with dependent types

**This is an archive of thoughts originally written in issue [#54](https://github.com/tweag/nickel/issues/52)**


_**Current status**: Intersection and Union types ended up being much more complex than anticipated, on standby for now. We are looking for a simpler way to encode Sum types, this issue presents a possible solution._

The idea would be to encode sums by using Pi and Sigma types: `A+B` would become `Sigma b: Bool, if b then A else B`, which could be constructed with `Pi b: Bool, (if b then A else B) -> Sigma c: Bool, if c then A else B`.

For this, we'd need to implement two new kind of contracts:
 * Dependent functions, `x: A -> \x. B`, which allow the codomain contract to depend on the input, as a reference we have [1] Racket's functions contracts, [2] is also a good reference. There are two challenges/decisions:
   * _strictness_, a function that ignores it's input may evaluate it anyways if the target contract does
   * _lax vs picky_, as pointed in [3], we can decide to wrap or not the argument on the domain contract when passed to the codomain, I think it should be checked there as well.
 * Dependent records, `{idx: Bool, val: if idx then A else B}` would allow us to have Sigma types quite naturally, together with some more complex stuff.


Ideas/problems around this:
 * We may be good enough with a Pi indexed only by simple types (Bool, Num, enums), I think this would simplify some of the concerns regarding strictness. Dually, some similar restriction might be helpful for Sigma. If we also drop Num, type checking might become much easier.
 * Maybe we can get rid of Sigma, and construct them with `Pi b: Bool, (if b then A else B) -> (Pi c: Bool, if c then Bool else if b then A else B)`, where `c` is the accessor for the pair (`true == fst`, `false == snd`).
 * A big question is what to do with type checking  for these types.
 * [4] talks about some problems with dependent function contracts, I'll try to understand in more detail what they're saying and check whether it makes any difference for us.

cc @aspiwack

[1] [Racket's contracts, in particular `->i`](https://docs.racket-lang.org/reference/function-contracts.html#%28form._%28%28lib._racket%2Fcontract%2Fbase..rkt%29._-~3ei%29%29)
[2] [On Contract Satisfaction in a Higher-Order World, Dimoulas & Felleisen](http://www.cs.umd.edu/class/spring2014/cmsc631/papers/dimoulas-contract-sat.pdf)
[3] [Contracts Made Manifest, Greenberg et. al.](https://www.cis.upenn.edu/~bcpierce/papers/contracts-popl.pdf)
[4] [An Investigation of Contracts as Projections, Findler et. al.](https://newtraell.cs.uchicago.edu/files/tr_authentic/TR-2004-02.pdf)


_Update on dependent records:_

 * I couldn't find anything talking about contracts for these nor for \Sigma types, will keep looking
 * There's theory about dependent records, I only read [this one](http://www.cs.rhul.ac.uk/~zhaohui/DRT09.pdf), there's one by [R. Pollack](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=4&ved=2ahUKEwihgKDoj4XmAhUCDmMBHS6CDuAQFjADegQIBBAC&url=http%3A%2F%2Fhomepages.inf.ed.ac.uk%2Frpollack%2Fexport%2FrecordsFAC.ps.gz&usg=AOvVaw1L_v-h9GS7GAVBvnFBJzWT) that I intend to read.
 The main points from the first one is that the dependency is only backwards in the definition of the record type (order matters). I'll keep looking for anything more general, but this may be good enough.


_Other worthy paper_
[Correct Blame for Contracts, Dimoulas et al.](https://www2.ccs.neu.edu/racket/pubs/popl11-dfff.pdf)

It states the different strategies one can choose for dependent function types. The answer provided is that you can also blame the contract, since it has a context of its own.

Right now this is not very compatible (but doable) with our system, since it needs to have two different labels for subject and context blaming, instead of just one that can either be positive or negative.
