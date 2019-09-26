## Notes on Intersection and Union types

There are two main works I could find on runtime checking (contract based typing) for intersection and union types, one by [Wadler et al][wadler] and another by [Keil][keil]. I list details of each below, and how they relate to Nickel's needs.

#### What we would want

The type system of Nickel is based on a powerful (as powerful as Nickel itself for now) programming language to write contracts. These contracts differ from the usual contracts on the literature in one big aspect: instead of taking a term and evaluating to a boolean value indicating if the contract is validated, they either return the value (which may be wrapped) or raise blame on the given label.

This allowed to write a function contract natively in Nickel, without the need of any special primitive operation. Ideally this language could allow to write other kinds of contracts, for instance, dependent function contract should be easily added.

This has a few major **advantages**:
 * No need for a runtime system (or primitives) to perform contracts, they are just part of the language with some (not truly needed) syntactic sugar on top.
 * Contracts definition are specified inside the language, which makes it easy to modify, extend and understand them.

And, of course, some **disadvantages**:
 * We move away from the "usual" way of doing things (not much work done this way)


#### Wadler paper

The paper by Wadler has a few advantages:
 * Intersection and union types are treated as any other two types, there's no normalization done on them.
 * Labels carry most of the magic, there's no need to have access to the context, the type or the term when evaluating. This is good because most of the logic is performed only when a blame is reached.

... and many cons:
 * They have a way to simulate "contexts" by counters on labels, these are used every time a function is called, and it's increased, this way they have a simple way to know if two negative blames correspond to the same context or not. I think this is quite bound to call by value evaluation, and may bring problems later on.
 * They don't allow for user defined flat types (contracts) since these would bring commutativity issues on Union and Intersection. We want to go even further and allow contracts to modify the term...

#### Keil paper

On the other hand, the advantages and disadvantages of the Keil paper seem to complement to those of Wadler. The pros:
 * There's no such a thing as context counter, Intersection types are delayed, so every time one of these is opened, the blame resolution will only consider the given context
 * They introduce a Drop rule which drops contracts inside other contracts evaluation.
 * They have an implementation roughly based on their ideas.

And some cons:
 * Intersections need to be delayed, but Unions don't. So they need to tranform their types into Unions of Intersections of non-Union types.


#### Next steps after Wadler and Keil

I think there are some nice properties we should try to show (at least informally)

 * If we have a term `A (B ...)` where `A` and `B` are contracts, either:
   * `B` is never evaluated, or
   * `B` is evaluated before `A`

   This would allow us to make A depend on B without fear of crazy evaluations order


#### Proposed approach

We have two ways of proceeding, as far as I understand:
 * The first would be to increase the language in some way to add dependencies to the labels, something like `check` and `set` fields. Every time we find `blame l` we first check that everything it depends on has been set, if that's true, we set everything that depends on this.
 * The second would be to provide powerful primitives per usecase, for instance, `split_union l` would generate two sublabels (that would depend on each other) that, if raised, would blame `l`.

There are still a few hard to solve challenges:
 * **Context tracking**: since negative blame depends on the context, in order to raise it we need to make sure it's the same label, and the same context, both papers solve this in different ways:
   * _Delaying intersection_: this means an intersection contract is only opened when is being evaluated, basically meaning the context tracking is taken care of by itself. However, it's not simple to understand how to deal with unions (or other contracts) under intersection, since these may need to have some kind of global state, instead of per context.
   * _Counters_: this follows from the Wadler paper, and it means we need to keep track every time a function contract is opened, and differentiating them by their labels. As said above, I'm not certain how well/bad this works on call by need.
 * **Function blaming**: This one is easier, but we need to keep in mind that a blame from a codomain contract of a function, doesn't necessarily means it needs to raise, maybe it's domain also blamed. It is important to keep in mind that this has a further complication in call by need.

I think at first the second option is easier, but if the first option is correctly implemented it would allow for some very fun stuff (for instance, something like a linear function contract) without actually extending the language implementation.

Both of these seem to need some kind of global state in the runtime system which I'm not super fond of.

#### Other implementations

The most similar implementation I could find is [TreatJS], which has a nice blame assignation based on callbacks. They allow flat contracts, but these are simpler (they can't modify the term).




[wadler]: http://homepages.inf.ed.ac.uk/wadler/papers/root-blame/root-blame.pdf
[treatjs]: https://proglang.informatik.uni-freiburg.de/treatjs/index.html
[keil]: http://matthias-keil.de/papers/icfp2015-blame.pdf
