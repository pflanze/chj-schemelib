# Error handling

## Classical

The Classical Scheme based error handling consists of `#f` as a value to represent nothing (routines returning those to express nothing, or a failure, are generally carrying `maybe-` as part of their name). And of throwing exceptions via `error` (which creates an exception value then throws it) or (Gambit specific) `raise` (which throws the value given as the argument).

## New-fangled

There is an explicit [`Maybe`](../Maybe.scm) type, as well as a [`Result`](../Result.scm) type.

Monadic infrastructure isn't ready yet to handle those nicely.

Also, how to give access via debugger? Okay, just capture the continuation in the error object? (if set up to, via some global, hm, costly parameter for per-thread? or TLS?), right?

What is the value again of Maybe and Result monads over parameterize and call/cc in "the Scheme monad" (where the error value is explicitly `raise`d at the call side and thus propagated until a handling site iff a continuation is called, or more generally, simply "handled" by the code specified by "the monad runner"). The difference is that using bind/monadic syntax determines the type and connection in a way that propagates the error; versus the continuation; that the handler has access to the continuation of the error site. "It's pretty equivalent really" (well, doesn't help much, but: provide syntax to convert between the two / handle both; also, Perl 6?).

## Booleans and Nothing with failure information

Predicates should be able to report why/where it failed. See `failure.scm` and `failing.scm` (? XXX), but this should be integrated with the rest of the error handling. I.e. booleans that contain information. Now, could just (use `.boolean` or) `.failure?` (and `.success?`?) methods (and `or`, `and` etc. in a namespace that can be chosen via simple macro, and those would do `(eq? v #f)` etc. checks as optimization before calling a method). What should the type before those methods be though? `Boolean`? But what is the difference to `Result`, really? Not necessarily errors?, does Haskell have a smarter naming with left and right there? Cases where boolean true is the error (classic way: `#f` is fine, anything else is an error message)? TODO.

Now, should `Nothing` also be able to carry information? Probably? And implement `.failure?` as true?

## Fitting into a whole

There is `error?` (an interface) and `error+?` (including Gambit's builtin) for testing for exception values. 

TODO:

- should those be called `exception` instead, and remain separate from `Error`, ehr, `Result`?

- or is just the way how those are thrown or raised different?

- but since there is no `Ok` for those, the union with OK types is `any` (the type is not nesting). Hence no predicate possible to check for safe threading. Hence only `Result` can be used for monadic threading, well, all can, but only `Result` can be used to provide safety between code and data for whether threading should happen. Well yes: `exception` are the values that always thread? Ah actually no: they don't, they require `raise` before they start to thread. Also, the turning from flying to value is different (catch versus bind--see "provide syntax to convert" above).

