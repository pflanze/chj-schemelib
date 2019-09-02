
Lazy evaluation means, do not evaluate an expression right away, but instead return a "promise" to it. This happens when using the `delay` form:

    > (def v (delay (/ 10 2)))
    > v
    [(debuggable-promise) #<procedure #63> #f #<continuation #64>]
    > (force v)
    5
    > (def v (delay (/ 10 0)))
    > (force v)
    cj-exception-handler: exception #65
    *** ERROR IN #<procedure #66>, (console)@48.15 -- Divide by zero
    (/ 10 0)
    1>

This is particularly useful for calculating sequences of things on demand, e.g. loading CSV records from disk or similar.

    (=> (csv-file-stream "my.csv") (.map first) list (.write-csv-file "my2.csv"))

This only holds one row at a time in memory, and releases it when advancing to the next one (although release happens only reliably when the code is compiled!).

