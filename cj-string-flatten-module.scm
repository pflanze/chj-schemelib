(requires
 cj-env
 (srfi-1 fold fold-right);;;
 (cj-string string-copy! @string-copy!_end)
 cj-test
 )

;(compile #t)

(exports
 flat-append-strings
 )

(exports-on-request
 flat-string-length
 )
