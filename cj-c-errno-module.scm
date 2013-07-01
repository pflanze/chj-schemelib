(requires
 (cj-env warn pp-through &)
 (cj-list-util map-iota list-join)
 (srfi-1 filter-map)
 cj-inline
 ;; checks only:
 (cj-string-util string-strip-until-last-chars)
 )

(compile #t)

(exports-macros
 ;; define-inline:
 check-not-posix-exception
 error-to-posix-exception
 ;; define-macro:
 define/check
 define/check->integer
 
 )


(exports-on-request
 strerror

 posix-exception-errno
 posix-exception-message
 posix-exception?
 posix-exception ;; "creator" but with cache
 ;; a way to wrap a standard message (with function name/args/values)
 ;; around the exception. Maybe use check-not-posix-exception instead:
 throw-posix-exception 
 
 )