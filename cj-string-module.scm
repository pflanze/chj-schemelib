;(namespace "")
(compile #t)

(requires
 ;;cj-env ;; testing only
 cj-test ;; TEST, %try
 )

(exports
 string-list->string 
 )

(exports-on-request
 @string-copy!_len
 @string-copy!_end
 my-string-append
 string-copy!
 )