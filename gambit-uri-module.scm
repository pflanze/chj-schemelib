
(requires
 ;;gambit-interpreter-env
 ;;(cj-list-util list-join)
 (srfi-1 fold-right)
 cj-string
 (cj-env & aif)
 (cj-test TEST)
 )

(exports

make-uri
uri?
uri-authority
uri-authority-set!
uri-fragment
uri-fragment-set!
uri-path
uri-path-set!
uri-query
uri-query-set!
uri-scheme
uri-scheme-set!
;;btw functional setters?..  just use cj-functional-record-types underneath.after adaption to define-type.

parse-uri
parse-uri-query

string->uri
string->uri-query
encode-for-uri

;; cj:
uri->string

)
