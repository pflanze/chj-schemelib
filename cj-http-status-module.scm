(requires
 gambit-interpreter-env
 cj-env
 (cj-string-util string-to-identifyer))

(exports
 ;; type:
 http-status?
 http-status-code

 http-status-name
 http-status-code->name
 http-status:is-client-error?
 http-status:is-error?
 http-status:is-info?
 http-status:is-redirect?
 http-status:is-server-error?
 http-status:is-success?
 ;;make-check-http-status-or-code
 ;;maybe-http-status:code

 http-status ;; by-symbol   or  symbol->http-status  whatever.
 
 )

(exports-on-request

 http-status-maybe-name

 )