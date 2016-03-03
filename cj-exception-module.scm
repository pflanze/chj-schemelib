(requires
 )

(exports
 ;; -- exception type: --
 
 
 ;; -- exception/continuation type: --

 ;; constructors and predicate:
 with-exception/continuation-catcher
 exception/continuation?
 
 ;; accessors:
 exception/continuation-exception
 exception/continuation-continuation

 ;; formatting:  to strings or ports
 exception/continuation-contextline ;; ,y
 exception/continuation-contextlines ;; ,b
 exception/continuation-message-in-context
 ;; (only exn, no cont necessary really:)
 exception/continuation-text
 ;; (delegate:)
 ;;exception/continuation-kind not interesting, can only return "ERROR"

 ;; extractors:  --those should take backindex integer value as optional!
 exception/continuation-procedure ;; cool! really returns the calling procedure as value.
 exception/continuation-locat ;; cool! the location, e.g. '#((stdin) 3407873)

 repl-within-exception/continuation

 exception/continuation->u8vector
 )

(exports-on-request

 make-exception/continuation
 )

