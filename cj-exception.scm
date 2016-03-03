;; Copyright 2006-2008 Christian Jaeger
;; Published unter the same terms as Gambit: dual LGPL
;; version 2.1 or Apache version 2.0 license


;; todo wha'ts their new safe name? none?
(define (cmd-b cont port depth)
  (if (and (##continuation? cont)
	   (port? port)
	   (fixnum? depth))
      (##cmd-b cont port depth)
      (error "cmd-b: invalid argument types of cont port depth:" cont port depth)))

(define (cmd-y cont port pinpoint? depth)
  (if (and (##continuation? cont)
	   (port? port)
	   (fixnum? depth))
      (##cmd-y cont port pinpoint? depth)
      (error "cmd-y: invalid argument types of cont port depth:" cont port depth)))

(define-type exception/continuation
  id: 4bad9e82-f84c-4ae4-9ba7-c8964bf3dffc
  exception
  continuation)


(define (to-port-or-string maybe-port fn)
  (if maybe-port
      (fn maybe-port)
      (with-output-to-string "" (lambda () (fn (current-output-port))))))


(define (exception/continuation-contextline e #!optional port)
  (to-port-or-string
   port
   (lambda (port)
     (cmd-y (exception/continuation-continuation e)
	    port
	    #f
	    0))))


(define (exception/continuation-contextlines e #!optional port)
  (to-port-or-string
   port
   (lambda (port)
     (cmd-b (exception/continuation-continuation e)
	    port
	    0))))


(define (exception/continuation-message-in-context e #!optional port)
  (to-port-or-string
   port
   (lambda (port)
     (display-exception-in-context (exception/continuation-exception e)
				   (exception/continuation-continuation e)
				   port))))


(define (exception/continuation-procedure e)
  (##exception->procedure
   (exception/continuation-exception e)
   (exception/continuation-continuation e)))


(define (exception/continuation-locat e)
  (##exception->locat
   (exception/continuation-exception e)
   (exception/continuation-continuation e)))


;; delegates:

(define (exception/continuation-text e #!optional port)
  (to-port-or-string
   port
   (lambda (port)
     (display-exception
      (exception/continuation-exception e)
      port))))


(define (repl-within-exception/continuation e)
  (if (exception/continuation? e)
      (##repl-within (exception/continuation-continuation e)
		     "repl-within-exception/continuation")
      ;; ^ don't know where the 2nd argument to ##repl-within is used
      (error " not a exception/continuation:" e)))


;; serialisation:

(define-type exception/continuation&string
  id: d3a6b590-3d09-48e2-99e3-01e076126796
  exception/continuation
  string)


(define (exception/continuation->serialisation-object e)
  (make-exception/continuation&string
   e
   (exception/continuation-contextlines e)))


(define (exception/continuation->u8vector e)
  (object->u8vector
   (exception/continuation->serialisation-object e)))


(define (u8vector->backtrace vec)
  (exception/continuation&string-string
   (u8vector->object vec)))


(define (with-exception/continuation-catcher handler th)
  (continuation-capture
   (lambda (cont)
     (with-exception-handler
      (lambda (e)
	(continuation-capture
	 (lambda (c)
	   (continuation-graft
	    cont
	    handler
	    (make-exception/continuation e c)))))
      th))))


;; example: 
;;  create an exception object with the continuation and 
;;  raise this in the context of with-ec-catcher

(define (with-ec-catch&rethrow thunk)
  (with-exception/continuation-catcher raise thunk))

