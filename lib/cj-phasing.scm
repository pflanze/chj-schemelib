;; This used to be compile-time, which could be used both for the
;; result as well as the side effects. now be explicit about the
;; purpose:

(define-macro* (insert-result-of . body)
  (eval `(begin
	   ,@body)))


;; compile-time and both-times are the same now as long as runtime
;; macros are being built into the binary: if we want the macros that
;; need the compile-time stuff to be available at runtime (and I use
;; define-macro* for everything now), then everything has to be
;; both-times.

(define-macro* (compile-time . body)
  (let ((code
	 `(begin
	    ,@body)))
    (eval code)
    code))

(define-macro* (both-times . body)
  (let ((code
	 `(begin
	    ,@body)))
    (eval code)
    code))
