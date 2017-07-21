
(require dummy-module ;; XX stupid, just add it here so that I don't
		      ;; have to add it everywhere, hack, finally
		      ;; implement the d* module system.
	 define-macro-star)

;; This used to be compile-time, which could be used both for the
;; result as well as the side effects. now be explicit about the
;; purpose:

(define-macro* (insert-result-of . body)
  (eval `(begin
	   ,@body)))


;; Note that macros that are available at runtime will need
;; the use of both-times, not compile-time!

(define-macro* (compile-time . body)
  (let ((code
	 `(begin
	    ,@body)))
    (eval code)
    `(void)))

(define-macro* (both-times . body)
  (let ((code
	 `(begin
	    ,@body)))
    (eval code)
    code))
