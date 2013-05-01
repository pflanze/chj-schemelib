;; also requires |define.|
;; Hm actually perhaps it doesn't.
;; Well or maybe it does. |for| as macro, then .for for the underlying?

(define-macro* (for var seq . body)
  `(.for seq
	 (lambda (,var return)
	   ,@body)))


(define (string-contains-char? str pred)
  (let ((len))))