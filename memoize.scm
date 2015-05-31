

(define (memoize f)
  (let ((t (make-table))
	(nothing (cons 1 2)))
    (lambda vs
      (let ((v? (table-ref t vs nothing)))
	(if (eq? v? nothing)
	    (let ((v (apply f vs)))
	      ;; XX: make multi-threading safe
	      (table-set! t vs v)
	      v)
	    v?)))))

