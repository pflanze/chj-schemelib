
;; can this calculation be optimized?
(define (quotient+modulo x y)
  (values (quotient x y)
	  (modulo x y)))
