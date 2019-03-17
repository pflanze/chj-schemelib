;;; Copyright 2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 (cj-symbol-with with-gensym))

(export throwing-function
	throwing)

"Turn \"maybe-\" functions to exception-throwing ones."

;; XX: make more like Rust


(define (throwing-function fn)
  (lambda args
    (or (apply fn args)
	(error "function returned false"))))


(define (throwing:error fn-expr v)
  (error "throwing: got #f from:" `(,fn-expr ',v)))


;; just 1-ary? sigh. How again? Forever!
(define-macro* (throwing fn-expr)
  (with-gensym V
	       `(lambda (,V)
		  (or (,fn-expr ,V)
		      (throwing:error ',fn-expr ,V)))))


(TEST
 > (define (maybe-car p)
     (and (pair? p)
	  (car p)))
 > (define mycar (throwing maybe-car))
 > (mycar '(a b))
 a
 > (%try (mycar '()))
 (exception text: "throwing: got #f from: (maybe-car '())\n"))

