;;; Copyright 2011-2016 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require define-macro-star
	 (fixnum inc)
	 test)

(export vector-equal?
	source-equal?
	(macro template:quote)
	(macro force-source-code))


(include "cj-standarddeclares.scm")


(define (vector-equal? equal? a b)
  (let ((la (vector-length a))
	(lb (vector-length b)))
    (and (= la lb)
	 (let lp ((i 0))
	   (if (< i la)
	       (and (equal? (vector-ref a i)
			    (vector-ref b i))
		    (lp (inc i)))
	       #t)))))

(define (source-equal? a b)
  (let ((a* (source-code a))
	(b* (source-code b)))
    (or (eq? a* b*)
	(cond ((number? a*)
	       (and (number? b*)
		    (= a* b*)))
	      ((string? a*)
	       (and (string? b*)
		    (string=? a* b*)))
	      ;; don't examine symbols more closely than eq?;
	      ((pair? a*)
	       (and (pair? b*)
		    (source-equal? (car a*) (car b*))
		    (source-equal? (cdr a*) (cdr b*))))
	      ((vector? a*)
	       (and (vector? b*)
		    (vector-equal? source-equal? a* b*)))
	      ((null? a*)
	       (null? b*))
	      ;; XXX boxes? and more?
	      ((symbol? a*)
	       #f)
	      (else
	       (error "source-equal?: unknown type of:" (cj-desourcify a*)))))))

(TEST
 > (source-equal? 'a 'a)
 #t
 > (source-equal? 'a 'b)
 #f
 > (source-equal? '#(a b) '#(a b))
 #t
 > (source-equal? '#(a b) '#(a b c))
 #f
 > (source-equal? '#(a b) '#(a c))
 #f
 > (source-equal? '#(a b) '(a b))
 #f
 > (source-equal? '(a . b) '(a . b))
 #t
 > (source-equal? '(a . b) '(a b))
 #f
 > (source-equal? '(a . #(b)) '(a . #(b)))
 #t
 > (source-equal? '(a . #(b)) '(a . #()))
 #f
 > (source-equal? '(a . #("a")) '(a . #("a")))
 #t
 > (source-equal? '(a . #("a")) '(a . #("b")))
 #f
 > (source-equal? '(a . #("a")) '(a . #(#f)))
 #f
 > (source-equal? '(a b) '(a))
 #f
 > (source-equal? '(a) '(a b))
 #f)

(define-macro* (template:quote form)
  `(u8vector->object ',(object->u8vector form)))

(TEST
 > (source-equal? 'a (template:quote a))
 #t
 > (source-equal? 'a (template:quote b))
 #f
 > (source-equal? '#(a b) (template:quote #(a b)))
 #t
 > (source-equal? '#(a b) (template:quote #(a c)))
 #f
 > (source-equal? '(a . b) (template:quote (a . b)))
 #t
 > (source-equal? '(a . b) (template:quote (a b)))
 #f
 > (source-equal? '(a . #(b)) (template:quote (a . #(b))))
 #t
 > (source-equal? '(a . #(b)) (template:quote (a . #())))
 #f
 > (source-equal? '(a . #("a")) (template:quote (a . #("a"))))
 #t
 > (equal? '(a . #("a")) (template:quote (a . #("a"))))
 #f
 )

(define-macro* (force-source-code vars . body)
  `(let ,(map (lambda (v)
		`(,v (source-code ,v)))
	      (source-code vars))
     ,@body))

(TEST
 > (expansion#force-source-code (x y) foo)
 (let ((x (source-code x)) (y (source-code y))) foo)
 )
