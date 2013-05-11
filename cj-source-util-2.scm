;;; Copyright 2013 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require cj-source
	 cj-match
	 )

(define (maybe-if-form-1ary-head v then)
  (and (pair? v)
       (pair? (cdr v))
       (null? (cddr v))
       (then (car v))))

(define (form-_? sym)
  (lambda (v)
    (maybe-if-form-1ary-head v (lambda (v)
				 (eq? (source-code v) sym)))))

(define form-quote? (form-_? 'quote))
(define form-quasiquote? (form-_? 'quasiquote))
(define form-unquote? (form-_? 'unquote))
(define form-unquote-splicing? (form-_? 'unquote-splicing))

(TEST
 > (form-unquote? '())
 #f
 > (form-unquote? '(unquote a))
 #t
 > (form-unquote? '(unquote))
 #f
 > (form-unquote? '(unquote a . b))
 #f
 > (form-quote? ''a)
 #t
 > (form-quasiquote? '`a)
 #t
 > (form-unquote-splicing? ',@a)
 #t
 )

(define assert:possibly-symbolize-procedures
  ;; XX basically just want to know if it's a toplevel procedure. sgh.
  '(
    ;; R5RS hmm
    = > < >= <= cons car cdr vector vector-ref list list-ref
    length vector-length f64vector-length u8vector-length f32vector-length
    pair? null? zero? negative?
    ;; own -- ah btw must exist by the time assert needs to symbolize
    ;; procedures for the first time!
    != inc dec quotient + - * / arithmetic-shift square sqrt log expt
    ))

(define (assert:possibly-symbolize v)
  (let ((v* (source-code v)))
    (if (procedure? v*)
	(let lp ((ss assert:possibly-symbolize-procedures))
	  (if (null? ss)
	      v
	      (let ((sym (car ss)))
		(if (eq? v* (eval sym))
		    sym
		    (lp (cdr ss))))))
	v)))

(compile-time
 (define (assert-replace-expand e)
   (mcase e
	  (pair?
	   (let ((e* (source-code e)))
	     `(##cons ,(assert-replace-expand (car e*))
		      ,(assert-replace-expand (cdr e*)))))
	  (symbol?
	   `(assert:possibly-symbolize ,e))
	  (else
	   `(quote ,e)))))

(TEST
 > (assert-replace-expand '(= e1 e2))
 (##cons = (##cons e1 (##cons e2 '())))
 )

(define-macro* (assert expr)
  `(if (not ,expr)
       (error ,(string-append "assertment failure: "
			      (scm:object->string (cj-desourcify expr)))
	      ,(assert-replace-expand expr))))

