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
    eq? eqv? equal?
    ;; own
    /= inc dec quotient + - * / arithmetic-shift square sqrt log expt
    eql?
    .type .info
    ))

(define assert:stopping-syntax-forms
  ;; forms that stop from recursing inside
  '(quote quasiquote))

(define assert:syntax-forms
  (append '(and or)
	  assert:stopping-syntax-forms))

(define (assert:possibly-symbolize v)
  (let ((v* (source-code v)))
    (if (procedure? v*)
	(let lp ((ss assert:possibly-symbolize-procedures))
	  (if (null? ss)
	      v
	      (let ((sym (car ss))
		    (_else (lambda () (lp (cdr ss)))))
		(cond ((with-exception-catcher
			(lambda (e)
			  (if (unbound-global-exception? e)
			      #f
			      (raise e)))
			(lambda ()
			  (eval sym)))
		       => (lambda (symv)
			    (if (eq? v* symv)
				sym
				(_else))))
		      (else (_else))))))
	v)))

(compile-time
 (define (assert-replace-expand e)
   (mcase e
	  (pair?
	   (let* ((e* (source-code e)))
	     (if (memq (source-code (car e*)) assert:stopping-syntax-forms)
		 `(quote ,e)
		 `(##cons ,(assert-replace-expand (car e*))
			  ,(assert-replace-expand (cdr e*))))))
	  (symbol?
	   (if (or (define-macro-star-maybe-ref (source-code e))
		   (memq (source-code e) assert:syntax-forms))
	       ;; even though it might be shadowed by a local
	       ;; definition, since we don't have (thanks expander) a
	       ;; way to check for that, we have to be conservative to
	       ;; avoid referencing errors at runtime.
	       `',e
	       `(assert:possibly-symbolize ,e)))
	  (else
	   `(quote ,e)))))

(TEST
 > (assert-replace-expand '(= e1 e2))
 (##cons (assert:possibly-symbolize =)
	 (##cons (assert:possibly-symbolize e1)
		 (##cons (assert:possibly-symbolize e2) '())))
 > (assert-replace-expand '((on foo bar) e1 e2))
 (##cons (##cons 'on
		 (##cons (assert:possibly-symbolize foo)
			 (##cons (assert:possibly-symbolize bar) '())))
	 (##cons (assert:possibly-symbolize e1)
		 (##cons (assert:possibly-symbolize e2) '())))
 > (assert-replace-expand ''e1)
 ''e1
 > (assert-replace-expand '(eq? 'a 'b))
 (##cons (assert:possibly-symbolize eq?) (##cons ''a (##cons ''b '())))
 > (eval #)
 (eq? 'a 'b)
 )

(define-macro* (assert expr)
  `(if (not ,expr)
       (error ,(string-append "assertment failure: "
			      (scm:object->string (cj-desourcify expr)))
	      ,(assert-replace-expand expr))))

