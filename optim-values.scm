;;; Copyright 2018 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;; Starting not to put all of my values related stuff into srfi-11.scm
;; (it is pointless for dependency reasons since srfi-11.scm depends
;; on this, but avoids sending people to the wrong place for
;; documentation)

(require define-macro-star
	 (cj-source-util schemedefinition-arity:pattern->template)
	 cj-symbol
	 (srfi-1 iota)
	 test)

(export (macro @values-ref)
	(macro @values-length)
	(macro %call-with-values))


(define-macro* (@values-ref v i)
  ;; need unsafe mode for ##vector-ref to be compiled efficiently!
  `(##let ()
	  (declare (not safe))
	  (##vector-ref ,v ,i)))

(define-macro* (@values-length v)
  ;; need unsafe mode here, too?
  `(##let ()
	  (declare (not safe))
	  (##vector-length ,v)))


;; XX finally have one place for scheme syntax analysis please...
(define (optim-values:maybe-lambda-binds v)
  (let ((v (source-code v)))
    (and (pair? v)
	 (let ((h (source-code (car v))))
	   (and (or (eq? h 'lambda)
		    (eq? h '##lambda))
		(let ((r (cdr v)))
		  (and (pair? r)
		       (car r))))))))

(define (optim-values:lambda? v)
  (and (optim-values:maybe-lambda-binds v) #t))

(TEST
 > (optim-values:lambda? '(lambda ()))
 #t
 > (cj-desourcify (optim-values:maybe-lambda-binds '(lambda (x))))
 (x)
 > (optim-values:lambda? '(##lambda (x)))
 #t
 ;; well:
 > (optim-values:lambda? '(lambda-values (x)))
 #f)


(define (optim-values:maybe-lambda-exact-arity v)
  (cond ((optim-values:maybe-lambda-binds v)
	 => (lambda (binds)
	      ;; wow need to strip source info here
	      (let ((t (schemedefinition-arity:pattern->template
			(source-code binds))))
		(and (eq? (vector-ref t 0) 'exact)
		     (vector-ref t 1)))))
	(else #f)))

(TEST
 > (optim-values:maybe-lambda-exact-arity '(lambda ()))
 0
 > (optim-values:maybe-lambda-exact-arity '(lambda (x a b)))
 3
 > (optim-values:maybe-lambda-exact-arity '(lambda (x a . b)))
 #f
 > (optim-values:maybe-lambda-exact-arity '(lambda (x a #!optional b)))
 #f
 ;; well:
 > (optim-values:maybe-lambda-exact-arity '(lambda-pair (a)))
 #f)


(define (optim-values:error val expected-arity)
  (define (show vs)
    (apply error
	   (string-append "expect "
			  (number->string expected-arity)
			  " value(s) but got "
			  (number->string (length vs))
			  ":")
	   vs))
  (show (if (##values? val)
	    (##vector->list val)
	    (list val))))

(define-macro* (%call-with-values producer consumer)
  (define (fallback)
    `(call-with-values ,producer ,consumer))
  (cond ((optim-values:maybe-lambda-exact-arity producer)
	 => (lambda (producer-arity)
	      (if (zero? producer-arity)
		  'ok
		  (source-error producer "producer must have arity 0")))))
  ;; XX only when compiled? Since otherwise will be slower (lambdas not
  ;; optimized away etc.)
  (cond ((optim-values:maybe-lambda-exact-arity consumer)
	 => (lambda (arity)
	      (if (= arity 1)
		  `(,consumer (,producer))
		  (with-gensym
		   V
		   `(##let ((,V (,producer)))
			   (##if (##let () (##declare (block)
						      (standard-bindings)
						      (extended-bindings)
						      (not safe) (fixnum))
					(##namespace ("" and values? =))
					(and (values? ,V)
					     (= (@values-length ,V) ,arity)))
				 (,consumer ,@(map (lambda (i)
						     `(@values-ref ,V ,i))
						   (iota arity)))
				 (optim-values:error ,V ,arity)))))))
	(else
	 (fallback))))


(TEST
 > (define TEST:equal? syntax-equal?)
 > (expansion#%call-with-values (lambda () (values 3 4 5)) (lambda (a b c) b))
 (##let ((GEN:V-3073 ((lambda () (values 3 4 5)))))
	(##if (##let ()
		     (##declare
                      (block)
                      (standard-bindings)
                      (extended-bindings)
                      (not safe)
                      (fixnum))
		     (##namespace ("" and values? =))
		     (and (values? GEN:V-3073)
			  (= (@values-length GEN:V-3073) 3)))
	      ((lambda (a b c) b)
	       (@values-ref GEN:V-3073 0)
	       (@values-ref GEN:V-3073 1)
	       (@values-ref GEN:V-3073 2))
	      (optim-values:error GEN:V-3073 3)))
 > (%call-with-values (lambda () (values 3 4 5)) (lambda (a b c) b))
 4
 > (expansion#%call-with-values (lambda () (values 3 4 5)) (lambda (a b . c) b))
 (call-with-values (lambda () (values 3 4 5)) (lambda (a b . c) b))
 > (%call-with-values (lambda () (values 3 4 5)) (lambda (a b . c) b))
 4)

