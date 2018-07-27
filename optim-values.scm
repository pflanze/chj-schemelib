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
	 ;; (cj-source-util schemedefinition-arity:pattern->template)
	 cj-symbol-with
	 ;;(srfi-1 iota) avoid
	 ;;test  avoid, see optim-values-test.scm
	 )

(export (macro @values-ref)
	(macro @values-length)
	(macro %call-with-values)
	(macro receive))

(include "cj-standarddeclares.scm")

;; simplified copy from srfi-1 to avoid dependency
(define (iota count)
  (define step 1)
  (define start 0) ;; huh wow why does it not start with n == start ?
  (let loop ((n 0) (r '()))
    (if (= n count)
	(reverse r)
	(loop (+ 1 n)
	      (cons (+ start (* n step)) r)))))


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

;; tests see optim-values-test.scm


(define (optim-values:maybe-lambda-exact-arity v)
  (cond ((optim-values:maybe-lambda-binds v)
	 => (lambda (binds)
	      ;; wow need to strip source info here
	      (let ((t (schemedefinition-arity:pattern->template
			(source-code binds))))
		(and (eq? (vector-ref t 0) 'exact)
		     (vector-ref t 1)))))
	(else #f)))

;; tests see optim-values-test.scm


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
		  ;; To be fair, there *could* be values tuples of
		  ;; length 1 (not when using |values| though);
		  `(,consumer (,producer))
		  ;; but definitely by way of making sure we never get
		  ;; arity 1 for this code, we ensure we do have a
		  ;; tuple at hands here:
		  (with-gensym
		   V
		   `(##let ((,V (,producer)))
			   (##if (##let () (##declare (block)
						      (standard-bindings)
						      (extended-bindings)
						      (not safe) (fixnum))
					(##namespace ("" and =))
					(and (##values? ,V)
					     (= (@values-length ,V) ,arity)))
				 (,consumer ,@(map (lambda (i)
						     `(@values-ref ,V ,i))
						   (iota arity)))
				 (optim-values:error ,V ,arity)))))))
	(else
	 (fallback))))

;; tests see optim-values-test.scm


;; Note: not using ##lambda so as to allow for unhygienically binding
;; |lambda| from easy-1.scm for typed etc. (fun how this is enabling, in
;; such an uncomplicated way, well. Hack or clean? They call it
;; unhygienic.)
(define-macro* (receive vars generator . body)
  `(%call-with-values (lambda ()
			,generator)
		      (lambda ,vars
			,@body)))

