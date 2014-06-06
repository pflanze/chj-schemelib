;;; Copyright 2010-2014 by Christian Jaeger, ch at christianjaeger ch

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (lib.define-macro-star)
	 (lib.test)
	 (lib.cj-env)
	 (lib.srfi-11))


;;;
;;;; simple explicit type checking
;;;


(define (cj-typed#type-check-error predstr w v)
  ;; v = value
  ;; w = result of predicate
  (cond ((eq? w #f)
	 (error (string-append "does not match "
			       predstr
			       ":")
		v))
	;; HACK: hand-coded predicate for |failure?| to avoid circular
	;; dependency between fail.scm and cj-typed.scm
	((and (vector? w)
	      (> (vector-length w) 1)
	      (eq? (vector-ref w 0) 'failure))
	 ;; although the joke is that we need to rely on circular
	 ;; dynamic linking for |failure.string| anyway... (but at
	 ;; least reducing to just this one will allow type-check to
	 ;; work without test.scm loaded).
	 (error (string-append "does not match "
			       predstr
			       " "
			       (failure.string w)
			       ":")
		v))
	(else
	 (error "predicate "
		predstr
		" returned invalid non-boolean value:"
		w))))


(define-macro* (type-check predicate expr . body)
  (let ((V (gensym))
	(W (gensym)))
    `(##let* ((,V ,expr)
	      (,W (,predicate ,V)))
	     (##if (##eq? ,W #t)
		   (##begin ,@body)
		   (cj-typed#type-check-error
		    ,(scm:object->string (cj-desourcify predicate))
		    ,W
		    ,V)))))


(define (transform-arg arg args body)
  ;; -> (values args* body*)
  (let ((arg* (source-code arg)))
    (define (err)
      (source-error arg "expecting symbol or #(predicate var)"))
    (cond ((symbol? arg*)
	   (values (cons arg args)
		   body))
	  ((vector? arg*)
	   (if (= (vector-length arg*) 2)
	       (let ((pred (vector-ref arg* 0))
		     (var (vector-ref arg* 1)))
		 (assert* symbol? var
			  (lambda (_)
			    (values (cons var args)
				    `(type-check ,pred ,var
						 ,body)))))
	       (err)))
	  ((meta-object? arg*)
	   (values (cons arg* args)
		   body))
	  ((pair? arg*)
	   ;; should be after an #!optional; XX verify? or leave that
	   ;; up to the next language layer?
	   (if (= (improper-length arg*) 2)
	       (let ((arg** (car arg*))
		     (default (cadr arg*)))
		 (letv ((subargs body*) (transform-arg arg** args body))
		       (let-pair ((subarg _) subargs)
				 (values (cons `(,subarg ,default) args)
					 body*))))
	       ;; XX could give better error message, though
	       (err)))
	  (else
	   (err)))))

;; for use by other code
(define (perhaps-typed.var x)
  (car (transform-arg x '() '())))

(define (typed? x)
  ;; stupid ~COPY
  (let ((x* (source-code x)))
    (and (vector? x*)
	 (= (vector-length x*) 2)
	 (symbol? (source-code (vector-ref x* 1))))))

(define (typed.var x)
  ;; again stupid ~COPY
  (vector-ref (source-code x) 1))


(define (args-detype args)
  (improper-fold-right* (lambda (tail? arg args*)
			  (let ((a* (fst (transform-arg arg args* #f))))
			    (if tail?
				(car a*)
				a*)))
			'()
			(source-code args)))

(TEST
 > (args-detype '(a b . c))
 (a b . c)
 > (args-detype '(a b #!optional c))
 (a b #!optional c)
 > (args-detype '(#(pair? a) b #!optional #(number? c)))
 (a b #!optional c)
 > (args-detype '(#(pair? a) b #!optional (c 10)))
 (a b #!optional (c 10))
 > (args-detype '(#(pair? a) b #!optional (#(number? c) 10)))
 (a b #!optional (c 10))
 )


(define-macro* (typed-lambda args . body)
  (letv ((vars body)
	 (let rem ((args args))
	   (let ((args_ (source-code args)))
	     (cond ((null? args_)
		    (values '()
			    `(begin ,@body)))
		   ((pair? args_)
		    (let-pair ((arg args*) args_)
			      (letv (($1 $2) (rem args*))
				    (transform-arg arg $1 $2))))
		   (else
		    ;; rest arg, artificially pick out the single var
		    (letv ((vars body)
			   (letv (($1 $2) (rem '()))
				 (transform-arg args $1 $2)))
			  (assert (= (length vars) 1))
			  (values (car vars)
				  body)))))))
	`(lambda ,vars
	   ,body)))

(TEST
 > (expansion#typed-lambda (a b) 'hello 'world)
 (lambda (a b) (begin 'hello 'world))
 > (expansion#typed-lambda foo 'hello 'world)
 (lambda foo (begin 'hello 'world))
 > (expansion#typed-lambda (a #(pair? b)) 'hello 'world)
 (lambda (a b)
   (type-check pair? b
	       (begin 'hello 'world)))
 > (expansion#typed-lambda (a #(pair? b) . c) 'hello 'world)
 (lambda (a b . c)
   (type-check pair? b
	       (begin 'hello 'world)))
 > (expansion#typed-lambda (a #(pair? b) #!rest c) 'hello 'world)
 (lambda (a b #!rest c) (type-check pair? b (begin 'hello 'world)))
 > (expansion#typed-lambda (a #(pair? b) . #(number? c)) 'hello 'world)
 (lambda (a b . c)
   (type-check pair? b (type-check number? c (begin 'hello 'world))))
 ;;^ XX wrong? make it list-of ? (this would be a redo, sigh)
 > (expansion#typed-lambda (a #!key #(pair? b) #!rest #(number? c)) 'hello 'world)
 (lambda (a #!key b #!rest c)
   (type-check pair? b (type-check number? c (begin 'hello 'world))))
 > (expansion#typed-lambda (#(pair? a) b #!optional (#(number?  c) 10)) hello)
 (lambda (a b #!optional (c 10))
   (type-check pair? a
	       (type-check number? c (begin hello)))))


(define-macro* (detyped-lambda args . body)
  `(lambda ,(args-detype args)
     ,@body))

(TEST
 > (expansion#detyped-lambda (a #(pair? b) . c) 'hello 'world)
 (lambda (a b . c)
   'hello 'world))

(define-macro* (define-typed name+args . body)
  (let ((name+args_ (source-code name+args)))
    (let-pair
     ((name args) name+args_)
     `(define ,name (typed-lambda ,args ,@body)))))

;; (TEST
;;  > (require (lib.cj-symbol)
;; 	    (lib.cj-expansion)))
;; (TEST
;;  > (define TEST:equal? syntax-equal?)
;;  > (expansion define-typed (f #(integer? x) #(symbol? a)) (vector x a))
;;  ...
;;  )
;; (XX provide actual tests instead.)


(define-macro* (-> pred expr)
  (with-gensym V
	       `(let ((,V ,expr))
		  (if (,pred ,V) ,V
		      (error "value fails to meet predicate:" ,V)))))

(TEST
 > (-> number? 5)
 5
 > (%try-error (-> number? "5"))
 #(error "value fails to meet predicate:" "5")
 )

