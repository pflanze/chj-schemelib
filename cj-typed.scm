;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

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


(define-macro* (type-check predicate expr . body)
  (let ((V (gensym)))
    `(let ((,V ,expr))
       (if (,predicate ,V)
	   (begin
	     ,@body)
	   (error ,(string-append "does not match "
				  (scm:object->string
				   (cj-desourcify predicate))
				  ":")
		  ,V)))))


(define handle-arg
  (lambda (arg $1 $2)
    (let ((arg_ (source-code arg)))
      (define (err)
	(source-error arg "expecting symbol or #(predicate var)"))
      (cond ((symbol? arg_)
	     (values (cons arg $1)
		     $2))
	    ((vector? arg_)
	     (if (= (vector-length arg_) 2)
		 (let ((pred (vector-ref arg_ 0))
		       (var (vector-ref arg_ 1)))
		   (assert* symbol? var
			    (lambda (_)
			      (values (cons var $1)
				      `(type-check ,pred ,var
						   ,$2)))))
		 (err)))
	    ((meta-object? arg_)
	     (values (cons arg_ $1) $2))
	    (else
	     (err))))))

;; for use by other code
(define (perhaps-typed.var x)
  (car (handle-arg x '() '())))

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
			  ;; uh ugly code; change handle-arg some time!
			  (let ((a* (fst (handle-arg arg args* #f))))
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
 (a b #!optional c))


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
				    (handle-arg arg $1 $2))))
		   (else
		    ;; rest arg, artificially pick out the single var
		    (letv ((vars body)
			   (letv (($1 $2) (rem '()))
				 (handle-arg args $1 $2)))
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
 )

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

(TEST
 > (require (lib.cj-symbol)
	    (lib.cj-expansion)))
(TEST
 > (define TEST:equal? syntax-equal?)
 > (expansion define-typed (f #(integer? x) #(symbol? a)) (vector x a))
 (letrec ((f (lambda (x a)
	       (let ((GEN:233 x))
		 (if (integer? GEN:233)
		     (let ((GEN:234 a))
		       (if (symbol? GEN:234)
			   (vector x a)
			   (error "does not match symbol?:" GEN:234)))
		     (error "does not match integer?:" GEN:233))))))
   cont)
 ;; well, or so
 )


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

