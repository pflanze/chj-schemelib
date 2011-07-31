;;; Copyright 2010, 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


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
  (lambda-values
   (arg ($1 $2))
   (let ((arg_ (source-code arg)))
     (define (err)
       (source-error arg
		     "expecting symbol or #(predicate var)"))
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
	   (else
	    (err))))))

(define-macro* (typed-lambda args . body)
  (letv ((vars body)
	 (let rem ((args args))
	   (let ((args_ (source-code args)))
	     (cond ((null? args_)
		    (values '()
			    `(begin ,@body)))
		   ((pair? args_)
		    (let-pair ((arg args*) args_)
			      (handle-arg arg (rem args*))))
		   (else
		    ;; rest arg, artificially pick out the single var
		    (letv ((vars body)
			   (handle-arg args (rem '())))
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
 )


(define-macro* (define-typed name+args . body)
  (let ((name+args_ (source-code name+args)))
    (let-pair
     ((name args) name+args_)
     `(define ,name (typed-lambda ,args ,@body)))))

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

