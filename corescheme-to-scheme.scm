;; Copyright 2016-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 corescheme
	 (failing <failing-on> <failing-off>)
	 test
	 test-logic)


(def. (corescheme-literal.scheme v)
  (let-corescheme-literal
   ((val) v)
   (cond (((either symbol? keyword? null?)
	   (source-code val))
	  `(quote ,val))
	 (else
	  val))))

(def. (corescheme-lambda.scheme v)
  (let-corescheme-lambda ((vars expr) v)
			 `(lambda ,(map .name vars)
			    ,@(map .scheme
				   (if (corescheme-begin? expr)
				       (.body expr)
				       (list expr))))))

(def. (corescheme-app.scheme v)
  (let-corescheme-app ((proc args) v)
		      `(,(.scheme proc)
			,@(map .scheme args))))

(def. corescheme-ref.scheme (comp .name .var))

(def. (corescheme-def.scheme v)
  (let-corescheme-def ((var val) v)
		      `(define ,(.name var)
			 ,(.scheme val))))

(def. (corescheme-begin.scheme v)
  `(begin ,@(map .scheme (.body v))))

(def. (corescheme-if.scheme v)
  (let-corescheme-if ((test then else) v)
		     `(if ,(.scheme test)
			  ,(.scheme then)
			  ,@(if else
				(list (.scheme else))
				'()))))

(def. (corescheme-set!.scheme v)
  `(set! ,(.name var)
	 ,(.scheme val)))

;; corescheme-letrec


;; change to <failing-on> for debugging
(modimport/prefix failing: <failing-off>)

(TEST
 > (def (corescheme-back source)
	(.scheme
	 (source.corescheme source get-ctx: default-scheme-env)))
 > (def t-scheme-code
	(lambda (source result)
	  (and
	   ;; are the given code fragments even evaluating to the same
	   ;; value?
	   (failing:equal? (eval source)
			   (eval result))
	   ;; and does the compiler actually give the given result?
	   (failing:source-equal? (corescheme-back source)
				  result))))
 > (def t-scheme-code*
	(lambda (v)
	  (let-pair ((a b) (source-code v))
		    (t-scheme-code a b))))
 > (def failures
	(qcheck
	 (source-code
	  (quote-source
	   ( ;; source and expected result from corescheme-back in pairs
	    ((let* ((x 4) (y x))
	       (begin 2 x))
	     .
	     ((lambda (x) ((lambda (y) 2 x) x)) 4))

	    ((let ((x 4) (y 5)) (begin 2 x))
	     .
	     ((lambda (x y) 2 x) 4 5))

	    ((let ((x 4) (y 5)) 2 x)
	     .
	     ((lambda (x y) 2 x) 4 5))

	    ((let ((x '()))
	       x)
	     .
	     ((lambda (x) x) '()))

	    ((define (corescheme-to-scheme:square n)
	       (* n n))
	     .
	     (define corescheme-to-scheme:square (lambda (n) (* n n))))

	    ((define (corescheme-to-scheme:fact n)
	       (if (zero? n)
		   1
		   (* n (corescheme-to-scheme:fact (- n 1)))))
	     .
	     (define corescheme-to-scheme:fact
	       (lambda (n)
		 (if (zero? n)
		     1
		     (* n (corescheme-to-scheme:fact (- n 1)))))))
	    
	    ((begin (define a 1) (define b a))
	     .
	     (begin (define a 1) (define b a)))

	    ((begin
	       (define (corescheme-to-scheme:odd? n)
	    	 (if (zero? n)
	    	     #f
	    	     (corescheme-to-scheme:even? (- n 1))))
	       (define (corescheme-to-scheme:even? n)
	    	 (if (zero? n)
	    	     #t
	    	     (corescheme-to-scheme:odd? (- n 1)))))
	     .
	     (begin
	       (define corescheme-to-scheme:odd?
		 (lambda (n)
		   (if (zero? n)
		       #f
		       (corescheme-to-scheme:even? (- n 1)))))
	       (define corescheme-to-scheme:even?
		 (lambda (n)
		   (if (zero? n)
		       #t
		       (corescheme-to-scheme:odd? (- n 1)))))))

            ((define (corescheme-to-scheme:foo fn)
               ((lambda (x) (fn x)) 10))
             .
             (define corescheme-to-scheme:foo
               (lambda (fn) ((lambda (x) (fn x)) 10))))
	    )))
	 t-scheme-code*))
 > failures
 ())

