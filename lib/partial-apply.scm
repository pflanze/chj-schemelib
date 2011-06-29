;;; Copyright 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.


;; requires cj-source-util


(define-macro* (define/pa nam+args . body)
  (let-pair ((nam args) (source-code nam+args))
	    (assert* symbol? nam
		     (lambda (nam*)
		       `(begin
			  (compile-time
			   (define ,(symbol-append "arity#" nam*)
			     ',(schemedefinition-arity:pattern->template args)))
			  (define ,nam+args
			    ,@body))))))

(define-macro* (pa fnname . args)
  (assert*
   symbol? fnname
   (lambda (fnname*)
     (let ((nargs (length args))
	   (arity (eval (symbol-append "arity#" fnname*))))
       (let ((need-vars-exact
	      (lambda (n-vars)
		(let ((n (- n-vars nargs)))
		  (if (negative? n)
		      (source-error
		       fnname
		       "function defined with fewer arguments than passed")
		      (let* ((vs (map gensym (make-list n
							'v))))
			`(lambda ,vs
			   (,fnname ,@args ,@vs)))))))
	     (need-vars/rest
	      ;; not handling up-to accurately, ok?
	      (lambda (n-vars)
		(let* ((vs (map gensym (make-list (max (- n-vars nargs) 0)
						  'v)))
		       (vrest (gensym)))
		  `(lambda ,(append vs vrest)
		     (apply ,fnname ,@args ,@vs ,vrest))))))
	 ((case (vector-ref arity 0)
	    ((exact)
	     need-vars-exact)
	    ((up-to at-least)
	     need-vars/rest)
	    (else (error "bug")))
	  (vector-ref arity 1)))))))

(TEST
 > (define/pa (f a . b) (cons a b))
 > arity#f
 #(at-least 1)
 > ((pa f 3) 4)
 (3 4)
 > ((pa f 3) 4 5)
 (3 4 5)
 > ((pa f 3 4) 5)
 (3 4 5)
 > (define/pa (f a b) (cons a b))
 ;; > ((pa f 3 4) 5)
 ;; *** ERROR IN (console)@11.1 -- Wrong number of arguments passed to procedure
 > ((pa f 3 4))
 (3 . 4)
 ;; > ((pa f 3 4 5))
 ;; *** ERROR IN syntax, (console)@14.6 -- function defined with fewer arguments than passed
 )

