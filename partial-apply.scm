;;; Copyright 2011 by Christian Jaeger <chrjae@gmail.com>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require (define-macro-star)
	 (lib.test)
	 (lib.cj-source-util))


(compile-time
 (define (partial-apply:expand nam args body)
   (assert* symbol? nam
	    (lambda (nam*)
	      `(begin
		 (compile-time
		  (define ,(symbol-append "arity#" nam*)
		    ',(schemedefinition-arity:pattern->template args)))
		 (define ,(cons nam args)
		   ,@body))))))

(define-macro* (define/pa nam+args . body)
  (let-pair ((nam args) (source-code nam+args))
	    (partial-apply:expand nam args body)))

(compile-time
 (define (partial-apply:pa suppress-thunk? fnname args)
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
		       (if (and suppress-thunk? (zero? n))
			   `(,fnname ,@args)
			   (let* ((vs (map gensym (make-list n
							     'v))))
			     `(lambda ,vs
				(,fnname ,@args ,@vs))))))))
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
	   (vector-ref arity 1))))))))

(define-macro* (pa fnname . args)
  (partial-apply:pa #f fnname args))

(define-macro* (pa* fnname . args)
  (partial-apply:pa #t fnname args))

;; with macro for implicit |pa|
(define-macro* (define/pa* nam+args . body)
  (let-pair ((nam args) (source-code nam+args))
	    (assert* symbol? nam
		     (lambda (nam*)
		       (let ((name-fn (symbol-append nam* ".")))
			 (with-gensym
			  ARGS
			  `(begin
			     ,(partial-apply:expand name-fn args body)
			     (define-macro* (,nam . ,ARGS)
			       `(pa* ,',name-fn ,@,ARGS)))))))))

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
 > (define/pa* (c-list3 a b c) (list a b c))
 > (c-list3 2 3 4)
 (2 3 4)
 > ((c-list3 2 3) 5)
 (2 3 5)
 > ((c-list3) 2 3 6)
 (2 3 6)
 )

