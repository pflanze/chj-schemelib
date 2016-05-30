;;; Copyright 2010-2014 by Christian Jaeger, ch at christianjaeger ch

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


;;;
;;;; simple explicit type checking
;;;

(require define-macro-star
	 (scheme-meta perhaps-quote)
	 test
	 srfi-11
	 (simple-match-1 assert*)
	 (cj-source-util-2 assert
			   assert:possibly-symbolize)
	 (improper-list improper-length)
	 cj-typed-1)


(define-macro* (type-check predicate expr . body)
  (let ((V (gensym))
	(W (gensym)))
    `(##let* ((,V ,expr)
	      (,W (,predicate ,V)))
	     (##if (##eq? ,W #t)
		   (##let () ,@body)
		   (cj-typed#type-check-error
		    ,(let ((expr* (cj-desourcify expr)))
		       ;; avoid putting gensyms into exception messages,
		       ;; to make code using this testable.
		       (if (cj-gensym? expr*)
			   (cond ((cj-gensym-maybe-name expr*)
				  => (lambda (name)
				       (string-append "gensym '"
						      (scm:object->string name))))
				 (else
				  #f))
			   (scm:object->string expr*)))
		    ,(scm:object->string (cj-desourcify predicate))
		    ,W
		    ,V)))))

(TEST
 ;; test that there's no "Ill-placed 'define'" compile-time error
 > (let ((foo "foo"))
     (type-check string? foo
		 (##begin (define bar "bar") (string-append foo bar))))
 "foobar"
 > (let ((foo "foo"))
     (type-check string? foo
		 (define bar "bar")
		 (string-append foo bar)))
 "foobar")



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
				 (values (cons (possibly-sourcify
						`(,subarg ,default)
						arg)
					       args)
					 body*))))
	       ;; XX could give better error message, though
	       (err)))
	  (else
	   (err)))))

(TEST
 > (define s1 '#(#(source1)
		  (#(#(source1) pair? (console) 1048595)
		    #(#(source1) a (console) 1441811))
		  (console)
		  983059))
 > (values->vector (transform-arg s1 '() 'BODY))
 #((#(#(source2) (#(#(source1) pair? (console) 1048595)
		   #(#(source1) a (console) 1441811))
       (console)
       983059))
   BODY))


;; for use by other code

;; Note: "var" (i.e. the result type) *should* be a symbol; we don't
;; check here, but it would be an error. (Default values would be
;; given outside, not here: `(#(number? y) 10) not `#(number? (y 10)))

(define (perhaps-typed.var x)
  (car (fst (transform-arg x '() '()))))

(define (typed? x)
  ;; stupid ~COPY
  (let ((x* (source-code x)))
    (and (vector? x*)
	 (= (vector-length x*) 2)
	 (symbol? (source-code (vector-ref x* 1))))))

(define (typed.var x) ;; careful, unsafe!
  ;; again stupid ~COPY
  (vector-ref (source-code x) 1))


(TEST
 > (perhaps-typed.var '#(foo? x))
 x
 > (perhaps-typed.var 'y)
 y
 > (typed? 'y)
 #f
 > (typed? '#(foo? x))
 #t
 > (typed.var '#(foo? x))
 x)



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


(define (typed-body-parse maybe-stx body cont/maybe-pred+body)
  (assert (not (source? body)))
  (let ((body+ (if maybe-stx
		   (sourcify body maybe-stx)
		   body)))
    (if (pair? body)
	(let ((fst* (source-code (car body))))
	  (if (eq? fst* '->)
	      (if ((improper-list/length>= 3) body)
		  (cont/maybe-pred+body (cadr body) (cddr body))
		  (source-error
		   body+
		   "a body starting with -> needs at least 2 more forms"))
	      (cont/maybe-pred+body #f body)))
	(source-error body+
		      "expecting body forms"))))

(TEST
 > (typed-body-parse #f '(a) vector)
 #(#f (a))
 > (typed-body-parse #f '(a b c) vector)
 #(#f (a b c))
 > (typed-body-parse #f '(-> b c) vector)
 #(b (c)))

(define (typed-lambda-args-expand args body)
  (let rem ((args args))
    (let ((args_ (source-code args)))
      (cond ((null? args_)
	     (values '()
		     `(##begin ,@body)))
	    ((pair? args_)
	     (let-pair ((arg args*) args_)
		       (letv (($1 $2) (rem args*))
			     (transform-arg arg $1 $2))))
	    (else
	     ;; rest arg, artificially pick out the single var
	     (letv ((vars body) (letv (($1 $2) (rem '()))
				      (transform-arg args $1 $2)))
		   (assert (= (length vars) 1))
		   (values (car vars)
			   body)))))))

(TEST
 > (define s
     ;; (quote-source ((pair? a)))
     '#(#(source1)
	 (#(#(source1)
	     (#(#(source1) pair? (console) 1048595)
	       #(#(source1) a (console) 1441811))
	     (console)
	     983059))
	 (console)
	 917523))
 > (values->vector (typed-lambda-args-expand s 'BODY))
 #((#(#(source2) (#(#(source1) pair? (console) 1048595)
		   #(#(source1) a (console) 1441811))
       (console)
       983059))
   (##begin . BODY)))


(define-macro* (typed-lambda args . body)
  (typed-body-parse
   stx body
   (lambda (maybe-pred body)
     (let ((body (if maybe-pred
		     `((-> ,maybe-pred ,@body))
		     body)))
       (letv ((vars body) (typed-lambda-args-expand args body))
	     `(##lambda ,vars
		,body))))))

(TEST
 > (expansion#typed-lambda (a b) 'hello 'world)
 (##lambda (a b) (##begin 'hello 'world))
 > (expansion#typed-lambda foo 'hello 'world)
 (##lambda foo (##begin 'hello 'world))
 > (expansion#typed-lambda (a #(pair? b)) 'hello 'world)
 (##lambda (a b)
   (type-check pair? b
	       (##begin 'hello 'world)))
 > (expansion#typed-lambda (a #(pair? b) . c) 'hello 'world)
 (##lambda (a b . c)
   (type-check pair? b
	       (##begin 'hello 'world)))
 > (expansion#typed-lambda (a #(pair? b) #!rest c) 'hello 'world)
 (##lambda (a b #!rest c) (type-check pair? b (##begin 'hello 'world)))
 > (expansion#typed-lambda (a #(pair? b) . #(number? c)) 'hello 'world)
 (##lambda (a b . c)
   (type-check pair? b (type-check number? c (##begin 'hello 'world))))
 ;;^ XX wrong? make it list-of ? (this would be a redo, sigh)
 > (expansion#typed-lambda (a #!key #(pair? b) #!rest #(number? c)) 'hello 'world)
 (##lambda (a #!key b #!rest c)
   (type-check pair? b (type-check number? c (##begin 'hello 'world))))
 > (expansion#typed-lambda (#(pair? a) b #!optional (#(number?  c) 10)) hello)
 (##lambda (a b #!optional (c 10))
   (type-check pair? a
	       (type-check number? c (##begin hello)))))

;; and -> result checks:
(TEST
 > (expansion#typed-lambda (#(pair? a) b #!optional (#(number?  c) 10))
			   -> foo?
			   hello)
 (##lambda (a b #!optional (c 10))
   (type-check pair? a (type-check number? c (##begin (-> foo? hello))))))


(define-macro* (detyped-lambda args . body)
  `(lambda ,(args-detype args)
     ,@(typed-body-parse stx body
			 (lambda (pred body)
			   body))))

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
;;  > (require (cj-symbol)
;; 	    (cj-expansion)))
;; (TEST
;;  > (define TEST:equal? syntax-equal?)
;;  > (expansion define-typed (f #(integer? x) #(symbol? a)) (vector x a))
;;  ...
;;  )
;; (XX provide actual tests instead.)


(define (->-error pred-code val)
  (error "value fails to meet predicate:"
	 (list pred-code (perhaps-quote val))))

(define-macro* (-> pred . body)
  (with-gensym V
	       `(##let ((,V (##let () ,@body)))
		       (##if (,pred ,V) ,V
			     (->-error ',pred ,V)))))

(TEST
 > (-> number? 5)
 5
 > (-> number? "bla" 5)
 5
 > (%try-error (-> number? "5"))
 #(error "value fails to meet predicate:" (number? "5"))
 )

;; test source location propagation
(TEST
 > (def e (with-exception-catcher
	   identity
	   (&
	    (eval
	     (quote-source
	      ;; missing actual body, triggering the message that we
	      ;; want to test
	      (typed-lambda (x) -> echz))))))
 > (source-error-message e)
 "a body starting with -> needs at least 2 more forms"
 > (source? (source-error-source e))
 #t)
