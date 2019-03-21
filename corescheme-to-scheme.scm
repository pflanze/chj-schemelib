;; Copyright 2016-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 corescheme
	 (failing <failing-on> <failing-off>)
         (scheme-meta self-quoting?)
	 test
	 test-logic)

(export (method .scheme))

(TEST
 > (def roundtrip
        (=>* (source.corescheme globals: '(.>>= return * + - f))
             .scheme))

 > (defmacro (ROUNDTRIP* expr)
     `(roundtrip (quote-source ,expr)))

 > (defmacro (ROUNDTRIP expr)
     `(cj-desourcify (roundtrip (quote-source ,expr)))))



;; XXX careful. After optimizations, there might be different
;; variables with the same name in the same scope, and hence actually
;; *do* need to be renamed for code gen. Not done yet. (Could be a
;; processing step, though? Although bad to still use the
;; corescheme-var type to represent the result.)


(def.* (corescheme-literal.scheme v)
  (let ((val* (source-code val)))
    (cond ((self-quoting? val*)
           val)
          (else
           `(quote ,val)))))

(TEST
 > (.scheme (RUN-CORESCHEME (corescheme-literal foo:)))
 foo:
 > (.scheme (RUN-CORESCHEME (corescheme-literal `foo)))
 'foo
 > (.scheme (RUN-CORESCHEME (corescheme-literal `())))
 '()
 ;; quoted lists
 > (source.corescheme ` (car))
 [(corescheme-app) #f [(corescheme-ref) #f [(corescheme-var) car 6]] ()]
 > (source.corescheme ` '(car))
 [(corescheme-literal) #f (car)]
 > (.scheme #)
 '(car)
 > (source.corescheme ` '())
 [(corescheme-literal) #f ()]
 > (source.corescheme ` ''())
 [(corescheme-literal) #f '()]

 > (.scheme (RUN-CORESCHEME (corescheme-literal `(1))))
 '(1)
 > (.scheme (RUN-CORESCHEME (corescheme-literal `'())))
 ''()
 > (.scheme (RUN-CORESCHEME (corescheme-literal `'foo)))
 ''foo

 > (ROUNDTRIP f)
 f
 > (ROUNDTRIP 'f)
 'f
 > (ROUNDTRIP ''f)
 ''f
 > (ROUNDTRIP '''f)
 '''f
 > (ROUNDTRIP foo:)
 foo:
 > (ROUNDTRIP 'foo:)
 foo: ;; loses the unnecessary quote, which is per spec
 > (with-exception-catcher source-error-message (& (ROUNDTRIP ())))
 "unquoted empty list treated as invalid function application"
 > (ROUNDTRIP '())
 '()
 > (ROUNDTRIP ''())
 ''()
 > (ROUNDTRIP 10)
 10
 > (ROUNDTRIP '10)
 10
 )


(def.* (corescheme-lambda.scheme v)
  `(lambda ,(map .name vars)
     ,@(map .scheme
            (if (corescheme-begin? expr)
                (.body expr)
                (list expr)))))

(def.* (corescheme-app.scheme v)
  `(,(.scheme proc)
    ,@(map .scheme args)))

(def. corescheme-ref.scheme (comp .name .var))

(def.* (corescheme-def.scheme v)
  `(define ,(.name var)
     ,(.scheme val)))


(def (corescheme-<seq>.scheme <seq>)
     (lambda (v)
       (let. ((body) v)
             `(,<seq> ,@(map .scheme body)))))

(def. corescheme-begin.scheme (corescheme-<seq>.scheme `begin))

(def.* (corescheme-if.scheme v)
  `(if ,(.scheme test)
       ,(.scheme then)
       ,@(if else
             (list (.scheme else))
             '())))

(def. (corescheme-set!.scheme v)
  `(set! ,(.name var)
	 ,(.scheme val)))

(def (corescheme-<let>.scheme <let>)
     (lambda (v)
       (let. ((vars exprs body-expr) v)
             `(,<let> ,(map (lambda (var expr)
                              ;;XX handle variable conflicts
                              `(,(corescheme-var.name var) ,(.scheme expr)))
                            vars exprs)
                      ,@(map .scheme
                             (if (corescheme-begin? body-expr)
                                 (corescheme-begin.body body-expr)
                                 (list body-expr)))))))

(def. corescheme-letrec.scheme (corescheme-<let>.scheme `letrec))
(def. corescheme-let.scheme (corescheme-<let>.scheme `let))
(def. corescheme-let*.scheme (corescheme-<let>.scheme `let*))

(def. corescheme-and.scheme (corescheme-<seq>.scheme `and))
(def. corescheme-or.scheme (corescheme-<seq>.scheme `or))



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

