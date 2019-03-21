;; Copyright 2016-2019 by Christian Jaeger <ch@christianjaeger.ch>

;;;    This file is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License (GPL) as published 
;;;    by the Free Software Foundation, either version 2 of the License, or
;;;    (at your option) any later version.


(require easy
	 corescheme
         (corescheme corescheme:flatten<T>)
	 (failing <failing-on> <failing-off>)
         (scheme-meta self-quoting?)
	 test
	 test-logic)

(export corescheme-to-scheme
        #!optional
        (method .scheme))


(TEST
 > (def roundtrip
        (=>* (source.corescheme globals: '(.>>= return * + - f))
             corescheme-to-scheme))

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



;; ------------------------------------------------------------------

;; Translate corescheme-core to corescheme-extended and make use of
;; the |let|, |let*|, |and| and |or| forms in the latter, to get
;; proper nice Scheme code back from |.scheme|.

(def (corescheme-to-scheme s #!optional (n 3));; XX n is a hack
     (let ((r (parameterize ((current-optimizing? #f)
                             (current-corescheme? corescheme-extended?))

                            (repeatedly n .corescheme-extended s))))
       ;; (assert (corescheme.optimized? r))
       ;;    -- XX adapt for here, too, change boolean to levels or bitmask?
       (.scheme r)))


(def.* (corescheme-literal.corescheme-extended s)
  s)

(def.* (corescheme-ref.corescheme-extended  s)
  s)

(def.* (corescheme-lambda.corescheme-extended s)
  (corescheme-lambda vars
                     (.corescheme-extended expr)))

(def.* (corescheme-app.corescheme-extended s)
  (let ((args* (map .corescheme-extended args)))
    (if (corescheme-lambda? proc)
        (begin
          (assert (lengths-= (.vars proc) args*))
          (corescheme-let (.vars proc)
                          args*
                          (.corescheme-extended (.expr proc))))
        (corescheme-app (.corescheme-extended proc) args*))))

(def.* (corescheme-def.corescheme-extended s)
  (corescheme-def var
                  (.corescheme-extended val)))

(def.* (corescheme-set!.corescheme-extended s)
  (corescheme-set! var
                   (.corescheme-extended val)))

(def.* (corescheme-begin.corescheme-extended s)
  (corescheme-begin (map .corescheme-extended body)))


(def (corescheme:eq? cs schemeval)
     (and (corescheme-literal? cs)
          (eq? (@corescheme-literal.val cs) schemeval)))

(def.* (corescheme-if.corescheme-extended s)
  (let ((test* (.corescheme-extended test))
        (then* (.corescheme-extended then))
        (else* (and else (.corescheme-extended else))))
    (cond
     ;; detect |and|
     ((corescheme:eq? else* #f)
      (corescheme-and (list test* then*)))
     ;; |or| must be handled in |let| -- or hmm ?
     (else ;; fallback
      (corescheme-if test* then* else*)))))

(def.* (corescheme-letrec.corescheme-extended s)
  (corescheme-letrec vars
                     (map .corescheme-extended exprs)
                     (.corescheme-extended body-expr)))

(def.* (corescheme-let.corescheme-extended s)
  (if (and (corescheme-let? body-expr)
           (length-= vars 1)
           (length-= (.vars body-expr) 1))
      (corescheme-let* (append vars (.vars body-expr))
                       (map .corescheme-extended
                            (append exprs (.exprs body-expr)))
                       (.corescheme-extended (.body-expr body-expr)))
      s))

(def.* (corescheme-let*.corescheme-extended s)
  (corescheme-let* vars
                   (map .corescheme-extended exprs)
                   (.corescheme-extended body-expr)))

(def.* (corescheme-and.corescheme-extended s)
  (corescheme-and (corescheme:flatten<T> corescheme-and?
                                         (map .corescheme-extended body))))

(def.* (corescheme-or.corescheme-extended s)
  (corescheme-or (corescheme:flatten<T> corescheme-or?
                                        (map .corescheme-extended body))))

;; ------------------------------------------------------------------

;; Tests:


;; change to <failing-on> for debugging
(modimport/prefix failing: <failing-off>)

(TEST
 > (def (corescheme-back source prettify?)
	((if prettify? corescheme-to-scheme .scheme)
	 (source.corescheme source get-ctx: default-scheme-env)))
 > (def t-scheme-code
	(lambda (source result prettyresult)
	  (and
	   ;; are the given code fragments even evaluating to the same
	   ;; value?
	   (failing:equal? (eval source)
			   (eval result))
	   ;; and does the compiler actually give the given result?
	   (failing:source-equal? (corescheme-back source #f)
				  result)
           (failing:source-equal? (corescheme-back source #t)
				  prettyresult))))
 > (def failures
	(qcheck
	 (source-code
	  (quote-source
	   ( ;; source, .scheme, corescheme-to-scheme
	    ((let* ((x 4) (y x)) (begin 2 x))
             ((lambda (x) ((lambda (y) 2 x) x)) 4)
             (let* ((x 4) (y x)) 2 x))

	    ((let ((x 4) (y 5)) (begin 2 x))
             ((lambda (x y) 2 x) 4 5)
             (let ((x 4) (y 5)) 2 x))

	    ((let ((x 4) (y 5)) 2 x)
             ((lambda (x y) 2 x) 4 5)
             (let ((x 4) (y 5)) 2 x))

	    ((let ((x '()))
	       x)
             ((lambda (x) x) '())
             (let ((x '()))
	       x))

	    ((define (corescheme-to-scheme:square n)
	       (* n n))
             (define corescheme-to-scheme:square (lambda (n) (* n n)))
             (define corescheme-to-scheme:square (lambda (n) (* n n))))

	    ((define (corescheme-to-scheme:fact n)
	       (if (zero? n)
		   1
		   (* n (corescheme-to-scheme:fact (- n 1)))))
             (define corescheme-to-scheme:fact
	       (lambda (n)
		 (if (zero? n)
		     1
		     (* n (corescheme-to-scheme:fact (- n 1))))))
             ;; XX pretty-print that in .scheme, not
             ;; .corescheme-extended ?
             (define corescheme-to-scheme:fact
	       (lambda (n)
		 (if (zero? n)
		     1
		     (* n (corescheme-to-scheme:fact (- n 1)))))))
	    
	    ((begin (define a 1) (define b a))
             (begin (define a 1) (define b a))
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
		       (corescheme-to-scheme:odd? (- n 1))))))
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
             (define corescheme-to-scheme:foo
               (lambda (fn) ((lambda (x) (fn x)) 10)))
             (define corescheme-to-scheme:foo
               (lambda (fn) (let ((x 10)) (fn x))))))))
	 (comp (applying t-scheme-code) source-code)))
 > failures
 ())

