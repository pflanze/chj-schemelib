;; Copyright 2016 by Christian Jaeger <ch@christianjaeger.ch>

(require easy
	 corescheme
	 (failing <failing-on> <failing-off>))


(def. (cs-literal.scheme-code v)
  (let-cs-literal
   ((val) v)
   (cond (((either symbol? keyword? null?)
	   (source-code val))
	  `(quote ,val))
	 (else
	  val))))

(def. (cs-lambda.scheme-code v)
  (let-cs-lambda ((vars expr) v)
		 `(lambda ,(map .name vars)
		    ,@(map .scheme-code
			   (if (cs-begin? expr)
			       (.body expr)
			       (list expr))))))

(def. (cs-app.scheme-code v)
  (let-cs-app ((proc args) v)
	      `(,(.scheme-code proc)
		,@(map .scheme-code args))))

(def. cs-ref.scheme-code (comp .name .var))

(def. (cs-def.scheme-code v)
  (let-cs-def ((var val) v)
	      `(define ,(.name var)
		 ,(.scheme-code val))))

(def. (cs-begin.scheme-code v)
  `(begin ,@(map .scheme-code (.body v))))

(def. (cs-if.scheme-code v)
  (let-cs-if ((test then else) v)
	     `(if ,(.scheme-code test)
		  ,(.scheme-code then)
		  ,@(if else
			(list (.scheme-code else))
			'()))))

(def. (cs-set!.scheme-code v)
  `(set! ,(.name var)
	 ,(.scheme-code val)))

;; cs-letrec


;; change to <failing-on> for debugging
(modimport/prefix failing: <failing-off>)

(TEST
 > (def (cs-back source)
	(.scheme-code
	 (source.cs-ast source default-scheme-env)))
 > (def t-scheme-code
	(lambda (source result)
	  (and
	   ;; are the given code fragments even evaluating to the same
	   ;; value?
	   (failing:equal? (eval source)
			   (eval result))
	   ;; and does the compiler actually give the given result?
	   (failing:source-equal? (cs-back source)
				  result))))
 > (def t-scheme-code*
	(lambda (v)
	  (let-pair ((a b) (source-code v))
		    (t-scheme-code a b))))
 > (def failures
	(qcheck
	 (source-code
	  (quote-source
	   (;; source and expected result from cs-back in pairs
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

	    ((define (square n)
	       (* n n))
	     .
	     (define square (lambda (n) (* n n))))

	    ((define (fact n)
	       (if (zero? n)
		   1
		   (* n (fact (- n 1)))))
	     .
	     (define fact
	       (lambda (n)
		 (if (zero? n)
		     1
		     (* n (fact (- n 1)))))))
	    
	    ((begin (define a 1) (define b a))
	     .
	     (begin (define a 1) (define b a)))

	    ((begin
	       (define (odd? n)
	    	 (if (zero? n)
	    	     #f
	    	     (even? (- n 1))))
	       (define (even? n)
	    	 (if (zero? n)
	    	     #t
	    	     (odd? (- n 1)))))
	     .
	     (begin
	       (define odd?
		 (lambda (n)
		   (if (zero? n)
		       #f
		       (even? (- n 1)))))
	       (define even?
		 (lambda (n)
		   (if (zero? n)
		       #t
		       (odd? (- n 1)))))))
	    )))
	 t-scheme-code*))
 > failures
 ())

