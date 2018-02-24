
(require define-macro-star
	 test)

;; https://www.scheme.com/tspl3/control.html

;; (define-syntax fluid-let
;;   (syntax-rules ()
;;     ((_ ((x v)) e1 e2 ...)
;;      (let ((y v))
;;        (let ((swap (lambda () (let ((t x)) (set! x y) (set! y t)))))
;;          (dynamic-wind swap (lambda () e1 e2 ...) swap))))))


(define-macro* (fluid-let var+expr-s . body)
  (assert*
   list? var+expr-s
   (lambda (var+expr-s*)

     (let ((list-of-2? (lambda (v)
			 (and (list? v)
			      (= (length v) 2)))))
       (let ((.var (lambda (var+expr)
		     (assert* list-of-2? var+expr
			      (lambda (var+expr)
				(assert* symbol? (car var+expr) values)))))
	     (.expr (lambda (var+expr)
		      (assert* list-of-2? var+expr cadr))))

	 (let* ((var->kept_
		 (map (lambda (var+expr)
			(cons (.var var+expr) (gensym)))
		      var+expr-s*))
		(var->kept (lambda (var)
			     (assert* symbol? var
				      (lambda (var)
					(cdr (assq var var->kept_)))))))
	   `(let ,(map (lambda (var+expr)
			 `(,(var->kept (.var var+expr)) #f))
		       var+expr-s*)
	      (dynamic-wind (lambda ()
			      ,@(map (lambda (var+expr)
				       (define var (.var var+expr))
				       (define expr (.expr var+expr))
				       `(begin
					  (set! ,(var->kept var) ,var)
					  (set! ,var ,expr)))
				     var+expr-s*))
		  (lambda ()
		    ,@body)
		  (lambda ()
		    ,@(map (lambda (var+expr)
			     (define var (.var var+expr))
			     `(begin
				;; simply drop value of var (and recalculate
				;; from expr)?
				(set! ,var ,(var->kept var))))
			   var+expr-s*))))))))))


(TEST
 > (define TEST:equal? syntax-equal?)
 > (expansion#fluid-let ((a 1) (b 2)) a)
 (let ((GEN:-3831 #f) (GEN:-3832 #f))
   (dynamic-wind
       (lambda ()
	 (begin (set! GEN:-3831 a) (set! a 1))
	 (begin (set! GEN:-3832 b) (set! b 2)))
       (lambda () a)
       (lambda () (begin (set! a GEN:-3831)) (begin (set! b GEN:-3832))))))


(TEST
 > (let ((x 3))
     (+ (fluid-let ((x 5))
	  x)
	x))
 8
 ;; A fluid-bound variable also reverts to the old value if a
 ;; continuation created outside of the fluid-let is invoked.
 > (let ((x 'a))
     (let ((f (lambda () x)))
       (cons (call/cc
	      (lambda (k)
		(fluid-let ((x 'b))
		  (k (f)))))
	     (f))))
 (b . a)
 ;; If control has left a fluid-let body, either normally or by the
 ;; invocation of a continuation, and control reenters the body by the
 ;; invocation of a continuation, the temporary value of the
 ;; fluid-bound variable is reinstated. Furthermore, any changes to
 ;; the temporary value are maintained and reflected upon reentry.
 > (define reenter #f)
 > (define x 0)
 > (define _return values)
 ;; this wrapper is needed as otherwise we would rely on the function
 ;; position in the use of |return| being evaluated after its
 ;; argument:
 > (define (return x) (_return x))
 > (return
    (fluid-let ((x 1))
      (call/cc (lambda (k) (set! reenter k)))
      (set! x (+ x 1))
      x))
 2
 > x
 0
 > (define (rerun)
     (call/cc (lambda (c)
		(set! _return c)
		(reenter '*))))
 > (rerun)
 3
 > (rerun)
 4
 > x
 0)
