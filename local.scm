
(require define-macro-star)

(define-macro* (local var+expr-s . body)
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


;; (TEST
;;  > (define TEST:equal? syntax-equal?)
;;  > (expansion#local ((a 1) (b 2)) a)
;;  (let ((GEN:-3831 #f) (GEN:-3832 #f))
;;    (dynamic-wind
;;        (lambda ()
;; 	 (begin (set! GEN:-3831 a) (set! a 1))
;; 	 (begin (set! GEN:-3832 b) (set! b 2)))
;;        (lambda () a)
;;        (lambda () (begin (set! a GEN:-3831)) (begin (set! b GEN:-3832))))))
